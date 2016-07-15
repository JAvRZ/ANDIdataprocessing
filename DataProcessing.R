###############
library(foreign)
library(lme4)
library(MASS)


###############
setwd("../15-7-2016/") # select a directory
testvarinfo <- read.csv( "https://raw.githubusercontent.com/andi-nl/ANDI-frontend/master/static/app/data/test_variable_info.csv",
                         sep = ";", header = TRUE, stringsAsFactors = F) 
# reads in a file that has some extra info on the tests, namely
# the variable name as used in the SPSS file for all variables to be included in the eventual dataset ("spss.name")
# the variable name in a prettier format, for writing to LateX ("newname")
# the lower bound per variable, under which scores are deemed unrealistic to be coming from a healthy participant ("lower")
# the upper bound per variable, above which scores are deemed unrealistic to be coming from a healthy participant ("higher")
# the upper bound ends in .001, as this is added to some variables later on
# the test name to which the variable belongs, which equals the name used in the spss-filename ("test"), e.g. AVLT
namesmat <- cbind( testvarinfo$long.name.1, testvarinfo$long.name.2, testvarinfo$long.name.3)
namesmat[is.na(namesmat)] <- ""
testvarinfo$short.name <- trimws(paste(namesmat[,1], namesmat[,2], namesmat[,3]))

# This is a compromise between long and short names for the y-label in the raw data plots
idnamesmat <- cbind( testvarinfo$ID1, testvarinfo$ID2, testvarinfo$ID3)
idnamesmat[is.na(idnamesmat)] <- ""
testvarinfo$testID.name <- trimws(paste(idnamesmat[,1], namesmat[,2], namesmat[,3]))

testvarinfo$shortest.name <- trimws(paste(idnamesmat[,1], idnamesmat[,2], idnamesmat[,3]))

metadataforMMNCandpatient <- data.frame( formulapart = as.character(), added = as.numeric(), mybestpowertransform = as.numeric(), printbestformula = as.character())
# A dataframe is initialized in which results can be saved for use in later computations
# these are the parts of the formula that were used in the selected model,
# whether something was added to scores to make scores non-negative or non-zero
# the power to which scores were raised in the Box-Cox transformation

mytestnames <- gsub("+Demo.sav", "", list.files()[grepl("+Demo.sav", list.files())], fixed = T)
# The test names are retrieved from the filenames, by removing the "+Demo.sav" part
# (Does require that all files are named +Demo.sav of course)

mytestnames <- mytestnames[ mytestnames %in% testvarinfo$dataset]
mytestnames <- unique( testvarinfo$dataset[ testvarinfo$dataset %in% mytestnames])
# Matches 

numberofcontributingstudies <- matrix( NA, length(mytestnames), 2 )
for( j in mytestnames){
  print(j)
  # For each of the different tests (So AVLT, RBMT, etc.)
  mydata <- as.data.frame(read.spss(paste0(j, "+Demo.sav"), use.value.labels = FALSE)) # select a sav
  
  # The corresponding SPSS file is read
  mytestname <- j
  # Perhaps a bit superfluous, but this mytestname will thus become AVLT, RBMT, etc.
  
  mydata[,-1][ mydata[,-1] == 999] <- NA # all 999s become NA
  mydata[,-1][ mydata[,-1] == 9999] <- NA # all 9999s become NA
  mydata$Sex <- mydata$Sexe - 1 # Sexe was a factor with 2 levels, 1 and 2, 1=male, now becomes 0(=male) and 1
  
  mydata$age <- mydata$age - 65 # Age is "centered" on 65, so a value of 0 now equals 65, a value of -1 equals 64 and so on
  mydata$agesquared <- mydata$age * mydata$age # a new variable Age to the power 2 is made by squaring age
  
  mydata <- subset(mydata, !is.na(age) & !is.na(Sex) & !is.na(edu_Ver))
  myvarnames <- colnames(mydata) # saves the names of the columns of the dataset
  deselectedvars <- c("SRT_MCR", "BADS_RegelWissel_cond1score", "BADS_Zoo2", "RAND_emoprob", "RAND_physicprob", "bourdon.fouten.totaal")
  selectedvars <- which(myvarnames %in% testvarinfo['spss.name'][,1][testvarinfo['dataset'] == j] & !(myvarnames %in% deselectedvars))
  # The variables that occur both in the SPSS file and in the prettyvarnames CSV are selected
  # So because AVLT1, AVLT2 etc are currently not in the CSV, they are not selected.
  # The total score for AVLT 1 to 5 is included in both the SPSS file and in the CSV, so it is selected
  # BADS_Zoo2 led to fatal errors due to ceiling effects coupled with modest sample sizes, so was added to the explicitly deselected variables
  
  for( i in selectedvars){ # this procedure is done for each of the selected variables (e.g. AVLTtotal1-5, AVLTdelayed1-5, etc.)
    myspecificvar <- myvarnames[i]
    myprettynamespecificvar <- testvarinfo['short.name'][,1][ testvarinfo['spss.name'][,1] == myspecificvar]
    myIDnamespecificvar <- testvarinfo['testID.name'][,1][ testvarinfo['spss.name'][,1] == myspecificvar]
    myshortestnamespecificvar <- testvarinfo['shortest.name'][,1][ testvarinfo['spss.name'][,1] == myspecificvar]
    myIDspecificvar <- testvarinfo['ID'][,1][ testvarinfo['spss.name'][,1] == myspecificvar]
    print(myspecificvar)
    
    mydatasubset <- subset(mydata, select = c(ID, study, eval(parse(text = myspecificvar)), age, agesquared, edu_Ver, Sex))
    # a subset of the data is selected, with just the variable under investigation, and the demographics
    mydatasubset <- mydatasubset[!is.na(mydatasubset[[myspecificvar]]),]
    # all rows with NA's are removed (these can be either in the demographics or the variable)
    
    multilevel <- length(unique(mydatasubset$study)) > 1
    # The object multilevel is a logical, indicating that for the specific variable under investigation, there is more than one study
    # For example, delayed1-3 might occur in just 1 study, and multilevel obtains the value FALSE
    # This means that normal unilevel regression models will be fitted instead of multilevel regression models
    
    # Extreme borders are retrieved here.
    upperbound <- testvarinfo['highborder'][,1][testvarinfo['spss.name'][,1]==myspecificvar]
    lowerbound <- testvarinfo['lowborder'][,1][testvarinfo['spss.name'][,1]==myspecificvar]
    extremeborderoutlierindices <- c(which(mydatasubset[[myspecificvar]] > upperbound),
                                     which(mydatasubset[[myspecificvar]] < lowerbound))
    if( length(extremeborderoutlierindices > 0 )){ # if there are observations outside of the extreme borders
      mycleandatasubset <- mydatasubset[-extremeborderoutlierindices,] # a clean version without the observations outside the extreme borders is made
    } else { mycleandatasubset <- mydatasubset }  # if there are no extreme border observations, the clean data is equal to the old data
    
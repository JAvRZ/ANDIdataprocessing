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
    
    
    # Box-Cox does not allow any zeroes or negative values. This statement checks for that and takes action.
    if( any(mycleandatasubset[[myspecificvar]] < 0)){
      anynegatives <- TRUE # saves this check for the metadata
      anyzero <- TRUE # saves this check for the metadata
      lowestvalue <- min(mycleandatasubset[[myspecificvar]]) # lowest negative value is found
      added <- abs(lowestvalue) + 0.001 # the amount added is saved, so it can be added to a patient's score as well in the beta
      mycleandatasubset[[myspecificvar]] <- mycleandatasubset[[myspecificvar]] + added  # all values are made positive
      # if the lowest value was -5601, 5601.001 is added to it, so the lowest value is now 0.001
    } else if (any(mycleandatasubset[[myspecificvar]] == 0)) {
      anynegatives <- FALSE
      anyzero <- TRUE
      added <- 0.001 # the amount added is saved, so it can be added to a patient's score as well in the beta
      mycleandatasubset[[myspecificvar]] <- mycleandatasubset[[myspecificvar]] + added #all values are made positive
      # if there is a zero, this is now 0.001 (but what was 7 is now also 7.001)
    } else {
      anynegatives <- FALSE # saves for metadata that raw scores were left unchanged
      anyzero <- FALSE
      added <- 0
    }
    
    # This section checks for adequate amounts of data for demographic variables
    # First, the demographic variables are temporarily split into categories
    mygendercategories <- factor(mycleandatasubset$Sex, levels = c(0:1))
    myagecategories <- cut( mycleandatasubset$age + 65 , breaks = c( 0, 55, 75, 120 ), include.lowest = TRUE)
    myeducategories <- factor( mycleandatasubset$edu_Ver, levels = c(1:7))
    
    # Frequencies of the different levels of the variables are tabled
    mygendertable <- table(mygendercategories)
    myagetable <- table(myagecategories)
    myedutable <- table(myeducategories)
    
    # A model is initialized to which terms can be added if there is enough data
    growingmodel <- c()
    if(min(mygendertable) >= 5){ growingmodel <- c( growingmodel, "Sex" )}
    if(median(myagetable) >= 5){ growingmodel <- c( growingmodel, "age")}
    if(median(myedutable) >= 5){ growingmodel <- c( growingmodel, "edu_Ver")}
    
    # If there is no data available to estimate any of the demographic effects, an intercept is added explicitly,
    # as an empty model cannot be fitted.
    if( length( growingmodel ) == 0 ){ growingmodel <- c("1") }
    
    # The fixed part of the model as understood by the lmer-function or the lm-function is formulated
    fullmodel <- paste0( myspecificvar, " ~ ", paste0( growingmodel, collapse = " + "))
    
    # If there is data from multiple studies available, a random intercept is added to the function
    if( multilevel ){ fullmodel <- paste0( fullmodel, " + (1 | study)") }
    
    if( multilevel){ # multilevel version using lmer
      mymodel <- lmer( as.formula(fullmodel), data = mycleandatasubset, REML = FALSE)
      # the full model is fit to the data
    } else { # single level version using lm
      mymodel <- lm( as.formula(fullmodel), data = mycleandatasubset)
      # the full model is fit to the data
    }
    
    
    
    
    # This part searches for the best possible model
    mytempmodel <- mymodel # the model to iterate over is initialized
    
    mydisplayformula <- as.character(as.formula(update(mymodel, "~ 1 + .")))[3]
    if( multilevel){
      mydisplayformula <- sub(" + (1 | study)", "", mydisplayformula, fixed = TRUE)
    }
    
    initial <- TRUE
    firstdrop <- TRUE
    myminAIC <- 0; myoldAIC <- 1 # values for the information criterion are initialized
    
    while( myminAIC < myoldAIC){ # while the lowest found information criterium is lower than what we had with the old model...
      mytemptable <- drop1( mytempmodel, k = 2) # investigate all models with 1 term dropped
      if( multilevel ){ # if it is a multilevel model
        myoldAIC <- mytemptable[,2][1] # the old AIC is saved
        myminAIC <- min(mytemptable[,2], na.rm = T) # the lowest AIC is saved
        mywhichmin <- which(mytemptable[,2] == myminAIC) # the location of the lowest AIC is saved
      } else { # if it is not a multilevel model
        myoldAIC <- mytemptable[,4][1] # the old AIC is saved
        myminAIC <- min(mytemptable[,4], na.rm = T) # # the lowest AIC is saved
        mywhichmin <- which(mytemptable[,4] == myminAIC) # the location of the lowest AIC is saved
        
      }
      initial <- FALSE
      
      if( myminAIC < myoldAIC ){ # if the lowest AIC is lower than the old AIC
        mytempmodel <- update( mytempmodel, paste0(". ~ . - ", rownames(mytemptable)[mywhichmin])) # the model is updated,
        firstdrop <- FALSE
        #with the term that resulted in the lowest AIC removed from the model
      }
      
      
    }
    
    if( !firstdrop){
      mydisplayfinalformula <- as.character(as.formula(mytempmodel))[3]
    } else { mydisplayfinalformula <- mydisplayformula }
    if( multilevel){
      mydisplayfinalformula <- sub(" + (1 | study)", "", mydisplayfinalformula, fixed = TRUE)
      mydisplayfinalformula <- sub("(1 | study)", "", mydisplayfinalformula, fixed = TRUE)
    }
    mydisplayfinalformula <- sub("1", "", mydisplayfinalformula, fixed = TRUE)
    if( mydisplayfinalformula == "") { mydisplayfinalformula <- "None"}
    
    
    
    finalmodel <- as.formula(mytempmodel)
    # The final model is saved (perhaps superfluously)
    
    if( multilevel){ # if it is multilevel
      myfinalmodel <- lmer( finalmodel, data = mycleandatasubset)
      # the final model is fit to the data (slightly redundant, but results in a nice model object)
    } else { # if it is unilevel
      myfinalmodel <- lm( finalmodel, data = mycleandatasubset)
      # the final model is fit to the data (slightly redundant, but results in a nice model object)
    }
    
    # The median absolute deviation from the median on the residuals is determined, and multiplied by 3.5
    residualoutliercutoffmad <- 3.5*mad(resid(myfinalmodel))
    # the upper and lower bounds for residuals are determined
    residualoutlierupperbound <- median(resid(myfinalmodel)) + residualoutliercutoffmad
    residualoutlierlowerbound <- median(resid(myfinalmodel)) - residualoutliercutoffmad
    # the rows of the residual outlier participants are saved
    regressionoutlierindices <- which(resid(myfinalmodel) < residualoutlierlowerbound | resid(myfinalmodel) > residualoutlierupperbound)
    
    if( length(regressionoutlierindices > 0)) { # if there are residual outlier observations
      mycleanerdatasubset <- mycleandatasubset[-regressionoutlierindices,] # a cleaner version without the residual outliers observations is made
    } else { mycleanerdatasubset <- mycleandatasubset } # if there are no residual outliers observations, the cleaner data is equal to the clean data
    
    # The fixed (non-random) part of the final model is reconstructed
    myfixedformula <- as.character(formula(finalmodel))
    if( multilevel){
      myfixedformula <- sub(" + (1 | study)", "", myfixedformula, fixed = TRUE)
    }
    myfixedformula <- as.formula( paste0(myfixedformula[2], myfixedformula[1], myfixedformula[3]))
    # A model is fitted with just the fixed part, even if the data is multilevel. This model is constructed as it will be used in the Box-Cox transformation
    myfixedmodel <- lm( myfixedformula, data = mycleanerdatasubset)
    
    # The optimal powertransformation is sought using the Box-Cox method
    if( multilevel){ # If the data is multilevel, a fixed study indicator variable is added (as.factor(study))
      myboxcox <- with( mycleanerdatasubset, boxcox(  as.formula( paste0(deparse(myfixedformula), " + as.factor(study)")), seq(-5, 12, 1/100), plotit = FALSE))
    } else {
      myboxcox <- with( mycleanerdatasubset, boxcox( myfixedmodel, seq(-5, 12, 1/1000), plotit = FALSE))
    }
    mybestpowertransform <- myboxcox$x[which.max(myboxcox$y)]
    
    # If the window for possible power transformations is too narrow (the estimate hits the upper or lower bound), a wider window is used
    if( mybestpowertransform == 12 | mybestpowertransform == -5 ){
      myboxcox <- with( mycleanerdatasubset, boxcox( myfixedmodel, seq(-30, 50, 1/1000), plotit = FALSE))
      mybestpowertransform <- myboxcox$x[which.max(myboxcox$y)]
    }
    
    # A new transformed version of the variable score is added to the dataframe
    mycleanerdatasubset[[noquote(paste0("transf.",myIDspecificvar))]] <- mycleanerdatasubset[[myspecificvar]]^mybestpowertransform
    if( mybestpowertransform < 0 ){ # If the power transformation is negative, this has the by-effect of reversing the order of scores. Here, the order is switched back.
      mycleanerdatasubset[[noquote(paste0("transf.",myIDspecificvar))]] <- -1*mycleanerdatasubset[[noquote(paste0("transf.",myIDspecificvar))]]
    }
    
    
    # In this step, scores are standardized (the standardizing values are explicitly defined, as they are also needed for later analyses)
    mymean.transformedscores <- mean( mycleanerdatasubset[[noquote(paste0("transf.",myIDspecificvar))]])
    mysd.transformedscores <- sd( mycleanerdatasubset[[noquote(paste0("transf.",myIDspecificvar))]])
    mycleanerdatasubset[[noquote(paste0("transf.",myIDspecificvar,".standardized"))]] <- ( mycleanerdatasubset[[noquote(paste0("transf.",myIDspecificvar))]] - mymean.transformedscores ) / mysd.transformedscores
    
    
    
    # A formula object is constructed with both fixed and random terms of the final model
    # found earlier with the step function.
    mybestformula <- as.character(formula(finalmodel))
    mybestformula <- sub(myspecificvar, paste0("transf.",myIDspecificvar,".standardized"), mybestformula, fixed = TRUE)
    mybestformula <- as.formula( paste0(mybestformula[2], mybestformula[1], mybestformula[3]))
    
    if( multilevel){
      # The model is fitted to the transformed data to get parameter estimates on this new scale
      mycleantransffinalmodel <- lmer( mybestformula, data = mycleanerdatasubset)
    } else {
      mycleantransffinalmodel <- lm( mybestformula, data = mycleanerdatasubset)
    }
    
    printbestformula <- as.character(myfixedformula)[3]
    printbestformula <- gsub( "agesquared", "a$^2$", printbestformula)
    printbestformula <- gsub( "age", "a", printbestformula)
    printbestformula <- gsub( "Sex", "s", printbestformula)
    printbestformula <- gsub( "edu_Ver", "e", printbestformula)
    printbestformula <- gsub( ":", "*", printbestformula)
    
    # At the first of the variables belonging to a certain test, a mycombineddataframes object is generated
    if( i == selectedvars[1]){
      mycombineddataframes <- mycleanerdatasubset
    } else { # If there are multiple variables, the extra variables are merged with the variables from the earlier instances of the loop
      mycombineddataframes <- merge( mycombineddataframes,
                                     mycleanerdatasubset, by = c("ID","Sex","edu_Ver","age", "agesquared", "study"), all = TRUE)
    }
    
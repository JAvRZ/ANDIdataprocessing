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

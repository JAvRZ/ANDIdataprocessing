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

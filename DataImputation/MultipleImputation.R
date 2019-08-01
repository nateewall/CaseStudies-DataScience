# Implementing Code from: 
# https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
# 

# necessary Libraries
library(mice)
library(VIM)
library(dplyr)

# Getting the data
filename = "carmpg.txt"
data = read.csv(file=filename, header=TRUE, sep="\t")

# Understanding what's missing
summary(data)

pMiss <- function(x){sum(is.na(x))/length(x)*100} #Calucations percent of an axis that is NA

apply(data,2,pMiss) # Percentages of missing values by column
apply(data,1,pMiss) # Percentages of missing values by record

md.pattern(data)

aggr_plot = aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Need to add a step in here to convert CYLINDERS and ENG_TYPE to factors

data = data %>%
  mutate(
    CYLINDERS = as.factor(CYLINDERS),
    ENG_TYPE = as.factor(ENG_TYPE)
  )

# Getting into imputing the data
tempData = mice(data,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

#shows the values that were imputed across the five different iterations 
tempData$imp$CYLINDERS 

# Gets the full imputed dataset
completedData = complete(tempData,1) 

# Plots to see how well our imputation lines up with the overall distribution
xyplot(tempData,MPG~CYLINDERS+SIZE+HP+WEIGHT+ACCEL+ENG_TYPE,pch=18,cex=1) # This one isn't working to show different colors for the imputed values vs. the known values
densityplot(tempData) 
stripplot(tempData, pch = 20, cex = 1.2)

# pooling the results
modelFit1 = with(tempData,lm(MPG~CYLINDERS+SIZE+HP+WEIGHT+ACCEL+ENG_TYPE))
summary(pool(modelFit1))
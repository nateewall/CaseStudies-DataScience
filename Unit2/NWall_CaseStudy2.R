library(ggplot2)
library(RANN)

## Read the data into a dataframe
txt = readLines("http://rdatasciencecases.org/Data/offline.final.trace.txt")

#function for processing the data
processLine = function(x){
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp), ncol = 6, byrow = TRUE), tmp)
}

# set angles to proper increments
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}


readData = function(filename){
  # load data, drop all comment rows (beginning with '#')
  txt = readLines(filename) 
  lines = txt[ substr(txt, 1, 1) != "#" ] # remove lines that start with #, comments
  
  # process each line via process line function, combine all matrices that are returned
  tmp = lapply(lines, processLine)
  output = as.data.frame(do.call("rbind", tmp), stringsAsFactors = FALSE)
  
  # set column names and numeric data types
  names(output) = c("time", "scanMac", "posX", "posY", "posZ", "orientation", "mac", "signal", "channel", "type")
  numVars = c("time", "posX", "posY", "posZ", "orientation", "signal")
  output[ numVars ] =  lapply(output[ numVars ], as.numeric)
  
  # remove rows that aren't type == 3, drop the type column
  output = output[ output$type == "3", ]
  output = output[ , "type" != names(output) ]
  
  # copy raw time, convert time from ms, set proper time data types
  output$rawTime = output$time
  output$time = output$time/1000
  class(output$time) = c("POSIXt", "POSIXct")
  
  # convert character variables to factors
  sapply(output[ , c("mac", "channel", "scanMac")], as.factor)
  
  # drop scanmac and posZ
  output = output[ , !(names(output) %in% c("scanMac", "posZ"))]
  
  # set angles to proper rounded increments
  output$angle = roundOrientation(output$orientation)
  
  # drop all rows that aren't in the top 7 MACs by count...(this is the subMacs component from the book)
  subMacs = names(sort(table(output$mac), decreasing = TRUE))[1:7]
  output = output[ output$mac %in% subMacs, ]
  
  return(output)
  
}

offline = readData("http://rdatasciencecases.org/Data/offline.final.trace.txt")

# ##lets look at the head of the data
# tail(offline)
# str(offline)
# 
# boxplot(signal~mac,data=offline, main="Mac Signal Measurements",
#         xlab="MAC Address", ylab="Signal") 
# 
# 
# boxplot(signal~angle,data=singleMAC, main="Angle Signal Measurements",
#         xlab="Angles", ylab="Signal") 

# ##before moving on lets do some explorations
# 
# ##boxplot of orientation & angle##
# with(offline, boxplot(orientation ~ angle,
#                       xlab = "nearest 45 degree angle",
#                       ylab="orientation"))
# 
# 



# code to create offline summary
processOfflineSummary = function(offline){
  offline$posXY = paste(offline$posX, offline$posY, sep = "-")
  byLocAngleAP = with(offline,
                      by(offline, list(posXY, angle, mac), function(x) x))
  signalSummary =
    lapply(byLocAngleAP,
           function(oneLoc) {
             ans = oneLoc[1, ]
             ans$medSignal = median(oneLoc$signal)
             ans$avgSignal = mean(oneLoc$signal)
             ans$num = length(oneLoc$signal)
             ans$sdSignal = sd(oneLoc$signal)
             ans$iqrSignal = IQR(oneLoc$signal)
             ans
           })
  return(do.call("rbind", signalSummary))
}

offlineSummary = processOfflineSummary(offline)

# head(offlineSummary)
# str(offlineSummary)
# 
# tst = reshapeSS(offlineSummary, varSignal = "avgSignal")
# 
# str(tst)

online = readData("http://www.rdatasciencecases.org/Data/online.final.trace.txt")

# head(online)
# str(online)

# code to create online summary
processOnlineSummary = function(online){
  online$posXY = paste(online$posX, online$posY, sep="-") # create new feature combining posX with posY for each row
  online$posXY <- factor(online$posXY) #let's make posXY a factor THIS IS WHAT WAS MISSING IN THE BOOK!
  
  keepVars = c("posXY", "posX","posY", "orientation", "angle")
  byLoc = with(online, # with applies an expression to the dataset "with(data, expression)", in this case applies "by"
               by(online, list(posXY),  # by applies a function to each level of a factor "by(data, factorlist, function)"
                  function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x$signal, x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 7, dimnames = list(ans$posXY, names(avgSS)))  # THIS NEEDS TO BE 7 COL, NOT 6 LIKE THE BOOK..BECAUSE WE HAVEN'T DROPPED EXTRA MAC YET
                    cbind(ans, y)
                  }))
  
  return(onlineSummary = do.call("rbind", byLoc))
}

onlineSummary = processOnlineSummary(online)

# head(onlineSummary)
# str(onlineSummary)
# 
# boxplot(orientation~angle,data=onlineSummary, main="Mac Signal Measurements",
#         xlab="MAC Address", ylab="Signal") 

reshapeSS = function(data, varSignal = "signal", keepVars = c("posXY", "posX","posY")) {
  data$posXY = factor(data$posXY) ## added this here as well, since it needs to be a factor for the by function below
  byLocation =
    with(data, by(data, list(posXY),
                  function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 7, dimnames = list(ans$posXY, names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}


selectTrain = function(angleNewObs, signals = NULL, m = 1){   # m is the number of angles to keep between 1 and 5
  
  refs = seq(0, by = 45, length  = 8)
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1)
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1)
      angles = angles[ -1 ]
    else
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles)
  offlineSubset = signals[ signals$angle %in% angles, ]
  return(reshapeSS(offlineSubset, varSignal = "avgSignal"))
}

## portion of code for testing distance function DELETE#
# #subset a group from training
# train90 = selectTrain(90, offlineSummary, m = 3)
# test = onlineSummary[2 , 6:12]
# test
# train <- train90[,4:10]
# head(train)
# 
# weights = scale <- function(x){1-(x-min(x))/(max(x)-min(x))}
# 
# diffs = apply(train90[,4:10], 1,function(x) x - test)
# diffs = 
# 
# for (c in 1:ncol(train90[,4:10])){
#   for (r in 1:nrow(train90))
#   
# }
# 
# dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )

findNN = function(newSignal, trainSubset, k = 3) {
  knn <- nn2(trainSubset[,4:10], newSignal, k = k)
  nn <- trainSubset[knn$nn.idx , ]
  nn$dist <- (array(knn$nn.dists))
  nn$weightX <- nn$posX*((1/nn$dist)/sum((1/nn$dist)))
  nn$weightY <- nn$posY*((1/nn$dist)/sum((1/nn$dist)))
  return(nn)
}

findWeightedNN = function(newSignal, trainSubset, k = 3) {
  knn <- nn2(trainSubset[,4:10], newSignal, k = k)
  nn <- trainSubset[knn$nn.idx , ]
  nn$dist <- (array(knn$nn.dists))
  nn$weightX <- nn$posX*((1/nn$dist)/sum((1/nn$dist)))
  nn$weightY <- nn$posY*((1/nn$dist)/sum((1/nn$dist)))
  return(nn)
}


predXY = function(newSignals, newAngles, trainData, numAngles = 1, k = 3){
  closeXY = list(length = nrow(newSignals))
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    tmp = findNN(newSignal = newSignals[i, ], trainSS, k=k)
    estXY = colMeans(tmp[sapply(tmp[1:3], is.numeric)])[,c('')]
    closeXY[[i]] = estXY
  }
  return(do.call(rbind, closeXY))
}


weightedPredXY = function(newSignals, newAngles, trainData, numAngles = 1, k = 3){
  closeXY = list(length = nrow(newSignals))
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    tmp = findNN(newSignal = newSignals[i, ], trainSS, k=k)
    estXY = colSums(tmp[,c('weightX','weightY')])
    names(estXY) = c('posX','posY')
    closeXY[[i]] = estXY
  }
  return(do.call(rbind, closeXY))
}

plotDistance = function(compare){
  p <- ggplot(compare, aes(x=posX, y=posY)) +
    geom_point(aes(colour = 'Actual' )) + 
    geom_point(aes(x=estX, y=estY, colour='Estimated')) +
    geom_segment(data = compare, 
                 aes(x = posX, xend = estX, 
                     y = posY, yend = estY), alpha = 0.25) +
    labs(title = 'Distance between Actual vs Predicted', x= 'Position X', y= 'Position Y', colour = "Groups") +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))
  return(p)
}


kTesting = function(newSignals, newAngles, trainData, numAngles = 3, range = 3){
  error = list(range)
  for (i in 1:range){
    estXY = predXY(newSignals = onlineSummary[ , 6:12],newAngles = onlineSummary[ , 4] 
                   ,offlineSummary, numAngles = 3, k = i)
    
    weightedEstXY = weightedPredXY(newSignals = onlineSummary[ , 6:12],newAngles = onlineSummary[ , 4] 
                                    ,offlineSummary, numAngles = 3, k = i) 
    
    calcError =function(estXY, actualXY) sum(rowSums((estXY - actualXY)^2))
    actualXY = onlineSummary[ , c("posX", "posY")]
    error[[i]] = sapply(list(estXY, weightedEstXY), calcError, actualXY)
    print(error[[i]])
    #also plot the weighted distance plots
    actualXY$estX <- weightedEstXY[,1]
    actualXY$estY <- weightedEstXY[,2]
    
    pdf(paste0("Distance_Plot_K", i, ".pdf"))
    plotDistance(actualXY)
    dev.off()
  }
  return(error)
}

error = kTesting(onlineSummary[ , 6:12], newAngles = onlineSummary[ , 4] ,offlineSummary, numAngles = 3, range = 10)
errorDF = data.frame(do.call(rbind, error))
errorDF$k <- as.numeric(row.names(errorDF))

pdf(paste0("ErrorPlotsAccrossRange.pdf"))
ggplot(errorDF, aes(k)) + 
  geom_line(aes(y = X1, colour = "Raw Average")) + 
  geom_line(aes(y = X2, colour = "Weighted Average")) +
  labs(title = 'Raw Average vs Weighted Average', x= 'K Nearest Neighbors', y= 'Error', colour = "Metric") +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
dev.off()


estXYk = predXY(newSignals = onlineSummary[ , 6:12],newAngles = onlineSummary[ , 4] 
                 ,offlineSummary, numAngles = 3, k = 1)

weightedEstXY = weightedPredXY(newSignals = onlineSummary[ , 6:12],newAngles = onlineSummary[ , 4] 
                 ,offlineSummary, numAngles = 3, k = 1)

actualXY$estX <- weightedEstXY[,1]
actualXY$estY <- weightedEstXY[,2]
pdf(paste0("Distance_Plot_Weighted_k1.pdf"))
plotDistance(actualXY)
dev.off()



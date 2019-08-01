library(tm)
#RSpamData is no longer available
#download data from http://www.stat.berkeley.edu/~nolan/data/spam/SpamAssassinMessages.zip

#-------------------------------------------------#
#---------------GET DATA FILES DATA---------------#
#-------------------------------------------------#

#point to where the files are stored & read in data
getDirNames = function(spamPath = '/home/newall/MSDS/MSDS7333/Unit6'){
  #look up dirs
  dirNames = list.files(path = spamPath)
  
  #look at the top of a file
  length(list.files(path = paste(spamPath, dirNames, sep = .Platform$file.sep)))
  
  #count files per group
  sapply(paste(spamPath, dirNames,sep = .Platform$file.sep),function(dir) length(list.files(dir)) )
  
  #get the full path
  fullDirNames = paste(spamPath, dirNames, sep = .Platform$file.sep)
  
  return(fullDirNames)
}

fullDirNames = getDirNames()

#get the list of files
fileNames = list.files(fullDirNames[1], full.names = TRUE)

#create test set
indx = c(1:5, 15, 27, 68, 69, 329, 404, 427, 516, 852, 971)
fn = list.files(fullDirNames[1], full.names = TRUE)[indx]
sampleEmail = sapply(fn, readLines)

#-------------------------------------------------#
#------------PREPROCESSING FUNCTIONS--------------#
#-------------------------------------------------#

#split message function
splitMessage = function(msg) {
  splitPoint = match("", msg)
  header = msg[1:(splitPoint-1)]
  body = msg[ -(1:splitPoint) ]
  return(list(header = header, body = body))
}

#get boundary function
getBoundary = function(header) {
  boundaryIdx = grep("boundary=", header)
  boundary = gsub('"', "", header[boundaryIdx])
  gsub(".*boundary=*([^;]*);?.*", "\\1", boundary)
}


dropAttach = function(body, boundary){
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  
  if (length(bStringLocs) <= 1) return(body)
  
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  if (length(eStringLoc) == 0) 
    return(body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1)])
  
  n = length(body)
  if (eStringLoc < n) 
    return( body[ c( (bStringLocs[1] + 1) : (bStringLocs[2] - 1), 
                     ( (eStringLoc + 1) : n )) ] )
  
  return( body[ (bStringLocs[1] + 1) : (bStringLocs[2] - 1) ])
}

cleanText = function(msg){
    tolower(gsub("[[:punct:]0-9[:space:][:blank:]]+", " ", msg))
}

findMsgWords = function(msg, stopWords) {
    if(is.null(msg))
      return(character())
    
    words = unique(unlist(strsplit(cleanText(msg), "[[:blank:]\t]+")))
    
    # drop empty and 1 letter words
    words = words[ nchar(words) > 1]
    words = words[ !( words %in% stopWords) ]
    invisible(words)
}

processAllWords = function(dirName, stopWords){
  
  # read all files in the directory
  fileNames = list.files(dirName, full.names = TRUE)
  
  # drop files that are not email, i.e., cmds
  notEmail = grep("cmds$", fileNames) 
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  
  messages = lapply(fileNames, readLines, encoding = "latin1")
  
  # split header and body
  emailSplit = lapply(messages, splitMessage)
  
  # put body and header in own lists
  bodyList = lapply(emailSplit, function(msg) msg$body)
  headerList = lapply(emailSplit, function(msg) msg$header)
  rm(emailSplit)
  
  # determine which messages have attachments
  hasAttach = sapply(headerList, function(header) {
    CTloc = grep("Content-Type", header)
    if (length(CTloc) == 0) return(0)
    multi = grep("multi", tolower(header[CTloc]))
    if (length(multi) == 0) return(0)
    multi
  })
  
  hasAttach = which(hasAttach > 0)
  
  # find boundary strings for messages with attachments
  boundaries = sapply(headerList[hasAttach], getBoundary)
  
  # drop attachments from message body
  bodyList[hasAttach] = mapply(dropAttach, bodyList[hasAttach], boundaries, SIMPLIFY = FALSE)
  
  # extract words from body
  msgWordsList = lapply(bodyList, findMsgWords, stopWords)
  invisible(msgWordsList)
}

#-------------------------------------------------#
#----------------PREPROCESS DATA------------------#
#-------------------------------------------------#
#import stopwords from tm package
stopWords = stopwords()

#process data
msgWordsList = lapply(fullDirNames, processAllWords,stopWords = stopWords)

#calc num of msgs
numMsgs = sapply(msgWordsList, length)
numMsgs

#assign labels
isSpam = rep(c(FALSE, FALSE, FALSE, TRUE, TRUE), numMsgs)

#flatten to 1 list
msgWordsList = unlist(msgWordsList, recursive = FALSE)


#-------------------------------------------------#
#----------------TRAIN TEST SPLIT-----------------#
#-------------------------------------------------#


numEmail = length(isSpam)
numSpam = sum(isSpam)
numHam = numEmail - numSpam

set.seed(418910)

testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

testMsgWords = c((msgWordsList[isSpam])[testSpamIdx],
                 (msgWordsList[!isSpam])[testHamIdx] )
trainMsgWords = c((msgWordsList[isSpam])[ - testSpamIdx], 
                  (msgWordsList[!isSpam])[ - testHamIdx])

testIsSpam = rep(c(TRUE, FALSE), 
                 c(length(testSpamIdx), length(testHamIdx)))
trainIsSpam = rep(c(TRUE, FALSE), 
                  c(numSpam - length(testSpamIdx), 
                    numHam - length(testHamIdx)))


#-------------------------------------------------#
#----------------PROBABILITY EST------------------#
#-------------------------------------------------#

bow = unique(unlist(trainMsgWords))

length(bow)

spamWordCounts = rep(0, length(bow))

names(spamWordCounts) = bow

tmp = lapply(trainMsgWords[trainIsSpam], unique)
tt = table( unlist(tmp) )
spamWordCounts[ names(tt) ] = tt

library(tm)
library(dplyr)
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
# 
# #-------------------------------------------------#
# #----------------TEST TRAIN SPLIT-----------------#
# #-------------------------------------------------#
numEmail = length(isSpam)
numSpam = sum(isSpam)
numHam = numEmail - numSpam
set.seed(418910)
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))
testMsgWords = c((msgWordsList[isSpam])[testSpamIdx],
                 (msgWordsList[!isSpam])[testHamIdx])
trainMsgWords = c((msgWordsList[isSpam])[ - testSpamIdx],
                  (msgWordsList[!isSpam])[ - testHamIdx])
testIsSpam = rep(c(TRUE, FALSE), c(length(testSpamIdx),
                                   length(testHamIdx)))
trainIsSpam = rep(c(TRUE, FALSE), c(numSpam - length(testSpamIdx),
                                    numHam - length(testHamIdx)))

# #-------------------------------------------------#
# #----------------NAIVE BAYES----------------------#
# #-------------------------------------------------#
# computeFreqs = function(wordsList, spam, bow = unique(unlist(wordsList))) { 
#   #create a matrix for spam, ham, and log odds 
#   wordTable = matrix(0.5, nrow = 4, ncol = length(bow), 
#                      dimnames = list(c("spam", "ham", 
#                                        "presentLogOdds", 
#                                        "absentLogOdds"), bow))
#   #For each spam message, add 1 to counts for words in message 
#   counts.spam = table(unlist(lapply(wordsList[spam], unique))) 
#   wordTable["spam", names(counts.spam)] = counts.spam + .5 
#   #Similarly for ham messages 
#   counts.ham = table(unlist(lapply(wordsList[!spam], unique))) 
#   wordTable["ham", names(counts.ham)] = counts.ham + .5 
#   #Find the total number of spam and ham 
#   numSpam = sum(spam) 
#   numHam = length(spam) - numSpam 
#   #Prob(word|spam) and Prob(word | ham) 
#   wordTable["spam",] = wordTable["spam", ]/(numSpam + .5) 
#   wordTable["ham",] = wordTable["ham", ]/(numHam + .5) 
#   #log odds 
#   wordTable["presentLogOdds",] = 
#     log(wordTable["spam",]) - log(wordTable["ham",]) 
#   wordTable["absentLogOdds",] = 
#     log((1 - wordTable["spam",])) - log((1 -wordTable["ham",])) 
#   
#   invisible(wordTable) 
# }
# 
# trainTable = computeFreqs(trainMsgWords, trainIsSpam)
# 
# computeMsgLLR = function(words, freqTable) { 
#   #Discards words not in training data. 
#   words = words[!is.na(match(words, colnames(freqTable)))] 
#   #Find which words are present 
#   present = colnames(freqTable) %in% words 
#   sum(freqTable["presentLogOdds", present]) + sum(freqTable["absentLogOdds", !present]) 
# }
# 
# typeIErrorRates = function(llrVals, isSpam) { 
#   o = order(llrVals) 
#   llrVals = llrVals[o] 
#   isSpam = isSpam[o] 
#   idx = which(!isSpam) 
#   N = length(idx) 
#   list(error = (N:1)/N, values = llrVals[idx])
# }
#  

#-------------------------------------------------#
#----------------DATA PREP ROUND 2----------------#
#-------------------------------------------------#
processHeader = function(header) { 
  #modify the first line to create a key:value pair 
  header[1] = sub("^From", "Top-From:", header[1]) 
  headerMat = read.dcf(tc<-textConnection(header), all = TRUE)
  close(tc)
  headerVec = unlist(headerMat) 
  dupKeys = sapply(headerMat, function(x) length(unlist(x))) 
  names(headerVec) = rep(colnames(headerMat), dupKeys) 
  return(headerVec)
}


processAttach = function(body, contentType){
  n = length(body)
  boundary = getBoundary(contentType)
  
  bString = paste("--", boundary, sep = "")
  bStringLocs = which(bString == body)
  eString = paste("--", boundary, "--", sep = "")
  eStringLoc = which(eString == body)
  
  if (length(eStringLoc) == 0) eStringLoc = n
  if (length(bStringLocs) <= 1) {
    attachLocs = NULL
    msgLastLine = n
    if (length(bStringLocs) == 0) bStringLocs = 0
  } else {
    attachLocs = c(bStringLocs[ -1 ],  eStringLoc)
    msgLastLine = bStringLocs[2] - 1
  }
  
  msg = body[ (bStringLocs[1] + 1) : msgLastLine] 
  if ( eStringLoc < n )
    msg = c(msg, body[ (eStringLoc + 1) : n ])
  
  if ( !is.null(attachLocs) ) {
    attachLens = diff(attachLocs, lag = 1) 
    attachTypes = mapply(function(begL, endL) {
      CTloc = grep("^[Cc]ontent-[Tt]ype", body[ (begL + 1) : (endL - 1)])
      if ( length(CTloc) == 0 ) {
        MIMEType = NA
      } else {
        CTval = body[ begL + CTloc[1] ]
        CTval = gsub('"', "", CTval )
        MIMEType = sub(" *[Cc]ontent-[Tt]ype: *([^;]*);?.*", "\\1", CTval)   
      }
      return(MIMEType)
    }, attachLocs[-length(attachLocs)], attachLocs[-1])
  }
  
  if (is.null(attachLocs)) return(list(body = msg, attachDF = NULL) )
  return(list(body = msg, 
              attachDF = data.frame(aLen = attachLens, 
                                    aType = unlist(attachTypes),
                                    stringsAsFactors = FALSE)))                                
}                       

readEmail = function(dirName) {
  # retrieve the names of files in directory
  fileNames = list.files(dirName, full.names = TRUE)
  # drop files that are not email
  notEmail = grep("cmds$", fileNames)
  if ( length(notEmail) > 0) fileNames = fileNames[ - notEmail ]
  
  # read all files in the directory
  lapply(fileNames, readLines, encoding = "latin1")
}

processAllEmail = function(dirName, isSpam = FALSE)
{
  # read all files in the directory
  messages = readEmail(dirName)
  fileNames = names(messages)
  n = length(messages)
  
  # split header from body
  eSplit = lapply(messages, splitMessage)
  rm(messages)
  
  # process header as named character vector
  headerList = lapply(eSplit, function(msg) 
    processHeader(msg$header))
  
  # extract content-type key
  contentTypes = sapply(headerList, function(header) 
    header["Content-Type"])
  
  # extract the body
  bodyList = lapply(eSplit, function(msg) msg$body)
  rm(eSplit)
  
  # which email have attachments
  hasAttach = grep("^ *multi", tolower(contentTypes))
  
  # get summary stats for attachments and the shorter body
  attList = mapply(processAttach, bodyList[hasAttach], 
                   contentTypes[hasAttach], SIMPLIFY = FALSE)
  
  bodyList[hasAttach] = lapply(attList, function(attEl) 
    attEl$body)
  
  attachInfo = vector("list", length = n )
  attachInfo[ hasAttach ] = lapply(attList, 
                                   function(attEl) attEl$attachDF)
  
  # prepare return structure
  emailList = mapply(function(header, body, attach, isSpam) {
    list(isSpam = isSpam, header = header, 
         body = body, attach = attach)
  },
  headerList, bodyList, attachInfo, 
  rep(isSpam, n), SIMPLIFY = FALSE )
  names(emailList) = fileNames
  
  invisible(emailList)
}

emailStruct = mapply(processAllEmail, fullDirNames,
                     isSpam = rep( c(FALSE, TRUE), 3:2))      
emailStruct = unlist(emailStruct, recursive = FALSE)

#sampleStruct = emailStruct[ indx ]

getMessageRecipients =
  function(header)
  {
    c(if("To" %in% names(header))  header[["To"]] else character(0),
      if("Cc" %in% names(header))  header[["Cc"]] else character(0),
      if("Bcc" %in% names(header)) header[["Bcc"]] else character(0)
    )
  }

SpamCheckWords =
  c("viagra", "pounds", "free", "weight", "guarantee", "million", 
    "dollars", "credit", "risk", "prescription", "generic", "drug",
    "financial", "save", "dollar", "erotic", "million", "barrister",
    "beneficiary", "easy", 
    "money back", "money", "credit card")

funcList = list(
  isSpam =
    expression(msg$isSpam)
  ,
  isRe =
    function(msg) {
      # Can have a Fwd: Re:  ... but we are not looking for this here.
      # We may want to look at In-Reply-To field.
      "Subject" %in% names(msg$header) && 
        length(grep("^[ \t]*Re:", msg$header[["Subject"]])) > 0
    }
  ,
  numLines =
    function(msg) length(msg$body)
  ,
  bodyCharCt =
    function(msg)
      sum(nchar(msg$body))
  ,
  underscore =
    function(msg) {
      if(!"Reply-To" %in% names(msg$header))
        return(FALSE)
      
      txt <- msg$header[["Reply-To"]]
      length(grep("_", txt)) > 0  && 
        length(grep("[0-9A-Za-z]+", txt)) > 0
    }
  ,
  subExcCt = 
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^!]","", x)))
    }
  ,
  subQuesCt =
    function(msg) {
      x = msg$header["Subject"]
      if(length(x) == 0 || sum(nchar(x)) == 0 || is.na(x))
        return(NA)
      
      sum(nchar(gsub("[^?]","", x)))
    }
  ,
  numAtt = 
    function(msg) {
      if (is.null(msg$attach)) return(0)
      else nrow(msg$attach)
    }
  
  ,
  priority =
    function(msg) {
      ans <- FALSE
      # Look for names X-Priority, Priority, X-Msmail-Priority
      # Look for high any where in the value
      ind = grep("priority", tolower(names(msg$header)))
      if (length(ind) > 0)  {
        ans <- length(grep("high", tolower(msg$header[ind]))) >0
      }
      ans
    }
  ,
  numRec =
    function(msg) {
      # unique or not.
      els = getMessageRecipients(msg$header)
      
      if(length(els) == 0)
        return(NA)
      
      # Split each line by ","  and in each of these elements, look for
      # the @ sign. This handles
      tmp = sapply(strsplit(els, ","), function(x) grep("@", x))
      sum(sapply(tmp, length))
    }
  ,
  perCaps =
    function(msg)
    {
      body = paste(msg$body, collapse = "")
      
      # Return NA if the body of the message is "empty"
      if(length(body) == 0 || nchar(body) == 0) return(NA)
      
      # Eliminate non-alpha characters and empty lines 
      body = gsub("[^[:alpha:]]", "", body)
      els = unlist(strsplit(body, ""))
      ctCap = sum(els %in% LETTERS)
      100 * ctCap / length(els)
    }
  ,
  isInReplyTo =
    function(msg)
    {
      "In-Reply-To" %in% names(msg$header)
    }
  ,
  sortedRec =
    function(msg)
    {
      ids = getMessageRecipients(msg$header)
      all(sort(ids) == ids)
    }
  ,
  subPunc =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        el = gsub("['/.:@-]", "", msg$header["Subject"])
        length(grep("[A-Za-z][[:punct:]]+[A-Za-z]", el)) > 0
      }
      else
        FALSE
    },
  hour =
    function(msg)
    {
      date = msg$header["Date"]
      if ( is.null(date) ) return(NA)
      # Need to handle that there may be only one digit in the hour
      locate = regexpr("[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]", date)
      
      if (locate < 0)
        locate = regexpr("[0-2]?[0-9]:[0-5][0-9]", date)
      if (locate < 0) return(NA)
      
      hour = substring(date, locate, locate+1)
      hour = as.numeric(gsub(":", "", hour))
      
      locate = regexpr("PM", date)
      if (locate > 0) hour = hour + 12
      
      locate = regexpr("[+-][0-2][0-9]00", date)
      if (locate < 0) offset = 0
      else offset = as.numeric(substring(date, locate, locate + 2))
      (hour - offset) %% 24
    }
  ,
  multipartText =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      numAtt = nrow(msg$attach)
      
      types = 
        length(grep("(html|plain|text)", msg$attach$aType)) > (numAtt/2)
    }
  ,
  hasImages =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("^ *image", tolower(msg$attach$aType))) > 0
    }
  ,
  isPGPsigned =
    function(msg)
    {
      if (is.null(msg$attach)) return(FALSE)
      
      length(grep("pgp", tolower(msg$attach$aType))) > 0
    },
  perHTML =
    function(msg)
    {
      if(! ("Content-Type" %in% names(msg$header))) return(0)
      
      el = tolower(msg$header["Content-Type"]) 
      if (length(grep("html", el)) == 0) return(0)
      
      els = gsub("[[:space:]]", "", msg$body)
      totchar = sum(nchar(els))
      totplain = sum(nchar(gsub("<[^<]+>", "", els )))
      100 * (totchar - totplain)/totchar
    },
  subSpamWords =
    function(msg)
    {
      if("Subject" %in% names(msg$header))
        length(grep(paste(SpamCheckWords, collapse = "|"), 
                    tolower(msg$header["Subject"]))) > 0
      else
        NA
    }
  ,
  subBlanks =
    function(msg)
    {
      if("Subject" %in% names(msg$header)) {
        x = msg$header["Subject"]
        # should we count blank subject line as 0 or 1 or NA?
        if (nchar(x) == 1) return(0)
        else 100 *(1 - (nchar(gsub("[[:blank:]]", "", x))/nchar(x)))
      } else NA
    }
  ,
  noHost =
    function(msg)
    {
      # Or use partial matching.
      idx = pmatch("Message-", names(msg$header))
      
      if(is.na(idx)) return(NA)
      
      tmp = msg$header[idx]
      return(length(grep(".*@[^[:space:]]+", tmp)) ==  0)
    }
  ,
  numEnd =
    function(msg)
    {
      # If we just do a grep("[0-9]@",  )
      # we get matches on messages that have a From something like
      # " \"marty66@aol.com\" <synjan@ecis.com>"
      # and the marty66 is the "user's name" not the login
      # So we can be more precise if we want.
      x = names(msg$header)
      if ( !( "From" %in% x) ) return(NA)
      login = gsub("^.*<", "", msg$header["From"])
      if ( is.null(login) ) 
        login = gsub("^.*<", "", msg$header["X-From"])
      if ( is.null(login) ) return(NA)
      login = strsplit(login, "@")[[1]][1]
      length(grep("[0-9]+$", login)) > 0
    },
  isYelling =
    function(msg)
    {
      if ( "Subject" %in% names(msg$header) ) {
        el = gsub("[^[:alpha:]]", "", msg$header["Subject"])
        if (nchar(el) > 0) nchar(gsub("[A-Z]", "", el)) < 1
        else FALSE
      }
      else
        NA
    },
  forwards =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      ans = length(grep("^[[:space:]]*>", x))
      100 * ans / length(x)
    },
  isOrigMsg =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[^[:alpha:]]*original[^[:alpha:]]+message[^[:alpha:]]*$", 
                  tolower(x) ) ) > 0
    },
  isDear =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("^[[:blank:]]*dear +(sir|madam)\\>", 
                  tolower(x))) > 0
    },
  isWrote =
    function(msg)
    {
      x = msg$body
      if(length(x) == 0) return(NA)
      
      length(grep("(wrote|schrieb|ecrit|escribe):", tolower(x) )) > 0
    },
  avgWordLen =
    function(msg)
    {
      txt = paste(msg$body, collapse = " ")
      if(length(txt) == 0 || sum(nchar(txt)) == 0) return(0)
      
      txt = gsub("[^[:alpha:]]", " ", txt)
      words = unlist(strsplit(txt, "[[:blank:]]+"))
      wordLens = nchar(words)
      mean(wordLens[ wordLens > 0 ])
    }
  ,
  numDlr =
    function(msg)
    {
      x = paste(msg$body, collapse = "")
      if(length(x) == 0 || sum(nchar(x)) == 0)
        return(NA)
      
      nchar(gsub("[^$]","", x))
    }
)

createDerivedDF =
  function(email = emailStruct, operations = funcList, 
           verbose = FALSE)
  {
    els = lapply(names(operations),
                 function(id) {
                   if(verbose) print(id)
                   e = operations[[id]]
                   v = if(is.function(e)) 
                     sapply(email, e)
                   else 
                     sapply(email, function(msg) eval(e))
                   v
                 })
    
    df = as.data.frame(els)
    names(df) = names(operations)
    invisible(df)
  }

emailDF = createDerivedDF(emailStruct)

#-------------------------------------------------#
#----------------RECURSIVE PARTITIONING-----------#
#-------------------------------------------------#
library(rpart)

setupRpart = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], 
                   function(x) {
                     x = as.factor(x)
                     levels(x) = c("F", "T")
                     x
                   })
  cbind(facVars, data[ , - logicalVars])
}

emailDFrp = setupRpart(emailDF)

str(emailDFrp)

set.seed(418910)
testSpamIdx = sample(numSpam, size = floor(numSpam/3))
testHamIdx = sample(numHam, size = floor(numHam/3))

testDF = 
  rbind( emailDFrp[ emailDFrp$isSpam == "T", ][testSpamIdx, ],
         emailDFrp[emailDFrp$isSpam == "F", ][testHamIdx, ] )
trainDF =
  rbind( emailDFrp[emailDFrp$isSpam == "T", ][-testSpamIdx, ], 
         emailDFrp[emailDFrp$isSpam == "F", ][-testHamIdx, ])

library(purrr)
library(dplyr)

grid <- list(minsplit = c(5),
             maxdepth = c(5, 10, 15, 20 , 25, 30),
             cp = c(0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001)
            ) %>% cross_df()



trainModel <- function(...) {
  rpart(isSpam ~ ., data = trainDF,
        method="class", 
        control = rpart.control(...) )
}


grid <- grid %>% mutate(fit = pmap(grid, trainModel))

compute_accuracy <- function(fit, test_features, test_labels) {
  predicted <- predict(fit, test_features, type = "class")
  mean(predicted == test_labels)
}

compute_type1 <- function(fit, test_features, test_labels) {
  predicted <- predict(fit, test_features, type = "class")
  mean(predicted[ !test_labels == "T" ] == "T") 
}

compute_type2 <- function(fit, test_features, test_labels) {
  predicted <- predict(fit, test_features, type = "class")
  mean(predicted[ test_labels == "T" ] == "F") 
}

test_features <- testDF %>% select(-isSpam)
test_labels   <- testDF$isSpam

grid <- grid %>%
  mutate(test_accuracy = map_dbl(fit, compute_accuracy,
                                 test_features, test_labels)) %>% 
  mutate(test_type1 = map_dbl(fit, compute_type1,
                                 test_features, test_labels)) %>%
  mutate(test_type2 = map_dbl(fit, compute_type2,
                              test_features, test_labels))

grid <- grid %>% arrange(desc(test_accuracy), desc(test_type2), desc(test_type1), desc(maxdepth))
grid

library(ggplot2)
## plot type I data
ggplot(grid, aes(as.factor(maxdepth), as.factor(cp))) +
  geom_tile(aes(fill = test_type1*100)) + 
  geom_text(aes(label = round(test_type1*100, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab('Max Depth') + ylab('Complexity Parameter') +
  guides(fill = guide_colourbar(title = NULL)) +
  ggtitle("Type I Error Rates") + 
  theme(plot.title = element_text(hjust = 0.5))


## plot type II
ggplot(grid, aes(as.factor(maxdepth), as.factor(cp))) +
  geom_tile(aes(fill = test_type2*100)) + 
  geom_text(aes(label = round(test_type2*100, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab('Max Depth') + ylab('Complexity Parameter') +
  guides(fill = guide_colourbar(title = NULL)) +
  ggtitle("Type II Error Rates") + 
  theme(plot.title = element_text(hjust = 0.5))

## plot accuracy
ggplot(grid, aes(as.factor(maxdepth), as.factor(cp))) +
  geom_tile(aes(fill = test_accuracy*100)) + 
  geom_text(aes(label = round(test_accuracy*100, 2))) +
  scale_fill_gradient(low = "white", high = "red") +
  xlab('Max Depth') + ylab('Complexity Parameter') +
  guides(fill = guide_colourbar(title = NULL)) +
  ggtitle("Accuracy") + 
  theme(plot.title = element_text(hjust = 0.5))


rpartObj = rpart(isSpam ~ ., data = trainDF,
                 method="class",
                 control = rpart.control(minsplit =7, maxdepth = 30, cp=0.001) )

plot(rpartObj)


# preds <- predict(rpartObj,
#         newdata = testDF[ , names(testDF) != "isSpam"],
#         type = "class")
# 
# 
# spam = testDF$isSpam == "T"
# numSpam = sum(spam)
# numHam = sum(!spam)
# 
# sum(preds[ !spam ] == "T") / numHam
# sum(preds[ spam ] == "F") / numSpam

# errs = sapply(rpartObj, function(preds) {
#   typeI = sum(preds[ !spam ] == "T") / numHam
#   typeII = sum(preds[ spam ] == "F") / numSpam
#   c(typeI = typeI, typeII = typeII)
# })

# plot(errs[1,] ~ complexityVals, type="l", col='blue', 
#      lwd = 2,  ylim = c(0,0.2), xlim = c(0,0.005), 
#      ylab="Error", xlab="complexity parameter values")
# points(errs[2,] ~ complexityVals, type="l", col='green', lwd = 2)
# text(x =c(0.003, 0.0035), y = c(0.12, 0.05), 
#      labels=c("Type II Error", "Type I Error"))

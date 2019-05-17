
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

#remove header
lines = txt[ substr(txt, 1, 1) != "#" ]
tmp = lapply(lines, processLine)

#convert to R data frame
offline = as.data.frame(do.call("rbind", tmp))

#data frame dimensions
dim(offline)

#name columns
names(offline) = c("time", "scanMac", "posX", "posY", "posZ","orientation", "mac", "signal","channel", "type")

#conver numvars
numVars = c("time", "posX", "posY", "posZ","orientation", "signal")
offline[ numVars ] =  lapply(offline[ numVars ], as.numeric)


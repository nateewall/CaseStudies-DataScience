library(XML)

extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url, year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url, encoding='UTF-8')
    
    if (year == 1999) {
      ff = getNodeSet(doc, "//pre")
      txt = xmlValue(ff[[1]])
      els = strsplit(txt, "\n")[[1]] ## the '99 files use \n not \r\n
    }
    else if (year == 2000) {
      # Revise the HTML by line to remove the /font tags and force the parse to correctly infer where they belong
      html2000 = readLines("http://www.cherryblossom.org/results/2000/cb003m.htm")
      newhtml2000 = sub("</font>","",html2000)
      doc = htmlParse(newhtml2000)
      pres = getNodeSet(doc,"//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]] 
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else if (year == 2001 & sex == "female") {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]
      els = els[-(1:3)]
      fix2001 = c("PLACE NUM   NAME                  AG HOMETOWN           NET     GUN",
                  "===== ===== ===================== == ================== ======= =======")
      els = c(fix2001, els)
    }
    else if (year == 2006) {
      # THIS FIXES ISSUE WITH 2006 FILES AS NO SEPARATOR
      # IN SPACER ROW BETWEEN HOMETOWN AND NET TIME
      # OUTLINED ON PG. 62 - fixed for both gender
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]] 
      separatorIdx = grep("^===", els)
      separatorRow = els[separatorIdx]
      separatorRowX = paste(substring(separatorRow, 1, 63), " ", substring(separatorRow, 65, nchar(separatorRow)), sep = "") # insert space
      els[separatorIdx] = separatorRowX # replace row
    }
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)){
      return(els)
    } else {
      # Write the lines as a text file.
      fileConn = file(file)
      writeLines(els, con = fileConn)
      close(fileConn)
      return(els)
    }
  }

# WILKE UPDATED
menURLs = 
  c("results/1999/cb99m.html", "results/2000/Cb003m.htm", "results/2001/oof_m.html",
    "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
    "results/2004/men.htm", "results/2005/CB05-M.htm", 
    "results/2006/men.htm", "results/2007/men.htm", 
    "results/2008/men.htm", "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")

# create URLs for getting all men's data 
ubase = "http://www.cherryblossom.org/"
men_urls = paste(ubase, menURLs, sep = "")

womenURLs = c("results/1999/cb99f.html", "results/2000/Cb003f.htm", "results/2001/oof_f.html",
              "results/2002/ooff.htm", "results/2003/CB03-F.HTM",
              "results/2004/women.htm", "results/2005/CB05-F.htm", 
              "results/2006/women.htm", "results/2007/women.htm", 
              "results/2008/women.htm", "results/2009/09cucb-F.htm",
              "results/2010/2010cucb10m-f.htm", 
              "results/2011/2011cucb10m-f.htm",
              "results/2012/2012cucb10m-f.htm")


years = 1999:2012

women_urls = paste(ubase, womenURLs, sep = "")

menFiles = sapply(years, function(x){paste(x, "m.txt", sep="")})
womenFiles = sapply(years, function(x){paste(x, "w.txt", sep="")})

mensTables = mapply(extractResTable, url = men_urls, year = years, sex = "male", file = menFiles)
womensTables = mapply(extractResTable, url = women_urls, year = years, sex = "female", file = womenFiles)

names(mensTables) = years
names(womensTables) = years

#save(mensTables, file = "CBMenTextTables.rda")
#save(womensTables, file = "CBWomenTextTables.rda")


```

```{r}
findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]  # returns the index of spaces
  rowLength = nchar(spacerRow) # returns the length of the spacer row
  
  if (substring(spacerRow, rowLength, rowLength) != " ") # if the last character of the spacerRow isn't a space
    return( c(0, spaceLocs, rowLength + 1)) # return the spacer locations with an additional location 1 greater than the space row
  else return(c(0, spaceLocs)) # else, space exists so return the spaceLocs as is
}

selectCols = function(colNames, headerRow, searchLocs) 
{
  sapply(colNames, 
         function(name, headerRow, searchLocs)
         {
           startPos = regexpr(name, headerRow)[[1]] # find the starting position of the name in the headerRow
           if (startPos == -1)  # if startPos = -1 (name doesn't exist) return NAs
             return( c(NA, NA) )
           
           index = sum(startPos >= searchLocs) # find the first Loc that contains the name
           c(searchLocs[index] + 1, searchLocs[index + 1]) # return position that is one greater than the space containing the name, and one less than next space // then modified to fix issues on pg. 57
         },
         headerRow = headerRow, searchLocs = searchLocs )
}


```

```{r}

extractVariables =  function(file, varNames =c("name", "home", "ag", "gun", "net", "time"))
{
  # Find the index of the row with =s
  eqIndex = grep("^===", file)
  
  # Extract the two key rows and the data: spacerRow and headerRow
  spacerRow = file[eqIndex] 
  headerRow = tolower(file[ eqIndex - 1 ])
  body = file[ -(1 : eqIndex) ]
  
  # Remove footnotes and blank rows // this is added per cleaning on pages 58-59
  footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
  if ( length(footnotes) > 0 ) body = body[ -footnotes ]
  
  blanks = grep("^[[:blank:]]*$", body)
  if (length(blanks) > 0 ) body = body[ -blanks ]
  
  # Obtain the starting and ending positions of variables
  searchLocs = findColLocs(spacerRow)
  locCols = selectCols(varNames, headerRow, searchLocs)
  
  Values = mapply(substr, list(body), start = locCols[1, ], stop = locCols[2, ])
  colnames(Values) = varNames
  
  invisible(Values) # change the print mode to invisble
}

```


```{r}
convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

```


```{r}

menResMat = sapply(mensTables, extractVariables)
womenResMat = sapply(womensTables, extractVariables)

womenDF = mapply(createDF, womenResMat, year = 1999:2012, sex = rep("W", 14), SIMPLIFY = FALSE)
menDF = mapply(createDF, menResMat, year = 1999:2012, sex = rep("M", 14), SIMPLIFY = FALSE)

cbMen = do.call(rbind, menDF)
cbWomen = do.call(rbind, womenDF)

```


```{r}
#Plots!
menAge = sapply(menResMat,
             function(x) as.numeric(x[ , 'ag']))
womenAge = sapply(womenResMat,
                function(x) as.numeric(x[ , 'ag']))


pdf("CB_BoxplotAgeByYr_Men.pdf", width = 8, height = 5)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
boxplot(menAge, ylab = "Age", xlab = "Year")
par(oldPar)
dev.off()

pdf("CB_BoxplotAgeByYr_Women.pdf", width = 8, height = 5)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
boxplot(womenAge, ylab = "Age", xlab = "Year")
par(oldPar)
dev.off()


pdf("CB_BoxplotTimeByYr_Men.pdf", width = 8, height = 5)
boxplot(sapply(menDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")
dev.off()

pdf("CB_BoxplotTimeByYr_Women.pdf", width = 8, height = 5)
boxplot(sapply(womenDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")
dev.off()


#start of section 2.4
pdf("CB_Overplot_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(runTime ~ age, data = cbMen, ylim = c(40, 180),
     xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()

pdf("CB_Overplot_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(runTime ~ age, data = cbWomen, ylim = c(40, 180),
     xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()

library(RColorBrewer)
#ls("package:RColorBrewer")

#display.brewer.all()

#Purples8 = brewer.pal(9, "Purples")[8]
#Purples8

OrRd8 = brewer.pal(9, "OrRd")[8]
Blues8 = brewer.pal(9,"Blues")[8]

#Purples8A = paste(Purples8, "14", sep = "")
OrRd8A = paste(OrRd8, "14", sep = "")
Blues8A = paste(Blues8, "14", sep = "")

pdf("CB_OverplotTransparent_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbMen, 
     pch = 19,cex = 0.2, col = OrRd8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()

pdf("CB_OverplotTransparent_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbWomen, 
     pch = 19,cex = 0.2, col = Blues8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()


pdf("CB_SmoothScatter_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
smoothScatter(y = cbMen$runTime, x = cbMen$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()

pdf("CB_SmoothScatter_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
smoothScatter(y = cbWomen$runTime, x = cbWomen$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()



cbMenSub = cbMen[cbMen$runTime > 30 &
                   !is.na(cbMen$age) & cbMen$age > 15, ]
cbWomenSub = cbWomen[cbWomen$runTime > 30 &
                   !is.na(cbWomen$age) & cbWomen$age > 15, ]

ageCatm = cut(cbMenSub$age, breaks = c(seq(15, 75, 10), 90))
ageCatw = cut(cbWomenSub$age, breaks = c(seq(15, 75, 10), 90))
table(ageCatm)
table(ageCatw)

pdf("CB_Boxplots_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(cbMenSub$runTime ~ ageCatm, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()

pdf("CB_Boxplots_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(cbWomenSub$runTime ~ ageCatw, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")
par(oldPar)
dev.off()



lmAge_m = lm(runTime ~ age, data = cbMenSub)
lmAge_m$coefficients
summary(lmAge_m)

lmAge_w = lm(runTime ~ age, data = cbWomenSub)
lmAge_w$coefficients
summary(lmAge_w)


pdf("CB_ResidSimpleLM_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
smoothScatter(x = cbMenSub$age, y = lmAge_m$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)
resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lmAge_m),
                                   age = cbMenSub$age))
age20to80 = 20:80
resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))
lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)
par(oldPar)
dev.off()

pdf("CB_ResidSimpleLM_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
smoothScatter(x = cbWomenSub$age, y = lmAge_w$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)
resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lmAge_w),
                                   age = cbWomenSub$age))
resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))
lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)
par(oldPar)
dev.off()



menRes.lo = loess(runTime ~ age, cbMenSub)
menRes.lo.pr = predict(menRes.lo, data.frame(age = age20to80))
over50_m = pmax(0, cbMenSub$age - 50)
lmOver50_m = lm(runTime ~ age + over50_m, data = cbMenSub)
summary(lmOver50_m)

womenRes.lo = loess(runTime ~ age, cbWomenSub)
womenRes.lo.pr = predict(womenRes.lo, data.frame(age = age20to80))
over50_w = pmax(0, cbWomenSub$age - 50)
lmOver50_w = lm(runTime ~ age + over50_w, data = cbWomenSub)
summary(lmOver50_w)

decades = seq(30, 60, by = 10)
overAge_m = lapply(decades, 
                 function(x) pmax(0, (cbMenSub$age - x)))
names(overAge_m) = paste("over", decades, sep = "")
overAge_m = as.data.frame(overAge_m)

overAge_w = lapply(decades, 
                   function(x) pmax(0, (cbWomenSub$age - x)))
names(overAge_w) = paste("over", decades, sep = "")
overAge_w = as.data.frame(overAge_w)


lmPiecewise_m = lm(runTime ~ . , 
                 data = cbind(cbMenSub[, c("runTime", "age")], 
                              overAge_m))
summary(lmPiecewise_m)
lmPiecewise_w = lm(runTime ~ . , 
                   data = cbind(cbWomenSub[, c("runTime", "age")], 
                                overAge_w))
summary(lmPiecewise_w)

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

predPiecewise_m = predict(lmPiecewise_m, overAgeDF)
predPiecewise_w = predict(lmPiecewise_w, overAgeDF)


pdf("CB_PiecewiseLoessCurves_Men.pdf", width = 8, height = 6)
plot(predPiecewise_m ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     #   type = "l", col = "purple", lwd = 2,
     xlab = "Age (years)", ylab = "Run Time Prediction")
lines(x = age20to80, y = menRes.lo.pr, col = "#4daf4a", lwd = 3, lty = 2)
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = c(1, 2), lwd = 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")
dev.off()

pdf("CB_PiecewiseLoessCurves_Women.pdf", width = 8, height = 6)
plot(predPiecewise_w ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     #   type = "l", col = "purple", lwd = 2,
     xlab = "Age (years)", ylab = "Run Time Prediction")
lines(x = age20to80, y = womenRes.lo.pr, col = "#4daf4a", lwd = 3, lty = 2)
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = c(1, 2), lwd = 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")
dev.off()



pdf("CB_NumRunnersLinePlot_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
numRunners_m= with(cbMen, tapply(runTime, year, length))
plot(numRunners_m ~ names(numRunners_m), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")
par(oldPar)
dev.off()

#summary(cbMenSub$runTime[cbMenSub$year == 1999])
#summary(cbMenSub$runTime[cbMenSub$year == 2012])

pdf("CB_NumRunnersLinePlot_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
numRunners_w = with(cbWomen, tapply(runTime, year, length))
plot(numRunners_w ~ names(numRunners_w), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")
par(oldPar)
dev.off()

#summary(cbWomenSub$runTime[cbWomenSub$year == 1999])
#summary(cbWomenSub$runTime[cbWomenSub$year == 2012])

pdf("CB_AgeDensity99vs12_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
age1999_m = cbMenSub[ cbMenSub$year == 1999, "age" ]
age2012_m = cbMenSub[ cbMenSub$year == 2012, "age" ]

plot(density(age1999_m, na.rm = TRUE), 
     ylim = c(0, 0.05), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012_m, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")
par(oldPar)
dev.off()

pdf("CB_AgeDensity99vs12_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
age1999_w = cbWomenSub[ cbWomenSub$year == 1999, "age" ]
age2012_w = cbWomenSub[ cbWomenSub$year == 2012, "age" ]

plot(density(age1999_w, na.rm = TRUE), 
     ylim = c(0, 0.05), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012_w, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")
par(oldPar)
dev.off()


qqplot(age1999_m, age2012_m, pch = 19, cex = 0.5, 
       ylim = c(10,90), xlim = c(10,90), 
       xlab = "Age in 1999 Race",
       ylab = "Age in 2012 Race", 
       main = "Quantile-quantile plot of male runner's age")
abline(a =0, b = 1, col="red", lwd = 2)

qqplot(age1999_w, age2012_w, pch = 19, cex = 0.5, 
       ylim = c(10,90), xlim = c(10,90), 
       xlab = "Age in 1999 Race",
       ylab = "Age in 2012 Race", 
       main = "Quantile-quantile plot of female runner's age")
abline(a =0, b = 1, col="red", lwd = 2)

mR.lo99_m = loess(runTime ~ age, cbMenSub[ cbMenSub$year == 1999,])
mR.lo.pr99_m = predict(mR.lo99_m, data.frame(age = age20to80))
mR.lo12_m = loess(runTime ~ age, cbMenSub[ cbMenSub$year == 2012,])
mR.lo.pr12_m = predict(mR.lo12_m, data.frame(age = age20to80))

mR.lo99_w = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 1999,])
mR.lo.pr99_w = predict(mR.lo99_w, data.frame(age = age20to80))
mR.lo12_w = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 2012,])
mR.lo.pr12_w = predict(mR.lo12_w, data.frame(age = age20to80))


pdf("CB_Loess99vs12_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

plot(mR.lo.pr99_m ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     xlab = "Age (years)", ylab = "Prediction (minutes)")  
lines(x = age20to80, y = mR.lo.pr12_m, col="#4daf4a", lty = 2, lwd = 3) 
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")
par(oldPar)
dev.off()

pdf("CB_Loess99vs12_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

plot(mR.lo.pr99_w ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     xlab = "Age (years)", ylab = "Prediction (minutes)")  
lines(x = age20to80, y = mR.lo.pr12_w, col="#4daf4a", lty = 2, lwd = 3) 
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = 1:2, lwd = 3,
       legend = c("1999", "2012"), bty = "n")
par(oldPar)
dev.off()

gap14_m = mR.lo.pr12_m - mR.lo.pr99_m
gap14_w = mR.lo.pr12_w - mR.lo.pr99_w

pdf("CB_DifferenceInFittedCurves_Men.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(gap14_m ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)
par(oldPar)
dev.off()

pdf("CB_DifferenceInFittedCurves_Women.pdf", width = 8, height = 6)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(gap14_w ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)
par(oldPar)
dev.off()
```
``` {r}
# This looks like code for normalizing the runners
# fastestMan = tapply(menRes$time, menRes$age, min, na.rm = TRUE)
# plot(fastestMan ~ names(fastestMan), type ="l", xlim = c(20, 80))
# ageFM = as.numeric(names(fastestMan))
# mR.loF = loess(fastestMan ~ ageFM)
# mR.lo.prF = predict(mR.loF, data.frame(age = ageFM), se = FALSE)
# lines(x = ageFM, y = mR.lo.prF, col = "purple", lwd = 2)
# 
# timeNorm = menRes$time / mR.lo.prF[as.character(menRes$age)]
# time99Norm = timeNorm[menRes$year == 1999]
# time12Norm = timeNorm[menRes$year == 2012]
# summary(time99Norm)
# summary(time12Norm)

# plot(density(100*time99Norm, na.rm = TRUE), 
#      # ylim = c(0, 0.05), 
#      col = "purple",
#      lwd = 3,  xlab = "Time (percentage)",
#      main = "Time Distribution for 1999 and 2012 Runners\n Percentage of the fastest runner for that age")
# lines(density(100*time12Norm, na.rm = TRUE), 
#       lwd = 3, col = "green")
# legend("topleft", fill = c("purple", "green"),
#        legend = c("1999", "2012"), bty = "n")

```

```{r}
# Start of Section 2.5
trimBlanks = function(charVector) {
  nameClean = gsub("^[[:blank:]]+", "", charVector)
  nameClean = gsub("[[:blank:]]+$", "", nameClean)
  nameClean = gsub("[[:blank:]]+", " ", nameClean)
}

nameClean_m = trimBlanks(cbMenSub$name)
nameClean_w = trimBlanks(cbWomenSub$name)
length(nameClean_m)
length(nameClean_w)
length(unique(nameClean_m))
length(unique(nameClean_w))
table(table(nameClean_m))
table(table(nameClean_w))

head( sort(table(nameClean_m), decreasing = TRUE), 1)
head( sort(table(nameClean_w), decreasing = TRUE), 1)

mSmith = cbMenSub[nameClean_m == "Michael Smith", ]
jJohnson = cbWomenSub[nameClean_w == "Jennifer Johnson", ]

head(unique(mSmith$home))
head(unique(jJohnson$home))

nameClean_m = tolower(nameClean_m)
nameClean_w = tolower(nameClean_w)

head( sort(table(nameClean_m), decreasing = TRUE), 1)
head( sort(table(nameClean_w), decreasing = TRUE), 1)
nameClean_m = gsub("[,.]", "", nameClean_m)
nameClean_w = gsub("[,.]", "", nameClean_w)
tabNameYr_m = table(cbMenSub$year, nameClean_m)
tabNameYr_w = table(cbWomenSub$year, nameClean_w)
max(tabNameYr_m)
max(tabNameYr_w)

indMax_m = which( tabNameYr_m == max(tabNameYr_m), arr.ind = TRUE )
colnames(tabNameYr_m)[indMax_m[2]]
cbMenSub$nameClean = nameClean_m

cbMenSub$yob = cbMenSub$year - cbMenSub$age

# Fix home in a similar way
homeClean = trimBlanks(tolower(cbMenSub$home))
cbMenSub$homeClean = gsub("[,.]", "", homeClean)

vars = c("year", "homeClean", "nameClean", "yob",  "runTime")
mb = which(nameClean_m == "michael brown")
birthOrder = order(cbMenSub$yob[mb])
cbMenSub[mb[birthOrder], vars]

cbMenSub$ID = paste(nameClean_m, cbMenSub$yob, sep = "_")

races = tapply(cbMenSub$year, cbMenSub$ID, length)

races8 = names(races)[which(races >= 8)]

men8 = cbMenSub[ cbMenSub$ID %in% races8, ]

orderByRunner = order(men8$ID, men8$year)
men8 = men8[orderByRunner, ]

men8L = split(men8, men8$ID)
names(men8L) = races8

length(unique(men8$ID))

gapTime = tapply(men8$runTime, men8$ID,
                 function(t) any(abs(diff(t)) > 20))

gapTime = sapply(men8L, function(df) 
  any(abs(diff(df$runTime)) > 20))

sum(gapTime)

lapply(men8L[ gapTime ][1:2], function(df) df[, vars])

homeLen = nchar(cbMenSub$homeClean)

cbMenSub$state = substr(cbMenSub$homeClean, 
                        start = homeLen - 1, stop = homeLen)

cbMenSub$state[cbMenSub$year == 2006] = NA

cbMenSub$ID = paste(cbMenSub$nameClean, cbMenSub$yob, 
                    cbMenSub$state, sep = "_")

numRaces = tapply(cbMenSub$year, cbMenSub$ID, length)
races8 = names(numRaces)[which(numRaces >= 8)]
men8 = cbMenSub[ cbMenSub$ID %in% races8, ]
orderByRunner = order(men8$ID, men8$year)
men8 = men8[orderByRunner, ]

men8L = split(men8, men8$ID)
names(men8L) = races8

length(races8)

groups = 1 + (1:length(men8L) %% 9)

addRunners = function(listRunners, colors, numLty) 
{
  numRunners = length(listRunners)
  colIndx = 1 + (1:numRunners) %% length(colors)
  ltys = rep(1:numLty, each = length(colors), length = numRunners)
  
  mapply(function(df, i) {      
    lines(df$runTime ~ df$age, 
          col = colors[colIndx[i]], lwd = 2, lty = ltys[i])
  }, listRunners, i = 1:numRunners) 
}

colors = c("#e41a1c", "#377eb8","#4daf4a", "#984ea3", 
           "#ff7f00", "#a65628")
par(mfrow = c(3, 3), mar = c(2, 2, 1, 1))
invisible(
  sapply(1:9, function(grpId){
    plot( x = 0, y = 0, type = "n",
          xlim = c(20, 80), ylim = c(50, 130),
          xlab = "Age (years)", ylab = "Run Time (minutes)")
    
    addRunners(men8L[ groups == grpId ], colors, numLty = 6)
  }) )

fitOne = function(oneRunner, addLine = FALSE, col = "grey") {
  lmOne = lm(runTime ~ age, data = oneRunner)
  if (addLine) 
    lines(x = oneRunner$age, y = predict(lmOne), 
          col = col, lwd = 2, lty = 2)
  
  ind = floor( (nrow(oneRunner) + 1) / 2)
  res = c(coefficients(lmOne)[2], oneRunner$age[ind],
          predict(lmOne)[ind])
  names(res) = c("ageCoeff", "medAge", "predRunTime")
  return(res)
}

par(mfrow = c(1, 1), mar = c(5, 4, 1, 1))

plot( x = 0, y = 0, type = "n",
      xlim = c(20, 80), ylim = c(50, 130),
      xlab = "Age (years)", ylab = "Run Time (minutes)")

addRunners(men8L[ groups == 9 ], colors, numLty = 6)
lapply(men8L[groups == 9], fitOne, addLine = TRUE, col = "black")

men8LongFit = lapply(men8L, fitOne)

coeffs = sapply(men8LongFit, "[", "ageCoeff" )
ages = sapply(men8LongFit, "[", "medAge")

longCoeffs = lm(coeffs ~ ages)

summary(longCoeffs)

pdf("CB_LongCoeffs.pdf", width = 10, height = 7)
oldPar = par(mar = c(4.1, 4.1, 1, 1))
plot(coeffs ~ ages, xlab = "Median Age (years)",
     ylab = "Coefficient (minutes per race / year)")
abline(longCoeffs, col = "#984ea3", lwd = 3)
abline(h = 0, col="blue", lwd = 3)
loCoeffs = loess(coeffs ~ ages)
ageV = min(ages):max(ages)
predV = predict(loCoeffs, new = data.frame(ages = ageV))
lines(x = ageV, y = predV, lwd = 3, lty = 2, col = "#4daf4a")
par(oldPar)
dev.off()
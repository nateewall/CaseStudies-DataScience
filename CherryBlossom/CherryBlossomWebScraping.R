library(XML)

# Loading data from html on page
ubase = "http://www.cherryblossom.org/"

menURLs = 
  c("results/1999/cb99m.html", "results/2000/cb003m.htm", 
    "results/2001/oof_m.html",
    "results/2002/oofm.htm", "results/2003/CB03-M.HTM",
    "results/2004/men.htm", "results/2005/CB05-M.htm", 
    "results/2006/men.htm", "results/2007/men.htm", 
    "results/2008/men.htm", "results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")

urls = paste(ubase, menURLs, sep = "")

extractResTable =
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.

  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL)
  {
    doc = htmlParse(url)
    
    if (year==1999) {
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]]
    }
    else if (year == 2000) {
      # Revise the HTML by line to remove the /font tags and force the parse to correctly infer where they belong
      html2000 = readLines("http://www.cherryblossom.org/results/2000/cb003m.htm")
      newhtml2000 = sub("</font>","",html2000)
      doc = htmlParse(newhtml2000)
      pres = getNodeSet(doc,"//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\n")[[1]] # Uses \n rather than \r\n.
    }
    else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    } 
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

years = 1999:2012
menTables = mapply(extractResTable, url = urls, year = years)
names(menTables) = years
sapply(menTables, length)

save(menTables, file = "CBMenTextTables.rda")

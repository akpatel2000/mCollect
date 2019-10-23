## Daily capture of Bloomberg Rates both Munis and Treasuries
## Dependents -- Proprietary MySQL database
# Change directory 
setwd("~/Applications/mCollect")

library(httr)
library(XML)
library(RSelenium)
library(lubridate)
library(RMySQL)
library(dbConnect)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(dplyr)


# Setup
system("docker run -d -p 4445:4444 selenium/standalone-chrome")
Sys.sleep(3)

# Start Selenium standalone server
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4445L
                      , browserName = "chrome")

# Open browser
remDr$open()

# navigate to webpage -- this may take some time if you done have 
# a headless browser
site <- 'https://www.bloomberg.com/markets/rates-bonds/government-bonds/us'
remDr$navigate(site) # navigates to webpage

### find the treasury table
elemMain <- remDr$findElements(using="class", value="data-table")
elem <- elemMain[[1]]
elem$highlightElement()
elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
bloombergGovies <- unlist(xpathApply(elemxml, '//td[@data-type="percent"]', xmlValue, 'data-table-row-cell__value'))
# convert to numeric value
bloombergGovies <- gsub("%", "", bloombergGovies)
bloombergGovies <- as.numeric(bloombergGovies)

treasuryYield3M <- bloombergGovies[1]
treasuryYield6M <- bloombergGovies[2]
treasuryYield1Y <- bloombergGovies[3]
treasuryYield2Y <- bloombergGovies[4]
treasuryYield5Y <- bloombergGovies[5]
treasuryYield10Y <- bloombergGovies[6]
treasuryYield30Y <- bloombergGovies[7]


### find the muni table
elem <- elemMain[[4]]
elem$highlightElement()
elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
bloombergAAA <- unlist(xpathApply(elemxml, '//td[@data-type="percent"]', xmlValue, 'data-table-row-cell__value'))
# convert to numeric value
bloombergAAA <- gsub("%", "", bloombergAAA)
bloombergAAA <- as.numeric(bloombergAAA)

muniYield1Y <- bloombergAAA[1]
muniYield2Y <- bloombergAAA[2]
muniYield5Y <- bloombergAAA[3]
muniYield10Y <- bloombergAAA[4]
muniYield30Y <- bloombergAAA[5]

###Check if muni and treasury record are valid by testing 10 year point
if (is.na(muniYield10Y)) {
    stop("Error muni record not good")
}

if (is.na(treasuryYield10Y)) {
    stop("Error treasury record is very bad Brian")
}

## Build record
date <- Sys.Date()
rateRecord <- data.frame(date, 
                         muniYield1Y, 
                         muniYield2Y, 
                         muniYield5Y, 
                         muniYield10Y, 
                         muniYield30Y,
                         treasuryYield3M, 
                         treasuryYield6M, 
                         treasuryYield1Y, 
                         treasuryYield2Y, 
                         treasuryYield5Y, 
                         treasuryYield10Y, 
                         treasuryYield30Y)


## Write to database and read from database the rates file
## append to flat file
if (nrow(rateRecord) > 0 ) {
    db1 <- dbConnect(MySQL(), user= "root", host = "localhost", db = "dbRates", password = "Newyork@1996")
    dbWriteTable(db1, rateRecord, name = "rates", append = TRUE, row.names = FALSE)
    dbDisconnect(db1)
    write.table(rateRecord, "db1.csv", sep = ",", col.names = !file.exists("db1.csv"), 
                append = T, 
                row.names = FALSE)
    message("rateRecord write succesful")
} else {
    stop("rateRecord not written -- rateRecord length is zero")
}

# Closing routine -- shutdown Rselenium
remDr$close()
system("docker stop $(docker ps -q)")

## Calculate spline curve from current yield curve
muniCurve <- spline(c(1,2,5,10,30), 
                    c(muniYield1Y, muniYield2Y, muniYield5Y, muniYield10Y, muniYield30Y), 
                    n=30, method = "natural")
muniCurve <- as.data.frame(muniCurve)
names(muniCurve) <- c("Maturity", "AAA_Yield")
muniCurve$AAA_Yield <- round(muniCurve$AAA_Yield, digits = 2)

print(muniCurve)

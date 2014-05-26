# file is in ~/Dropbox/Kiva/readingJSON.R
#
library(RJSONIO) 
setwd("~/Documents/Organizations/Bentley University/MA710/Lectures/Week 12 - Graph Databases")
baseDir = "/data/Kiva/Data"

getKivaData = function(aFile) {
  jsonBlob = fromJSON(aFile)
  someData = jsonBlob[[2]]
  invisible(someData)
}
loansFilelist = list.files(paste(baseDir,sep='/','loans'), full.names=TRUE)
lendersFilelist = list.files(paste(baseDir,sep='/','lenders'), full.names=TRUE)
loans_lendersFilelist = list.files(paste(baseDir,sep='/','loans_lenders'), full.names=TRUE)


jsonBlob = fromJSON("loans/110.json") # from the "loans" folder
metadata = jsonBlob[[1]] # metadata
loanData = jsonBlob[[2]] # the data
length(loanData) # 500 entries (probably 1 entry per loan, TBV)
aLoan = loanData[[1]] # the first entry
names(aLoan) # list of names (of fields)
aLoan$status # a character string
aLoan$loan_amount # numeric
aLoan$payments # a list of 12 payments
aPayment = aLoan$payments[[1]] # the first (payment) entry
str(aPayment) # list of fields and their (data) types

jsonBlob = fromJSON("lenders/222.json") # from the "lenders" folder
metadata = jsonBlob[[1]] # metadata
lenderData = jsonBlob[[2]] # the data
length(lenderData) # 1000 entries (probably 1 entry per lender, TBV)
aLender = lenderData[[1]] # the first entry
str(aLender) # list of fields and their types

jsonBlob = fromJSON('loans_lenders/100.json')
metadata = jsonBlob[[1]] # metadata
loans_lendersData = jsonBlob[[2]] # the data
length(loans_lendersData) # 1000 entries
aLink = loans_lendersData[[1]]
aLink
anotherLink = loans_lendersData[[2]]
anotherLink

library(parallel)
cl = makeCluster(20)
clusterExport(cl,'fromJSON')
stopCluster(cl)


loans_lendersList = 
  lapply(cl, loans_lendersFilelist, getKivaData);
loans_lendersUnlist = 
  unlist(loans_lendersList, recursive=FALSE)

lendersList = lapply(cl, lendersFilelist, getKivaData); 
lendersUnlist = unlist(lendersList, recursive=FALSE)

lenders_id = lapply(lendersUnlist, function(x) { x$lender_id })
lenders_idVector = as.vector(lenders_id)
names(lendersUnlist) = lenders_idVector
lendersUnlist[['matt']]$lender_id

loansList = lapply(cl, loansFilelist[1:100], getKivaData)
loansUnlist = unlist(loansList, recursive=FALSE)

loans_id = lapply(loansUnlist, function(x) { x$id })
loans_idVector = as.vector(loans_id)
names(loansUnlist) = loans_idVector
loansUnlist[['89']]$id

loans_lendersData[1:4]
loans_lendersData[[1]]


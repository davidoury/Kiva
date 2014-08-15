# Kiva project - dataprep.R
# Contains commands to read JSON data and create dataframes

library(RJSONIO) # fromJSON
library(plyr) # rbind.fill
library(parallel) # mclapply
library(ggplot2) # qplot

# Set the base directory which contains the 3 folders below
# which contain the data files.
#
baseDir = "/data/sets/Kiva"

# Create 3 vectors of data file names:
#   loansFilelist, lendersFilelist, loans_lendersFilelist
# one for each folder (loans, lenders, loans_lenders)
#
loansFilelist = 
  list.files(paste(baseDir,sep='/','loans'),
             full.names=TRUE)
lendersFilelist = 
  list.files(paste(baseDir,sep='/','lenders'), 
             full.names=TRUE)
loans_lendersFilelist = 
  list.files(paste(baseDir,sep='/','loans_lenders'), 
             full.names=TRUE)
loansFilelist[1:3] # Verify

# The fromJSON function of the RJSONIO package returns a list 
# created from the JSON contents of the input file
#
aList = fromJSON(loansFilelist[1])
names(aList) # This list has two elements: "header" and "loans"
# Explore the first element
names(aList$header)
aList$header
# Explore the second element
names(aList$loans) # There are no named elements
length(aList$loans) # There are 500 elements (which are loans)
names(aList$loans[[1]])

# The list.from.json function returns the list of loans 
# from its input file
#
list.from.json = function(aFile) {
  fromJSON(aFile, nullValue=NA)[[2]]
  # header data in the first item (ignored)
  # loan data in the second item (returned)
}
someLoans = list.from.json(loansFilelist[1])
names(someLoans)
length(someLoans)

# Explore the first loan
names(someLoans[[1]])
# Some elements are single numbers or strings
someLoans[[1]][c('id','name','status')]
# Some elements are lists with a fixed number of (named) elements
someLoans[[1]]$location
# Some elements are lists with a variable number of elements
sapply(someLoans[1:10], 
       function(x) { length(x$payments) })
length(someLoans[[1]]$payments)
length(someLoans[[10]]$payments)

# Function df.from.list creates a single row of a dataframe from a list
# input aList - the list of input data
# input somefields - the fields of aList to use in creating the dataframe row
#
df.from.list = function(aList,someFields) { 
  data.frame(rbind(unlist(aList[someFields])),
             stringsAsFactors=FALSE)
}
# For example, we create a dataframe row from aList$loans[[1]]$location
# using only the fields country_code, country and town
someLoans[[1]]$location
df.from.list(someLoans[[1]]$location, 
             c('country_code','country','town'))
# Another example, notice what happens when we include the geo field
df.from.list(someLoans[[1]]$location, 
             c('country_code','geo'))

# A better example, we create a dataframe row from someLoans[[1]] which
# is the data for the first loan using only those fields in keep.fields
keep.fields = c('id','partner_id','status','delinquent',
                'loan_amount','sector','activity','funded_date','location')
# Retrieve the fields of keep.fields from the first loan
someLoans[[1]][keep.fields]
# Now create a single row dataframe from these fields
df.from.list(someLoans[[1]],
             keep.fields)

# Use df.from.list with input from the first loans dataset file
loansFilelist[1] # the first file name of the loans dataset
length(list.from.json(loansFilelist[1])) # the number of loans in that file
names(list.from.json(loansFilelist[1])[[1]]) # the fields of the first loan
df.from.list(list.from.json(loansFilelist[1])[[1]],
             keep.fields) # the dataframe row for this first loan

# This last command is found in the innermost command below (line 147)

# Create aList with items which are the single row dataframes created
# from each each loan of the first file of the loans datasets
aList = 
  lapply(list.from.json(loansFilelist[1]), # read only the first datafile
         function(y) {df.from.list(y,keep.fields)})
length(aList) # 500 elements one for each loan
aList[1:3] # each element of the list is a dataframe row

# Function rbind.fill takes a list of dataframes and returns 
# a single dataframe with rows from the dataframes in the list
aDataframe =  
  rbind.fill(lapply(list.from.json(loansFilelist[1]), # read only the first file
                    function(y) {df.from.list(y,keep.fields)}))
aDataframe[1:3,]

anotherList = mclapply(X=loansFilelist[1:3], # read first three files
                       FUN=function(x) {
                         rbind.fill(
                           lapply(list.from.json(x),
                                  function(y) {df.from.list(y,keep.fields)}))},
                       mc.cores=detectCores())
length(anotherList)
sapply(anotherList, function(x) { class(x) })
sapply(anotherList, function(x) { dim(x) })
# anotherList contain 3 dataframes each with 500 rows and 14 columns
anotherDataframe =
  rbind.fill(
      mclapply(X=loansFilelist[1:3], # read first three files
               FUN=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          function(y) {df.from.list(y,keep.fields)}))},
               mc.cores=detectCores()))
class(anotherDataframe) # we have a data frame 
dim(anotherDataframe)   # with 1500=3*500 rows and 14 columns
# loans.df is created below using the above command but with all datafiles

## Create loans.df dataframe
#
system.time({ # 112s
  if(exists('loans.df')) rm(loans.df)
  loans.df = 
    rbind.fill(
      mclapply(X=loansFilelist,
               FUN=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          function(y) {df.from.list(y,keep.fields)}))},
               mc.cores=detectCores()))})
object.size(loans.df) # 150MB
# Notice that all fields are typed character. 
str(loans.df)
# This is important for speed and is a consequence of the stringsAsFactors
# parameter of the df.from.list function. Creating factor variables takes longer.
df.from.list

# Data transformations 
#
# setup proper types for the variables
# 
loans.df$id                    = as.numeric(loans.df$id)
loans.df$partner_id            = as.numeric(loans.df$partner_id) 
loans.df$loan_amount           = as.numeric(loans.df$loan_amount)
loans.df$delinquent[is.na(loans.df$delinquent)] = FALSE
loans.df$delinquent            = factor(loans.df$delinquent, labels=c('No','Yes'))
loans.df$status                = factor(loans.df$status)
loans.df$sector                = as.factor(loans.df$sector)
loans.df$location.country_code = as.factor(loans.df$location.country_code)
loans.df$funded_date           = as.Date(loans.df$funded_date)
# Create loan_size field as an ordered factor
loans.df$loan_size = 
  ordered(cut(loans.df$loan_amount,
              c(-Inf,500,1000,5000,Inf)),
          labels = c('Small','Medium','Large','Extra'))
# Delete these fields
loans.df$location.country = NULL
loans.df$location.town = NULL
loans.df$location.geo.level = NULL
loans.df$location.geo.pairs = NULL
loans.df$location.geo.type = NULL
str(loans.df) # Verify

# "One down one to go another town and one more show" --- Yes

# lenders.df
#
system.time({ # 50s
  if(exists('lenders.df')) rm(lenders.df)
  keep.fields = c('lender_id','country_code','loan_count','loan_because')
  lenders.df = 
    rbind.fill(
      mclapply(X=lendersFilelist, 
               FUN=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          function(y) {df.from.list(y,keep.fields)}))},
               mc.cores=detectCores()))})
str(lenders.df)
object.size(lenders.df)
lenders.df$lender_id =  factor(lenders.df$lender_id)
lenders.df$country_code = factor(lenders.df$country_code)
lenders.df$loan_count = as.numeric(lenders.df$loan_count)

#
# loans-lenders dataset
#
df.or.null = function(x) { 
  if(any(is.na(x))) NULL else data.frame(x,stringsAsFactors=FALSE) 
}
system.time({ # 32s
  if(exists('loans.lenders.df')) rm(loans.lenders.df)
  loans.lenders.df = 
    rbind.fill(
      mclapply(X=loans_lendersFilelist, 
               FUN=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          df.or.null))},
               mc.cores=detectCores()))})
str(loans.lenders.df)
object.size(loans.lenders.df)

# Save the three dataframes to disk
#
save(file="dataframes", 
     list=c('loans.df','lenders.df','loans.lenders.df'))

# DONE

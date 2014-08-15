library(RJSONIO) # fromJSON 2
library(plyr) # rbind.fill
#library(dplyr) # rbind.fill
library(parallel) # mclapply
library(ggplot2) # qplot
library(arules) # apriori (assocation rules)

baseDir = "/data/Kiva"

detectCores() # number of CPUs

cl = makeCluster(36) 
# stopCluster(cl) # Don't forget to run this before quiting RStudio.
clusterExport(cl, 'rbind.fill')
clusterExport(cl, 'fromJSON')


loansFilelist = 
  list.files(paste(baseDir,sep='/','loans'),
             full.names=TRUE)
lendersFilelist = 
  list.files(paste(baseDir,sep='/','lenders'), 
             full.names=TRUE)
loans_lendersFilelist = 
  list.files(paste(baseDir,sep='/','loans_lenders'), 
             full.names=TRUE)

list.from.json = function(aFile) {
    fromJSON(aFile, nullValue=NA)[[2]]
}
clusterExport(cl, 'list.from.json')
#library(data.table)
df.from.list = function(aList, fieldVector) { 
  data.frame(rbind(unlist(aList[fieldVector])),
             stringsAsFactors=FALSE)
}
# df.from.list = function(aList, fieldVector) { 
#   data.table(rbind(unlist(aList[fieldVector])),
#              stringsAsFactors=FALSE)
# }
clusterExport(cl, 'df.from.list')

# 
# Create and preprocess lenders.df
#
keep.fields = c('id','partner_id','status','delinquent',
                'loan_amount','sector','activity','funded_date','location')

if (0) # Do not execute this first command, use parLapply below
system.time({
  loans.df = 
    rbind.fill(
      mclapply(X=loansFilelist,
               FUN=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          function(y) {df.from.list(y,keep.fields)}))},
               mc.cores=detectCores()))})
# user  system elapsed 
# 710.650 108.370  87.661 

#clusterExport(cl, 'rbindlist')
#clusterExport(cl, 'data.table')
clusterExport(cl, 'keep.fields')
system.time({
  loans.df = 
    rbind.fill(
#     rbindlist(
      parLapply(cl=cl,
                X=loansFilelist,
                fun=function(x) {
                  rbind.fill(
#                   rbindlist(
                    lapply(list.from.json(x),
                           function(y) {df.from.list(y,keep.fields)}))}))})
# user  system elapsed 
# 13.710   3.570  70.259 
#stopCluster(cl) # Don't forget to run this before quiting RStudio.

loans.df$id = as.numeric(loans.df$id)
loans.df$partner_id = as.numeric(loans.df$partner_id) 
loans.df$loan_amount = as.numeric(loans.df$loan_amount)

loans.df$delinquent[is.na(loans.df$delinquent)] = FALSE

loans.df$status = as.factor(loans.df$status)
loans.df$activity = as.factor(loans.df$activity)
loans.df$delinquent = as.factor(loans.df$delinquent)
loans.df$sector = as.factor(loans.df$sector)
loans.df$location.country_code = as.factor(loans.df$location.country_code)

loans.df$funded_date = as.Date(loans.df$funded_date)

loans.df$loan_size = 
  ordered(cut(loans.df$loan_amount,
              c(-Inf,500,1000,5000,Inf)),
          labels = c('Small','Medium','Large','Extra'))

loans.df$location.country = NULL
loans.df$location.town = NULL
loans.df$location.geo.level = NULL
loans.df$location.geo.pairs = NULL
loans.df$location.geo.type = NULL

# 
# Create and preprocess lenders.df
#
keep.fields = c('lender_id','country_code','loan_count',
                'loan_because','member_since')

if (0) # Do not execute this first command, use parLapply below
system.time({
  lenders.df = 
    rbind.fill(
      mclapply(X=lendersFilelist,
               FUN=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          function(y) {df.from.list(y,keep.fields)}))},
               mc.cores=detectCores()))})
# user  system elapsed 
# 345.670  79.410  38.385 

clusterExport(cl, 'keep.fields')
system.time({
  lenders.df = 
    rbind.fill(
      parLapply(cl=cl,
               X=lendersFilelist,
               fun=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          function(y) {df.from.list(y,keep.fields)}))}))})
# user  system elapsed 
# 0.140   0.060  19.717 

lenders.df$lender_id =  factor(lenders.df$lender_id)
lenders.df$country_code = factor(lenders.df$country_code)
lenders.df$loan_count = as.numeric(lenders.df$loan_count)
lenders.df$loan_because = iconv(lenders.df$loan_because, to="UTF-8")
lenders.df$member_since = as.Date(lenders.df$member_since)

#
# Create and preprocess loans.lenders.df
#
df.or.null = function(x) { if(all(!is.na(x)))  data.frame(x) else NULL }
clusterExport(cl, 'df.or.null')

if (0) # Do not execute this first command, use parLapply below
system.time({
  loans.lenders.df = 
    rbind.fill(
      mclapply(X=loans_lendersFilelist, 
               FUN=function(x) {
                 rbind.fill(
                   lapply(list.from.json(x),
                          df.or.null))},
               mc.cores=detectCores()))})
# user  system elapsed - workspace with loans.df and lenders.df
# 534.120 451.950  54.261 
# user  system elapsed - minimal workspace
# 301.490 208.460  44.891 

system.time({
  loans.lenders.df = 
    rbind.fill(
      parLapply(cl=cl,
                X=loans_lendersFilelist, 
                fun=function(x) {
                  rbind.fill(
                    lapply(list.from.json(x),
                           df.or.null))}))})
# user  system elapsed 
# 14.830   1.810  29.049 

loans.lenders.df$lender_ids = factor(loans.lenders.df$lender_ids)

#
# Save all three dataframes to file <dataframes>
#
save(file="dataframes", 
     list=c('loans.df','lenders.df','loans.lenders.df'))

#
# Stop the cluster
#
stopCluster(cl) # Don't forget to run this before quiting RStudio.

library(RJSONIO) 
library(plyr)
library(cluster)
library(parallel)

baseDir = "/data/Kiva/Data"

loansFilelist = 
  list.files(paste(baseDir,sep='/','loans'),
             full.names=TRUE)
lendersFilelist = 
  list.files(paste(baseDir,sep='/','lenders'), 
             full.names=TRUE)
loans_lendersFilelist = 
  list.files(paste(baseDir,sep='/','loans_lenders'), 
             full.names=TRUE)

get.json.ds = function(aFile) {
  fromJSON(aFile, nullValue='unknown')[[2]]
  # meta data in the first item 
  # real data in the second item
}

rec.rbind.fill = function(a.df.list, rec.level=0) {
  n = length(a.df.list); 
  m = round(n/2); 
  if(n==1)
    a.df.list[[1]]
  else if(rec.level<=0)
    rbind(rec.rbind.fill(a.df.list[1    :m]),
          rec.rbind.fill(a.df.list[(m+1):n]))
  else 
    rbind.fill(mclapply(list(a.df.list[1    :m],
                             a.df.list[(m+1):n]),
                        function(a.df.list) { 
                          rec.rbind.fill(a.df.list, rec.level-1)},
                        mc.preschedule=FALSE))}

system.time({
<<<<<<< HEAD
  if(exists('loans.lenders.df')) rm(loans.lenders.df)
  loans.lenders.df = 
    rbind.fill(
      mclapply(X=loans_lendersFilelist, 
               FUN=function(x) {
                 rbind.fill(
                   lapply(get.json.ds(x),
                          data.frame))},
               mc.cores=detectCores()
               ))})
=======
  rm(loans.lenders.df)
  loans.lenders.df = 
    rbind.fill(
      mclapply(loans_lendersFilelist, 
               function(x) {
                 rbind.fill(
                   lapply(get.json.ds(x),
                          data.frame))},
               mc.cores=detectCores()))})
>>>>>>> 264d19c8cd434e653c4654e78766b59e6a61b33b
# Elapsed time: 26s (lapply, 20 files)
# Elapsed time: 36s (mclapply outside, all files)

# Before this works
<<<<<<< HEAD
# After this needs to be checked, reworked or tossed
=======
# After this needs to be checked
>>>>>>> 264d19c8cd434e653c4654e78766b59e6a61b33b

start_time = Sys.time()
loans.ds = 
  unlist(lapply(loansFilelist, get.json.ds),
         recursive=FALSE)
message('Elapsed time: ', Sys.time()-start_time)
# Elapsed time: 1.03 for loansFilelist[1:100]
# Elapsed time: approx 30 minutes for 1:1000 (all)
# rsession 24g res

start_time = Sys.time()
lenders.ds = 
  unlist(lapply(lendersFilelist, get.json.ds),
         recursive=FALSE)
message('Elapsed time: ', Sys.time()-start_time)
# Elapsed time: 
# rsession 3.5g res


aLoan = loans.ds[[1]] # the first entry
names(aLoan) # list of names (of fields)

aLoan$name
aLoan$id 

str(aLoan$location)

aLoan$sector
aLoan$activity

strwrap(aLoan$use)
strwrap(aLoan$description)

aLoan$status
aLoan$delinquent
aLoan$funded_amount
aLoan$paid_amount
aLoan$loan_amount

length(aLoan$payments)
aPayment = aLoan$payments[[1]]
str(aPayment)

as.POSIXct(aLoan$posted_date, tz='GMT')
as.POSIXct(aLoan$funded_date, tz='GMT')
as.POSIXct(aLoan$paid_date, tz='GMT')

aLender = lenders.ds[[1]] 
names(aLender) 

aLender$name
aLender$lender_id 
aLender$country_code
aLender$loan_count

aLender$inviter_id 
aLender$invitee_count

strwrap(aLender$whereabouts)
strwrap(aLender$loan_because)
strwrap(aLender$occupation)
strwrap(aLender$occupational_info)

as.POSIXct(aLender$member_since, tz='GMT')

aLoanLender = loans.lenders.ds[[1]] 
aLoanLender$id
aLoanLender$lender_ids

loans.ds.to.df = function (x) {
    data.frame(
      id=x$id,
      borrower.gender=x$borrowers[[1]]$gender,
      status=factor(x$status), 
      delinquent=x$delinquent,
      sector=x$sector, 
      activity=x$activity, 
      use=x$use, 
      country.code=x$location$country_code,
      desc.lang=x$description$languages,
      desc.text=x$description$text,
      repayment.interval=factor(x$terms$repayment_interval),
      loan.amount=x$loan_amount,
      funded.amount=x$funded_amount,
      paid.amount=x$paid_amount,
      posted.date=x$posted_date,
      funded.date=x$funded_date,
      paid.date=x$paid_date,
      #   lender.count=x$lender_count,
      borrower.count=length(x$borrowers),
      payments.numof=length(x$payments)
    )
  }
loans.df.list  = lapply(loans.ds, loans.ds.to.df)
loans.df      = ldply(loans.df.list, rbind)
str(loans.df)

lenders.ds.to.df = function (x) {
    data.frame(
      lender.id=x$lender_id,
      whereabouts=x$whereabouts, 
      country.code=factor(x$country_code),
      member.since=x$member_since,
      occupation=factor(x$occupation), 
      occupational.info=as.character(x$occupational_info), 
      loan.count=x$loan_count, 
      inviter.id=x$inviter_id, 
      invitee.count=x$invitee_count,
      stringsAsFactors=FALSE
    )
  }
lenders.df.list = lapply(lenders.ds, 
                         lenders.ds.to.df)
lenders.df      = ldply(lenders.df.list, 
                        rbind)
str(lenders.df)

loans.df$loan.size = 
  factor(ifelse(loans.df$loan.amount<=1000,
                'small',
                ifelse(loans.df$loan.amount<=2000,
                       'medium',
                       'large')))
table(loans.df$loan.size)

table(loans.df$repayment.interval)
table(loans.df$country.code)
table(loans.df$sector)



system.time({ loans.lenders.df.list = 
                lapply(loans.lenders.ds, as.data.frame) } )
loans.lenders.df      = ldply(loans.lenders.df.list, rbind)
str(loans.lenders.df)
system.time({ 
  loans.lenders.df = rbind.fill(loans.lenders.df.list[1:5000]) 
}) # 11s
system.time({ 
  loans.lenders.df = rbind.fill(loans.lenders.df.list[1:10000]) 
}) # 48s - not a good trend 2x data -> 4x time


system.time( { loans.lenders.df = 
                 rec.rbind.fill(loans.lenders.df.list, rec.level=4) } )
# user  system elapsed 
# 21.790  37.880  70.956 
system.time( { loans.lenders.df = 
                 rec.rbind.fill(loans.lenders.df.list, rec.level=0) } )
# user  system elapsed 
# 130.280   1.250 131.403 

library(arules)
cat.vars = c('status',
             'sector',
             'country.code',
             'loan.size',
             'repayment.interval')
num.vars = c('payments.numof',
             'borrow.count',
             #            'lender.count',
             'loan.amount')
rules = 
  apriori(loans.df[,cat.vars],
          parameter=list(supp=0.01, conf=0.5))

summary(rules)

rules.paid = 
  subset(rules, 
         subset = rhs %in% "status=paid" & lift > 1.0)
inspect(head(sort(rules.paid, by='lift'), 20))

rules.lift = subset(rules, subset = lift > 1.0)
inspect(head(sort(rules.lift, by='lift'), 10))
inspect(head(sort(rules.lift, by='confidence'), 10))

cat.vars = c('status',
               'sector',
               'country.code',
               'loan.size',
               'repayment.interval')
loans.dis = daisy(loans.df[,cat.vars])

loans.df[1:5,cat.vars]
daisy(loans.df[1:5,cat.vars])

loans.hclust = hclust(loans.dis)
plot(loans.hclust)

loans.hclust.cluster = 
  cutree(loans.hclust, h=0.75)
table(loans.hclust.cluster)

loans.hclust.cluster = cutree(loans.hclust, k=8)
table(loans.hclust.cluster)

loans.kmeans = kmeans(loans.dis,8)
loans.kmeans.cluster = loans.kmeans$cluster
table(loans.kmeans.cluster)

cat.agg = function (loans.df, n) {
    loans.agg = 
      aggregate(formula=count~., 
                data=cbind(count=1,
                           loans.df[,cat.vars]), 
                FUN=sum)
    agg.ndx = sort(loans.agg$count,
                   decreasing=TRUE,
                   index.return=TRUE)$ix
    loans.agg[agg.ndx[1:n],]
  }

cat.agg(loans.df[loans.kmeans.cluster==1,
                   cat.vars],5)
cat.agg(loans.df[loans.kmeans.cluster==2,
                 cat.vars],5)
cat.agg(loans.df[loans.kmeans.cluster==3,
                 cat.vars],5)
cat.agg(loans.df[loans.kmeans.cluster==4,
                 cat.vars],5)
cat.agg(loans.df[loans.kmeans.cluster==5,
                 cat.vars],5)
cat.agg(loans.df[loans.kmeans.cluster==6,
                 cat.vars],5)
cat.agg(loans.df[loans.kmeans.cluster==7,
                 cat.vars],5)
cat.agg(loans.df[loans.kmeans.cluster==8,
                 cat.vars],5)

library(rpart)
library(maptree)
loans.formula = status ~ loan.size+sector+repayment.interval
loans.rpart = 
rpart(data = loans.df, 
formula = loans.formula,
control = rpart.control(maxdepth=3)
)
print(loans.rpart)
draw.tree(loans.rpart, nodeinfo = TRUE)

loans.formula = status ~ loan.amount+borrower.count+payments.numof
loans.rpart = 
rpart(data = loans.df[,], 
formula = loans.formula,
control = rpart.control(maxdepth=3)
)
print(loans.rpart)
draw.tree(loans.rpart, nodeinfo = TRUE)

library(party)
loans.formula = status ~ sector+loan.size+borrower.count+payments.numof+sector+loan.size
loans.ctree = 
ctree(data = loans.df[1:1000,], 
formula = loans.formula,
control = ctree_control(maxdepth = 3)
)
plot(loans.ctree, type='simple')
plot(loans.ctree)


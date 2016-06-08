#Lab1
#Name: Naveen Kumar Duddala
#VOTES: 1/24/2016
#Including XML and Httr library
library(XML)
library(httr)
library(RCurl)
ElectionsURL = "http://elections.sos.state.tx.us/elchist233_state.htm"
Elections.table = readHTMLTable(ElectionsURL, header=T, which=1,stringsAsFactors=F)
write.csv(Elections.table, file = "ElectionsDemocratic.csv")

ElectionsURL = "http://elections.sos.state.tx.us/elchist273_state.htm"
Elections.table = readHTMLTable(ElectionsURL, header=T, which=1,stringsAsFactors=F)
write.csv(Elections.table, file = "ElectionsRepublican.csv")

#------------------------------------------------------------------------------
#Lab2
#Name: Naveen Kumar Duddala
#VOTES: 2/12/2016
library(gdata)  
library(xml2)
library(Hmisc)
FinalElectionReport = read.csv("FinalElection.csv")

xmlOutput <- ''
for (i in 1:100) { 
  xmlOutput <-paste0(xmlOutput,'<Elections>')
  for (j in 1:ncol(FinalElectionReport)) {
    xmlOutput <- paste0(xmlOutput,'<', colnames(FinalElectionReport)[j],'>', FinalElectionReport[i,j],'</',colnames(FinalElectionReport)[j],'>')
  }
  xmlOutput <-paste0(xmlOutput,'</Elections>')
}
fileConn<-file("FinalElectionReport.xml")
write(xmlOutput, fileConn)
close(fileConn)
#------------------------------------------------------------------------------
#Lab3
#Name: Naveen Kumar Duddala
#VOTES: 2/12/2016
library(gdata)  
library(xml2)
library(Hmisc)
FinalElectionReport = read.csv("FinalElection.csv")
summary(FinalElectionReport)

describe(FinalElectionReport)
plot(PERCENT~VOTES,data=FinalElectionReport)
plot(PERCENT~YEAR,data=FinalElectionReport)
plot(PERCENT~Precincts,data=FinalElectionReport)
plot(PERCENT~PrecinctsReporting,data=FinalElectionReport)
plot(PERCENT~PARTY..1.REP..2.DEM.,data=FinalElectionReport)


hist(FinalElectionReport$VOTES)
hist(FinalElectionReport$YEAR)
hist(FinalElectionReport$Precincts)
hist(FinalElectionReport$PrecinctsReporting)
hist(FinalElectionReport$PARTY..1.REP..2.DEM.)


#------------------------------------------------------------------------------
#Lab4
#Name: Naveen Kumar Duddala
#VOTES: 2/12/2016
library(plyr)
library(base)
FinalElectionReport <- read.csv("FinalElection.csv")
summary(FinalElectionReport)

FinalElectionReport[1:3,]


plot(PERCENT~VOTES,data=FinalElectionReport)
plot(PERCENT~YEAR,data=FinalElectionReport)
plot(PERCENT~Precincts,data=FinalElectionReport)
plot(PERCENT~PrecinctsReporting,data=FinalElectionReport)
plot(PERCENT~PARTY..1.REP..2.DEM.,data=FinalElectionReport)

## regression on all data
fit=lm(PERCENT ~YEAR+VOTES+Precincts+PrecinctsReporting+PARTY..1.REP..2.DEM.,data=FinalElectionReport)
summary(fit)
plot(PERCENT~PrecinctsReporting,data=FinalElectionReport)


FinalElectionReport[1:3,]

set.seed(1)
## fixing the seed value for the random selection guarantees the 
## same results in repeated runs
n=length(FinalElectionReport$PERCENT)
n1=100
n2=n-n1
train=sample(1:n,n1)

## regression on training set
m1=lm(PERCENT~YEAR+VOTES,data=FinalElectionReport[train,])
summary(m1)
pred=predict(m1,newdat=FinalElectionReport[-train,])
obs=FinalElectionReport$PERCENT[-train]
diff=obs-pred
percdiff=abs(diff)/obs
me=mean(diff)
rmse=sqrt(sum(diff**2)/n2)
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out)
n=length(FinalElectionReport$PERCENT)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m1=lm(PERCENT~YEAR+VOTES+Precincts+PrecinctsReporting+PARTY..1.REP..2.DEM.,data=FinalElectionReport[train,])
  pred=predict(m1,newdat=FinalElectionReport[-train,])
  obs=FinalElectionReport$PERCENT[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}

me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 


## cross-validation (leave one out): Model with just State
n=length(FinalElectionReport$PERCENT)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m1=lm(PERCENT~VOTES,data=FinalElectionReport[train,])
  pred=predict(m1,newdat=FinalElectionReport[-train,])
  obs=FinalElectionReport$PERCENT[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## Adding the squares of State and Transportation Phase to the model
FinalElectionReport$VOTES2=FinalElectionReport$VOTES^2
FinalElectionReport$Precincts2=FinalElectionReport$Precincts^2
m11=lm(PERCENT~VOTES+Precincts,data=FinalElectionReport)
summary(m11)
m12=lm(PERCENT~VOTES+VOTES2+Precincts+Precincts2,data=FinalElectionReport)
summary(m12)
m13=lm(PERCENT~VOTES+VOTES2+Precincts,data=FinalElectionReport)
summary(m13)
plot(m11$res~m11$fitted)
hist(m11$res)
plot(m12$res~m12$fitted)



# Logistic Regression with quasipoisson
LRFinalElectionReport <- glm(PERCENT~YEAR+VOTES+Precincts+PrecinctsReporting+PARTY..1.REP..2.DEM.,data=FinalElectionReport,family= quasipoisson())
summary(LRFinalElectionReport) # display results
confint(LRFinalElectionReport) # 95% CI for the coefficients
exp(coef(LRFinalElectionReport)) # exponentiated coefficients
exp(confint(LRFinalElectionReport)) # 95% CI for exponentiated coefficients
predict(LRFinalElectionReport, type="response") # predicted values

resOutput <- residuals(LRFinalElectionReport, type="deviance") # residuals
plot(LRFinalElectionReport)
plot(predict(LRFinalElectionReport),resOutput, xlab="Fitted values", ylab = "Residuals",ylim = max(abs(resOutput)) * c(0,1))
abline(h=0.6,lty=2,col="grey")
glm.out = glm(PERCENT~YEAR+VOTES+Precincts+PrecinctsReporting+PARTY..1.REP..2.DEM.,data=FinalElectionReport)
anova(glm.out, test="Chisq")
1 - pchisq(6, df=7)
#------------------------------------------------------------------------------
#Lab5
#Name: Naveen Kumar Duddala
#VOTES: 2/13/2016
FinalElectionReport <- read.csv("FinalElection.csv")
FinalElectionReport
library(party)
fit <- ctree(PERCENT~YEAR+VOTES+Precincts+PrecinctsReporting+PARTY..1.REP..2.DEM.,data=na.omit(FinalElectionReport))
plot(fit, main="Conditional Inference Tree for VOTES")
## Plot what we end up with
plot(FinalElectionReport[,c("VOTES","Precincts")],cex=0.02*exp(FinalElectionReport$PERCENT))
abline(v=8098, col=41.001, lwd=2)
lines(x=c(-200,300), y=c(300,300), col=6, lwd=2)




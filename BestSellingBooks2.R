#------------------------------------------------------------------------------
#Lab2
#Name: 
#ratingcount: 2/12/2016
library(gdata)  
library(xml2)
library(Hmisc)
BestBooks = read.csv("book1111.csv")

xmlOutput <- ''
for (i in 1:100) { 
  xmlOutput <-paste0(xmlOutput,'<BestSellingBooks>')
  for (j in 1:ncol(BestBooks)) {
    xmlOutput <- paste0(xmlOutput,'<', colnames(BestBooks)[j],'>', BestBooks[i,j],'</',colnames(BestBooks)[j],'>')
  }
  xmlOutput <-paste0(xmlOutput,'</BestSellingBooks>')
}
fileConn<-file("BestBooks.xml")
write(xmlOutput, fileConn)
close(fileConn)
#------------------------------------------------------------------------------
#Lab3
#Name: 
#ratingcount: 2/12/2016
library(gdata)  
library(xml2)
library(Hmisc)
BestBooks = read.csv("book1111.csv")
summary(BestBooks)
describe(BestBooks)

plot(ratingvalue~.,data=BestBooks)

#converting categiorical data into numerical data
BestBooks.edition<-factor(BestBooks$edition)
class(BestBooks.edition)
editionInteger<- unclass(BestBooks.edition)
editionInteger
BestBooks<-cbind(BestBooks,editionInteger)

#converting categiorical data into numerical data
BestBooks.bookname<-factor(BestBooks$bookname)
class(BestBooks.bookname)
booknameInteger<- unclass(BestBooks.bookname)
booknameInteger
BestBooks<-cbind(BestBooks,booknameInteger)

#converting categiorical data into numerical data
BestBooks.author<-factor(BestBooks$author)
class(BestBooks.author)
authorInteger<- unclass(BestBooks.author)
authorInteger
BestBooks<-cbind(BestBooks,authorInteger)

hist(BestBooks$ratingcount)
hist(BestBooks$reviewvalue)
hist(BestBooks$noofpages)
hist(BestBooks$isbn)
hist(BestBooks$editionInteger)
hist(BestBooks$booknameInteger)
hist(BestBooks$authorInteger)


#------------------------------------------------------------------------------
#Lab4
#Name: 
#ratingcount: 2/12/2016
library(plyr)
library(base)

summary(BestBooks)

BestBooks[1:3,]

hist(BestBooks$reviewvalue)
plot(ratingvalue~reviewvalue,data=BestBooks)
plot(ratingvalue~noofpages,data=BestBooks)
plot(ratingvalue~isbn,data=BestBooks)
plot(ratingvalue~editionInteger,data=BestBooks)
plot(ratingvalue~booknameInteger,data=BestBooks)
plot(ratingvalue~authorInteger,data=BestBooks)


## regression on all data
fit = lm(ratingvalue ~ratingcount+reviewvalue+noofpages,data=BestBooks)
summary(fit)
plot(ratingvalue~ratingcount,data=BestBooks)
abline(fit, col="red")

BestBooks[1:3,]

set.seed(1)
## fixing the seed value for the random selection guarantees the 
## same results in repeated runs
n=800
n1=400
n2=n-n1
train=sample(1:n,n1)

## cross-validation (leave one out) for the model on all six regressors
n=length(BestBooks$ratingcount)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  ## the R expression "train1[train1!=k]" picks from train1 those 
  ## elements that are different from k and stores those elements in the
  ## object train. 
  ## For k=1, train consists of elements that are different from 1; that 
  ## is 2, 3, ., n.
  m1=lm(ratingvalue~ratingcount+reviewvalue,data=BestBooks[train,])
  pred=predict(m1,newdat=BestBooks[-train,])
  obs=BestBooks$ratingcount[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out) for the model on weight only
n=length(BestBooks$ratingcount)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(ratingvalue~reviewvalue,data=BestBooks[train,])
  pred=predict(m2,newdat=BestBooks[-train,])
  obs=BestBooks$ratingcount[-train]
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
BestBooks$ratingcount2=BestBooks$ratingcount^2
BestBooks$noofpages2=BestBooks$noofpages^2
m11=lm(ratingvalue~ratingcount+noofpages,data=BestBooks)
summary(m11)
m12=lm(ratingvalue~ratingcount+ratingcount2+noofpages+noofpages2,data=BestBooks)
summary(m12)
m13=lm(ratingvalue~ratingcount+ratingcount2+noofpages,data=BestBooks)
summary(m13)
plot(m11$res~m11$fitted)
hist(m11$res)
plot(m12$res~m12$fitted)



# Logistic Regression with quasipoisson
LRBestBooks <- glm(ratingvalue~ratingcount+reviewvalue+noofpages,data=BestBooks,family= quasipoisson())
summary(LRBestBooks) # display results
confint(LRBestBooks) # 95% CI for the coefficients
exp(coef(LRBestBooks)) # exponentiated coefficients
exp(confint(LRBestBooks)) # 95% CI for exponentiated coefficients
predict(LRBestBooks, type="response") # predicted values

resOutput <- residuals(LRBestBooks, type="deviance") # residuals
plot(LRBestBooks)
plot(predict(LRBestBooks),resOutput, xlab="Fitted values", ylab = "Residuals",ylim = max(abs(resOutput)) * c(0,0.01))
abline(h=0.002,lty=2,col="grey")


#------------------------------------------------------------------------------
#Lab5
#Name: 
#ratingcount: 2/13/2016
BestBooks <- read.csv("book1111.csv")
BestBooks
library(party)
fit <- ctree(ratingvalue~reviewvalue+ratingcount+noofpages,data=na.omit(BestBooks))
plot(fit, main="Conditional Inference Tree for ratingvalue")
## Plot what we end up with
plot(BestBooks[,c("ratingcount","reviewvalue")],cex=0.1*exp(BestBooks$ratingvalue))
abline(v=50000, col=50.001, lwd=2)
lines(x=c(0,500001), y=c(450000,500001), col=6, lwd=2)

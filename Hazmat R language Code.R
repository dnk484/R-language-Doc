#Lab1
#Name: Naveen Kumar Duddala
#Date: 1/24/2016
#Including XML and Httr library
library(XML)
library(httr)

#URLfrom http://www.phmsa.dot.gov/ showning Gas Distribution assigned to GasURL
GasURL = "http://www.phmsa.dot.gov/pipeline/library/data-stats/annual-report-mileage-for-gas-distribution-systems"
#To get webpage context and assign to response
GasWebage <- GET(GasURL)

#URL tags div is assigned to xURLTags
xURLTags <- '<div align="left"><a href="/pipeline/library/data-stats/annual-report-mileage-for-hazardous-liquid-or-carbon-dioxide-systems">
Hazardous Liquid</a> | <strong>Gas Distribution</strong> | 
<a href="/pipeline/library/data-stats/annual-report-mileage-for-natural-gas-transmission-and-gathering-systems">Gas Transmission Gathering</a> |
<a href="/pipeline/library/data-stats/liquefied-natural-gas-lng-facilities-and-total-storage-capacities">Liquefied Natural Gas</a><br />
&#160;</div>
'

library(XML)
#xmlParse to parse the HTML tags and assign to scrappedHTML
scrappedHTML <- xmlParse(xURLTags)

#Collecting URLs from above HTML code and assign to hrefs
links <- xpathSApply(scrappedHTML, "//div/a", xmlGetAttr, 'href')

#Read table of Gas Distribution
phmsaGas1.table = readHTMLTable(GasURL, header=T, which=6,stringsAsFactors=F)
#Saving Gas Distribution data to Phmsa-Gas1.csv
write.csv(phmsaGas1.table, file = "Phmsa-Gas1.csv")

mainUrl <- "http://www.phmsa.dot.gov/"
#Gas Transmission Gathering page URL
FinalUrl <- paste0(mainUrl,links[2])
#Read table of Gas Transmission Gathering
phmsaGas2.table = readHTMLTable(FinalUrl, header=T, which=6,stringsAsFactors=F)
#Saving Gas Transmission Gathering to Phmsa-Gas2.csv
write.csv(phmsaGas2.table, file = "Phmsa-Gas2.csv")

#Liquefied Natural Gas page URL
FinalUrl <- paste0(mainUrl,links[3])
#Read table of Saving Liquefied Natural Gas
phmsaGas3.table = readHTMLTable(FinalUrl, header=T, which=6,stringsAsFactors=F)
#Saving Liquefied Natural Gas to Phmsa-Gas3.csv
write.csv(phmsaGas3.table, file = "Phmsa-Gas3.csv")



#------------------------------------------------------------------------------
#Lab2
#Name: Naveen Kumar Duddala
#Date: 2/12/2016
library(gdata)  
library(xml2)
library(Hmisc)
DamageIncidentReport = read.csv("10_years_PHMSA.csv")

xmlOutput <- ''
for (i in 1:1000) { #nrow(DamageIncidentReport)) 
  xmlOutput <-paste0(xmlOutput,'<Damage>')
  for (j in 1:ncol(DamageIncidentReport)) {
    xmlOutput <- paste0(xmlOutput,'<', colnames(DamageIncidentReport)[j],'>', DamageIncidentReport[i,j],'</',colnames(DamageIncidentReport)[j],'>')
  }
  xmlOutput <-paste0(xmlOutput,'</Damage>')
}
fileConn<-file("IncidentReportOutput.xml")
write(xmlOutput, fileConn)
close(fileConn)



#------------------------------------------------------------------------------
#Lab3
#Name: Naveen Kumar Duddala
#Date: 2/12/2016
library(gdata)  
library(xml2)
library(Hmisc)
IncidentReportSummary = read.csv("10_years_PHMSA.csv")
summary(IncidentReportSummary)


describe(IncidentReportSummary)
plot(Total.Damages~Date,data=IncidentReportSummary)
plot(Total.Damages~Month,data=IncidentReportSummary)
plot(Total.Damages~Year,data=IncidentReportSummary)
plot(Total.Damages~State,data=IncidentReportSummary)
plot(Total.Damages~Mode.Of.Transportation,data=IncidentReportSummary)
plot(Total.Damages~Transportation.Phase,data=IncidentReportSummary)


IncidentReportSummary$Transportation.Phase <- ifelse(IncidentReportSummary$Transportation.Phase == "IN TRANSIT", 1, ifelse(IncidentReportSummary$Transportation.Phase == "IN TRANSIT STORAGE", 2, ifelse(IncidentReportSummary$Transportation.Phase == "LOADING", 3, ifelse(IncidentReportSummary$Transportation.Phase == "UNLOADING", 4, 5))))
IncidentReportSummary$Mode.Of.Transportation <- ifelse(IncidentReportSummary$Mode.Of.Transportation == "FAA-AIR", 1, ifelse(IncidentReportSummary$Mode.Of.Transportation == "FMCSA-HIGHWAY", 2, ifelse(IncidentReportSummary$Mode.Of.Transportation == "FRA-RAILWAY", 3, ifelse(IncidentReportSummary$Mode.Of.Transportation == "USCG-WATER", 4, 5))))
IncidentReportSummary$State <- ifelse(IncidentReportSummary$State == "AK", 1, ifelse(IncidentReportSummary$State == "AL", 2, ifelse(IncidentReportSummary$State == "AR", 3, ifelse(IncidentReportSummary$State == "AZ", 4,ifelse(IncidentReportSummary$State == "CA", 5,ifelse(IncidentReportSummary$State == "CO", 6,ifelse(IncidentReportSummary$State == "CT", 7,ifelse(IncidentReportSummary$State == "DC", 8,ifelse(IncidentReportSummary$State == "DE", 9,ifelse(IncidentReportSummary$State == "FL", 10,ifelse(IncidentReportSummary$State == "GA", 11,ifelse(IncidentReportSummary$State == "GU", 12,ifelse(IncidentReportSummary$State == "HI", 13,ifelse(IncidentReportSummary$State == "IA", 14,ifelse(IncidentReportSummary$State == "ID", 15,ifelse(IncidentReportSummary$State == "IL", 16,ifelse(IncidentReportSummary$State == "IN", 17,ifelse(IncidentReportSummary$State == "KS", 18,ifelse(IncidentReportSummary$State == "KY", 19,ifelse(IncidentReportSummary$State == "LA", 20,ifelse(IncidentReportSummary$State == "MA", 21,ifelse(IncidentReportSummary$State == "MD", 22,ifelse(IncidentReportSummary$State == "ME", 23,ifelse(IncidentReportSummary$State == "MI", 24,ifelse(IncidentReportSummary$State == "MN", 25,ifelse(IncidentReportSummary$State == "MO", 26,ifelse(IncidentReportSummary$State == "MS", 27,ifelse(IncidentReportSummary$State == "MT", 28,ifelse(IncidentReportSummary$State == "NC", 29,ifelse(IncidentReportSummary$State == "ND", 30,ifelse(IncidentReportSummary$State == "NE", 31,ifelse(IncidentReportSummary$State == "NH", 32,ifelse(IncidentReportSummary$State == "NJ", 33,ifelse(IncidentReportSummary$State == "NM", 34,ifelse(IncidentReportSummary$State == "NV", 35,ifelse(IncidentReportSummary$State == "NY", 36,ifelse(IncidentReportSummary$State == "OH", 37,ifelse(IncidentReportSummary$State == "OK", 38,ifelse(IncidentReportSummary$State == "OR", 39,ifelse(IncidentReportSummary$State == "PA", 40,ifelse(IncidentReportSummary$State == "PR", 41,ifelse(IncidentReportSummary$State == "RI", 42,ifelse(IncidentReportSummary$State == "SC", 43,ifelse(IncidentReportSummary$State == "SD", 44,ifelse(IncidentReportSummary$State == "TN", 45,ifelse(IncidentReportSummary$State == "TX", 46,ifelse(IncidentReportSummary$State == "UT", 47,ifelse(IncidentReportSummary$State == "VA", 48,ifelse(IncidentReportSummary$State == "VI", 49,ifelse(IncidentReportSummary$State == "VT", 50,51))))))))))))))))))))))))))))))))))))))))))))))))))
IncidentReportSummary[1:3,]

hist(IncidentReportSummary$Date)
hist(IncidentReportSummary$Month)
hist(IncidentReportSummary$Year)
hist(IncidentReportSummary$State)
hist(IncidentReportSummary$Mode.Of.Transportation)
hist(IncidentReportSummary$Transportation.Phase)



#------------------------------------------------------------------------------
#Lab4
#Name: Naveen Kumar Duddala
#Date: 2/12/2016
library(plyr)
library(base)
IncidentReport <- read.csv("10_years_PHMSA.csv")
summary(IncidentReport)


IncidentReport$Transportation.Phase <- ifelse(IncidentReport$Transportation.Phase == "IN TRANSIT", 1, ifelse(IncidentReport$Transportation.Phase == "IN TRANSIT STORAGE", 2, ifelse(IncidentReport$Transportation.Phase == "LOADING", 3, ifelse(IncidentReport$Transportation.Phase == "UNLOADING", 4, 5))))
IncidentReport$Mode.Of.Transportation <- ifelse(IncidentReport$Mode.Of.Transportation == "FAA-AIR", 1, ifelse(IncidentReport$Mode.Of.Transportation == "FMCSA-HIGHWAY", 2, ifelse(IncidentReport$Mode.Of.Transportation == "FRA-RAILWAY", 3, ifelse(IncidentReport$Mode.Of.Transportation == "USCG-WATER", 4, 5))))
IncidentReport$State <- ifelse(IncidentReport$State == "AK", 1, ifelse(IncidentReport$State == "AL", 2, ifelse(IncidentReport$State == "AR", 3, ifelse(IncidentReport$State == "AZ", 4,ifelse(IncidentReport$State == "CA", 5,ifelse(IncidentReport$State == "CO", 6,ifelse(IncidentReport$State == "CT", 7,ifelse(IncidentReport$State == "DC", 8,ifelse(IncidentReport$State == "DE", 9,ifelse(IncidentReport$State == "FL", 10,ifelse(IncidentReport$State == "GA", 11,ifelse(IncidentReport$State == "GU", 12,ifelse(IncidentReport$State == "HI", 13,ifelse(IncidentReport$State == "IA", 14,ifelse(IncidentReport$State == "ID", 15,ifelse(IncidentReport$State == "IL", 16,ifelse(IncidentReport$State == "IN", 17,ifelse(IncidentReport$State == "KS", 18,ifelse(IncidentReport$State == "KY", 19,ifelse(IncidentReport$State == "LA", 20,ifelse(IncidentReport$State == "MA", 21,ifelse(IncidentReport$State == "MD", 22,ifelse(IncidentReport$State == "ME", 23,ifelse(IncidentReport$State == "MI", 24,ifelse(IncidentReport$State == "MN", 25,ifelse(IncidentReport$State == "MO", 26,ifelse(IncidentReport$State == "MS", 27,ifelse(IncidentReport$State == "MT", 28,ifelse(IncidentReport$State == "NC", 29,ifelse(IncidentReport$State == "ND", 30,ifelse(IncidentReport$State == "NE", 31,ifelse(IncidentReport$State == "NH", 32,ifelse(IncidentReport$State == "NJ", 33,ifelse(IncidentReport$State == "NM", 34,ifelse(IncidentReport$State == "NV", 35,ifelse(IncidentReport$State == "NY", 36,ifelse(IncidentReport$State == "OH", 37,ifelse(IncidentReport$State == "OK", 38,ifelse(IncidentReport$State == "OR", 39,ifelse(IncidentReport$State == "PA", 40,ifelse(IncidentReport$State == "PR", 41,ifelse(IncidentReport$State == "RI", 42,ifelse(IncidentReport$State == "SC", 43,ifelse(IncidentReport$State == "SD", 44,ifelse(IncidentReport$State == "TN", 45,ifelse(IncidentReport$State == "TX", 46,ifelse(IncidentReport$State == "UT", 47,ifelse(IncidentReport$State == "VA", 48,ifelse(IncidentReport$State == "VI", 49,ifelse(IncidentReport$State == "VT", 50,51))))))))))))))))))))))))))))))))))))))))))))))))))
IncidentReport[1:3,]


plot(Total.Damages~Date,data=IncidentReport)
plot(Total.Damages~Month,data=IncidentReport)
plot(Total.Damages~Year,data=IncidentReport)
plot(Total.Damages~State,data=IncidentReport)
plot(Total.Damages~Mode.Of.Transportation,data=IncidentReport)
plot(Total.Damages~Transportation.Phase,data=IncidentReport)


## regression on all data
fit=lm(Year ~Total.Damages,data=IncidentReport)
summary(fit)
plot(IncidentReport$Total.Damages~IncidentReport$Year, main="Total Damages Reports")
abline(fit, col="red")

IncidentReport[1:3,]

set.seed(1)
## fixing the seed value for the random selection guarantees the 
## same results in repeated runs
n=length(IncidentReport$Total.Damages)
n1=1000
n2=n-n1
train=sample(1:n,n1)

## regression on training set
m1=lm(Total.Damages~Month+Year+Date+State+Mode.Of.Transportation+Transportation.Phase,data=IncidentReport[train,])
summary(m1)
pred=predict(m1,newdat=IncidentReport[-train,])
obs=IncidentReport$Total.Damages[-train]
diff=obs-pred
percdiff=abs(diff)/obs
me=mean(diff)
rmse=sqrt(sum(diff**2)/n2)
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out)
n=length(IncidentReport$Total.Damages)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m1=lm(Total.Damages~Month+Year+Date+State+Mode.Of.Transportation+Transportation.Phase,data=IncidentReport[train,])
  pred=predict(m1,newdat=IncidentReport[-train,])
  obs=IncidentReport$Total.Damages[-train]
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
n=length(IncidentReport$Total.Damages)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m1=lm(Total.Damages~State,data=IncidentReport[train,])
  pred=predict(m1,newdat=IncidentReport[-train,])
  obs=IncidentReport$Total.Damages[-train]
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
IncidentReport$State2=IncidentReport$State^2
IncidentReport$Transportation.Phase2=IncidentReport$Transportation.Phase^2
m11=lm(Total.Damages~State+Transportation.Phase,data=IncidentReport)
summary(m11)
m12=lm(Total.Damages~State+State2+Transportation.Phase+Transportation.Phase2,data=IncidentReport)
summary(m12)
m13=lm(Total.Damages~State+State2+Transportation.Phase,data=IncidentReport)
summary(m13)
plot(m11$res~m11$fitted)
hist(m11$res)
plot(m12$res~m12$fitted)



# Logistic Regression with quasipoisson
LRIncidentReport <- glm(Total.Damages~Month+Year+Date+State+Mode.Of.Transportation+Transportation.Phase,data=IncidentReport,family= quasipoisson())
summary(LRIncidentReport) # display results
confint(LRIncidentReport) # 95% CI for the coefficients
exp(coef(LRIncidentReport)) # exponentiated coefficients
exp(confint(LRIncidentReport)) # 95% CI for exponentiated coefficients
predict(LRIncidentReport, type="response") # predicted values

resOutput <- residuals(LRIncidentReport, type="deviance") # residuals
plot(LRIncidentReport)
plot(predict(LRIncidentReport),resOutput, xlab="Fitted values", ylab = "Residuals",ylim = max(abs(resOutput)) * c(0,0.01))

abline(h=75,lty=2,col="grey")
glm.out = glm(Total.Damages~State*Mode.Of.Transportation*Transportation.Phase,data=IncidentReport)
anova(glm.out, test="Chisq")
1 - pchisq(6, df=7)



#------------------------------------------------------------------------------
#Lab5
#Name: Naveen Kumar Duddala
#Date: 2/13/2016
IncidentReport <- read.csv("10_years_PHMSA.csv")
IncidentReport
library(tree)

IncidentReport$Transportation.Phase <- ifelse(IncidentReport$Transportation.Phase == "IN TRANSIT", 1, ifelse(IncidentReport$Transportation.Phase == "IN TRANSIT STORAGE", 2, ifelse(IncidentReport$Transportation.Phase == "LOADING", 3, ifelse(IncidentReport$Transportation.Phase == "UNLOADING", 4, 5))))
IncidentReport$Mode.Of.Transportation <- ifelse(IncidentReport$Mode.Of.Transportation == "FAA-AIR", 1, ifelse(IncidentReport$Mode.Of.Transportation == "FMCSA-HIGHWAY", 2, ifelse(IncidentReport$Mode.Of.Transportation == "FRA-RAILWAY", 3, ifelse(IncidentReport$Mode.Of.Transportation == "USCG-WATER", 4, 5))))
IncidentReport$State <- ifelse(IncidentReport$State == "AK", 1, ifelse(IncidentReport$State == "AL", 2, ifelse(IncidentReport$State == "AR", 3, ifelse(IncidentReport$State == "AZ", 4,ifelse(IncidentReport$State == "CA", 5,ifelse(IncidentReport$State == "CO", 6,ifelse(IncidentReport$State == "CT", 7,ifelse(IncidentReport$State == "DC", 8,ifelse(IncidentReport$State == "DE", 9,ifelse(IncidentReport$State == "FL", 10,ifelse(IncidentReport$State == "GA", 11,ifelse(IncidentReport$State == "GU", 12,ifelse(IncidentReport$State == "HI", 13,ifelse(IncidentReport$State == "IA", 14,ifelse(IncidentReport$State == "ID", 15,ifelse(IncidentReport$State == "IL", 16,ifelse(IncidentReport$State == "IN", 17,ifelse(IncidentReport$State == "KS", 18,ifelse(IncidentReport$State == "KY", 19,ifelse(IncidentReport$State == "LA", 20,ifelse(IncidentReport$State == "MA", 21,ifelse(IncidentReport$State == "MD", 22,ifelse(IncidentReport$State == "ME", 23,ifelse(IncidentReport$State == "MI", 24,ifelse(IncidentReport$State == "MN", 25,ifelse(IncidentReport$State == "MO", 26,ifelse(IncidentReport$State == "MS", 27,ifelse(IncidentReport$State == "MT", 28,ifelse(IncidentReport$State == "NC", 29,ifelse(IncidentReport$State == "ND", 30,ifelse(IncidentReport$State == "NE", 31,0)))))))))))))))))))))))))))))))

library(party)
fit <- ctree(Total.Damages~Month+Year+Date+State+Mode.Of.Transportation+Transportation.Phase,data=na.omit(IncidentReport))
plot(fit, main="Conditional Inference Tree for Total Damages")
## Plot what we end up with
plot(IncidentReport[,c("Transportation.Phase","State")],cex=0.02*exp(IncidentReport$Total.Damages))
abline(v=1.5, col=4, lwd=2)
lines(x=c(-2,1.5), y=c(2,15), col=4, lwd=2)



#------------------------------------------------------------------------------
#Lab6
#Name: Naveen Kumar Duddala
#Date: 2/20/2016

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)   

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")    

cname <- file.path("C:", "text")   
cname   
dir(cname)  

library(tm)   
docs <- Corpus(DirSource(cname))   

## Preprocessing      
docs <- tm_map(docs, removePunctuation)   # *Removing punctuation:*    
docs <- tm_map(docs, removeNumbers)      # *Removing numbers:*    
docs <- tm_map(docs, tolower)   # *Converting to lowercase:*    
docs <- tm_map(docs, removeWords, stopwords("english"))   # *Removing "stopwords" 
library(SnowballC)   
docs <- tm_map(docs, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")   
docs <- tm_map(docs, stripWhitespace)   # *Stripping whitespace   
docs <- tm_map(docs, PlainTextDocument)   
## *This is the end of the preprocessing stage.*   


### Stage the Data      
dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)   

### Explore your data      
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix.csv")   
### FOCUS - on just the interesting stuff...   
#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
### Word Frequency   
head(table(freq), 20)   
# The above output is two rows of numbers. The top number is the frequency with which 
# words appear and the bottom number reflects how many words appear that frequently. 
#
tail(table(freq), 20)   
# Considering only the 20 greatest frequencies
#
# **View a table of the terms after removing sparse terms, as above.
freq <- colSums(as.matrix(dtms))   
freq   
# The above matrix was created using a data transformation we made earlier. 
# **An alternate view of term frequency:**   
# This will identify all terms that appear frequently (in this case, 50 or more times).   
findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your data.
#
#
#   
### Plot Word Frequencies
# **Plot words that appear at least 50 times.**   
library(ggplot2)   
wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   
#  
## Relationships Between Terms
### Term Correlations
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.    
findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98   
# 
# Change "question" & "analysi" to terms that actually appear in your texts.
# Also adjust the `corlimit= ` to any value you feel is necessary.
#
# 
### Word Clouds!   
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)    

### Clustering by Term Similarity

### Hierarchal Clustering   
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="ward")   
plot.new()
plot(fit, hang=1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   

### K-means clustering   
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   




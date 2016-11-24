rm(list=ls(all=TRUE))
setwd("#Path To Working Directory")

data=read.csv(file="FlightDelays.csv", header=T)

data = subset(data,select=-c(FL_DATE,FL_NUM,TAIL_NUM))
names(data)
str(data)
DEP_TIME_BIN=0
for(i in 1:2201) {
  if(data$DEP_TIME[i]<600){DEP_TIME_BIN[i]="0500-0559"} 
  else if(data$DEP_TIME[i]<700) {DEP_TIME_BIN[i]="0600-0659"}
  else if(data$DEP_TIME[i]<800) {DEP_TIME_BIN[i]="0700-0759"}
  else if(data$DEP_TIME[i]<900) {DEP_TIME_BIN[i]="0800-0859"}
  else if(data$DEP_TIME[i]<1000) {DEP_TIME_BIN[i]="0900-0959"}
  else if(data$DEP_TIME[i]<1100) {DEP_TIME_BIN[i]="1000-1059"}
  else if(data$DEP_TIME[i]<1200) {DEP_TIME_BIN[i]="1100-1159"}
  else if(data$DEP_TIME[i]<1300) {DEP_TIME_BIN[i]="1200-1259"}
  else if(data$DEP_TIME[i]<1400) {DEP_TIME_BIN[i]="1300-1359"}
  else if(data$DEP_TIME[i]<1500) {DEP_TIME_BIN[i]="1400-1459"}
  else if(data$DEP_TIME[i]<1600) {DEP_TIME_BIN[i]="1500-1559"}
  else if(data$DEP_TIME[i]<1700) {DEP_TIME_BIN[i]="16300-1659"}
  else if(data$DEP_TIME[i]<1800) {DEP_TIME_BIN[i]="1700-1759"}
  else if(data$DEP_TIME[i]<1900) {DEP_TIME_BIN[i]="1800-1859"}
  else if(data$DEP_TIME[i]<2000) {DEP_TIME_BIN[i]="1900-1959"}
  else if(data$DEP_TIME[i]<2100) {DEP_TIME_BIN[i]="2000-2059"}
  else if(data$DEP_TIME[i]<2200) {DEP_TIME_BIN[i]="2100-2159"}
  else if(data$DEP_TIME[i]<2300) {DEP_TIME_BIN[i]="2200-2259"}
  else {DEP_TIME_BIN[i]="2300-2359"}
}

Flight.Status2 = factor(ifelse(data$Flight.Status == "delayed", 1, 0))

data2 = data.frame(data[,-c(2,5)],DEP_TIME_BIN,Flight.Status2)
names(data2)

train = sample(1:2201,1321) # to take a random sample of  60% of the records for train data 
data_train = data2[train,] 
nrow(data_train)

test = (1:2201) [-train] 
data_test = data2[test,] 
nrow(data_test)

table(data_train$Flight.Status2)
1079/(1079+242)

write.csv(data_train, "data_train.csv")
install.packages("e1071")
library(e1071)
model = naiveBayes(Flight.Status2 ~ CARRIER+DEP_TIME_BIN+DEST+ORIGIN, data = data_train)
model$tables
model$apriori

pred = predict(model, data_train)
predraw = predict(model, data_train, type="raw")
a=table(pred, data_train$Flight.Status2)
a_acc = sum(diag(a))/sum(rowSums(a[1:2,]))

pred = predict(model, data_test)
b=table(pred, data_test$Flight.Status2)
b_acc = sum(diag(b))/sum(rowSums(b[1:2,]))


# References
# http://www.inside-r.org/packages/cran/e1071/docs/naiveBayes
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Na%C3%AFve_Bayes
# http://www.cs.upc.edu/~belanche/Docencia/mineria/English-september-2008/Practical-work/Labo-NBayes-kNN.R
# http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/

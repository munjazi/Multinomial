mydata<-read.csv("D:\\Workspace\\Git\\R-Programming\\ProjectBandung\\advertising.csv",header=TRUE)
mydata$Age<-as.numeric(mydata$Age)
mydata$Male<-as.numeric(mydata$Male)
summary(mydata)

mydata<-mydata[-5]
mydata<-mydata[-5]
mydata<-mydata[-7]
mydata<-mydata[-6]
names(mydata)

n<-nrow(mydata)
n1<-floor(n*(0.75))
n2<-n-n1

set.seed(123)
ind<-sample(1:n,n1,replace = FALSE)
train<-mydata[ind,]
test<-mydata[-ind,]
log_fit1<-glm(Clicked.on.Ad~.,family = "binomial",data = train)
summary(log_fit1)

ptrain <- predict(log_fit1,newdata=train,type="response")
gg1=floor(ptrain+0.5)
ttt=table(train$Clicked.on.Ad,gg1)
ttt

ptest <- predict(log_fit1,newdata=test,type="response")
gg2=floor(ptest+0.5)
ttt=table(test$Clicked.on.Ad,gg2)
ttt

require(ROSE)
roc.curve(train$Clicked.on.Ad,gg1)
roc.curve(test$Clicked.on.Ad,gg2)


require(caTools)
require(caret)
require(rpart)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
require(randomForest)

#load dataset
mydata=read.csv("~.\\advertising.csv",header=TRUE)

#simplify column names
colnames(mydata)[which(names(mydata) == "Daily.Time.Spent.on.Site")] <- "spent"
colnames(mydata)[which(names(mydata) == "Daily.Internet.Usage")] <- "usage"
colnames(mydata)[which(names(mydata) == "Area.Income")] <- "income"
colnames(mydata)[which(names(mydata) == "Age")] <- "age"
colnames(mydata)[which(names(mydata) == "Ad.Topic.Line")] <- "topic"
colnames(mydata)[which(names(mydata) == "City")] <- "city"
colnames(mydata)[which(names(mydata) == "Male")] <- "gender"
colnames(mydata)[which(names(mydata) == "Country")] <- "country"
colnames(mydata)[which(names(mydata) == "Timestamp")] <- "timestamp"
colnames(mydata)[which(names(mydata) == "Clicked.on.Ad")] <- "clickad"
names(mydata)

#edit data type
mydata$age=as.numeric(mydata$age)
mydata$gender=as.numeric(mydata$gender)
summary(mydata)

#split data into train and test dataset
set.seed(123)
sample = sample.split(mydata,SplitRatio = 0.8)
train=subset(mydata,sample ==TRUE)
test=subset(mydata, sample==FALSE)

#Apply the 1st Logistic Regression algorithm
log_fit1<-glm(clickad~spent+usage+income+age+country+gender,family = "binomial",data = train)
summary(log_fit1)
#Apply the 2nd Logistic Regression algorithm
log_fit2<-glm(clickad~spent+usage+income+age,family = "binomial",data = train)
summary(log_fit2)

#Tabulation training model prediction and real data
ptrain <- predict(log_fit1,newdata=train,type="response")
gg1=floor(ptrain+0.5)
tab1=table(train$clickad,gg1)
tab1
ptrain2 <- predict(log_fit2,newdata=train,type="response")
gg2=floor(ptrain2+0.5)
tab2=table(train$clickad,gg2)
tab2

#Tabulation test model prediction and real data
ptest <- predict(log_fit1,newdata=test,type="response")
gg3=floor(ptest+0.5)
tab3=table(test$clickad,gg3)
tab3
ptest2 <- predict(log_fit2,newdata=test,type="response")
gg4=floor(ptest2+0.5)
tab4=table(test$clickad,gg4)
tab4

#Apply the Desicion Tree Algorithm
tree1 = rpart(clickad~spent+usage+income+age+country+gender, data=train, method = "class")
fancyRpartPlot(tree1)
dttest <- predict(tree1, test, type = "class")

# Make results ready for submission
dtree <- data.frame(test, clickad = dttest )
dtree[1:5,]


# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(clickad)~spent+usage+income+age+gender, data=train, importance=TRUE,ntree=1000)

# Make your prediction using the test set
rftest <- predict(my_forest, test)

# Create a data frame with data test
rforest <- data.frame(test, clickad = rftest )
rforest[1:5,]

#create random forest plot
varImpPlot(my_forest)






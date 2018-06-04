library(caTools)
library(e1071)
data=read.csv("D:\\data Jazi\\test\\DATA.csv", sep=',')
head(data)

set.seed(123)
sample = sample.split(data,SplitRatio = 0.75)
train =subset(data,sample ==TRUE)
test=subset(data, sample==FALSE)

n1=svm(response~factor(platform)+condition+courier_count+view_count+seller_delivery_response_time,data=train,kernel='linear')

m1=lm(sold~factor(platform)+condition+courier_count+view_count+seller_delivery_response_time,data=data)
summary(m1)
m2=lm(sold~factor(platform)+condition+view_count+seller_delivery_response_time,data=data)
summary(m2)
m3=lm(sold~condition+view_count+seller_delivery_response_time,data=data)
summary(m3)


#R code Programming

maxin=function(data){
m=max(data[,2])
for (i in 1:nrow(data)){
if (data[i,2]==m){
 print(data[i,])
 }}}
maxin(data)
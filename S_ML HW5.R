# STAT5241 S_ML HW5
# Litong Wen  UNI:lw2627
# Problem 1
# a. 
plot(NA,NA,type="n",xlim = c(-4, 2),ylim = c(-1, 5),asp = 1,xlab = "X1",ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
# b.
plot(NA,NA,type="n",xlim = c(-4, 2),ylim = c(-1, 5),asp = 1,xlab = "X1",ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")
# c.
plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"),type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(0),c(0),pos=4,"(0,0) blue",cex=0.7)
text(c(-1),c(1),pos=3,"(-1,1) red",cex=0.7)
text(c(2),c(2),pos=3,"(2,2) blue",cex=0.7)
text(c(3),c(8),pos=1,"(3,8) blue",cex=0.7)

# problem 3
# set up package
library(e1071)
# load data
x5<-read.table("train.5.txt",header = F,sep = ",")
x6<-read.table("train.6.txt",header = F,sep = ",")
x<-rbind(as.matrix(x5),as.matrix(x6))
y<-as.matrix(rep(c(-1,1),c(nrow(x5),nrow(x6))))
colnames(y)<-"class"
dataset<-cbind(y,x)
# define train and test data
set.seed(123)
test.size<-0.2*nrow(dataset)
p<-sample.int(nrow(dataset))
testIndex<-p[1:test.size]
trainIndex<-p[-(1:test.size)]
train.data<-dataset[trainIndex,]
test.data<-dataset[testIndex,]
class.train<-as.matrix(train.data[,1])
class.test<-as.matrix(test.data[,1])
x.train<-as.matrix(train.data[,-1])
x.test<-as.matrix(test.data[,-1])
# 1.1 Train a linear SVM with soft margin
cost<-c(0.0000001,0.000001,0.00001,0.0001,0.001,0.01,0.1,0.5,1,10)
err.linear<-rep(NA,10)
for (i in 1:length(cost)){
  svm.linear<-svm(class.train ~.,data=train.data,type="C-classification",kernel="linear",gamma=0,cost=cost[i],cross=5)
  acc[i]<-summary(svm.linear)$tot.accuracy
  err.linear[i]<-100-acc[i]
}
plot(cost,err.linear,type="l",xlab = "cost",ylab = "misclassification rate",main = "Linear SVM misclassification")
# 1.2.Train a radial SVM with soft margins and RBF kernel
err.rbf<-matrix(NA,nrow=4,ncol=10)
acc.rbf<-matrix(NA,nrow=4,ncol=10)
gamma <- 10^c(-1, -2, -3, -4)
for (j in 1:length(gamma)){
  for (i in 1:length(cost)){
    svm.rbf<-svm(class.train ~., data = train.data,type="C-classification",kernel="radial",gamma=gamma[j],cost=cost[i],cross=5)
    acc.rbf[j,i]<-summary(svm.rbf)$tot.accuracy
    err.rbf[j,i]<-100-acc.rbf[j,i]
  }
}
plot(cost,err.rbf[1,],type = "l",ylim=c(0,50),xlab = "cost",ylab = "misclassification rate",main = "Non-linear SVM misclassification")
lines(cost,err.rbf[2,],lty=2,col="red")
lines(cost,err.rbf[3,],lty=3,col="blue")
lines(cost,err.rbf[4,],lty=4,col="green")
legend(6,35,c("gamma=0.1","gamma=0.01","gamma=0.001","gamma=0.0001"),lty=c(1:4),col = c("black","red","blue","green"))
# 2.1 Train linear SVM on test dataset with optimal parameter values
# choose cost = 0.001
opt.linear<-svm(x.train,class.train,type="C-classification",kernel="linear",gamma=0,cost=0.001,cross=5) #tot.accuracy=97.7459
pred.linear<-predict(opt.linear,x.test,decision.values = TRUE)
sum(pred.linear!=class.test)/length(class.test)
# 2.2 Train non-linear SVM on test dataset with optimal parameter values
# choose gamma = 0.001, cost = 10
opt.rbf<-svm(x.train,class.train,type="C-classification",kernel="radial",gamma=0.001,cost=10,cross=5) #tot.accuracy=98.2582
pred.rbf<-predict(opt.rbf,x.test,decision.values = TRUE)
sum(pred.rbf!=class.test)/length(class.test)

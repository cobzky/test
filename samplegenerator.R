library(MASS)
library(ggplot2)
#Generates a random sample for classification (see ESL page. 17)
generatedata<-function(){
  means1<-mvrnorm(10,c(1,0),matrix(c(1,0,0,1),nrow=2))
  means2<-mvrnorm(10,c(0,1),matrix(c(1,0,0,1),nrow=2))

  firstmeans<-sample(seq(1,10),100,replace=T)
  secondmeans<-sample(seq(1,10),100,replace=T)

  blue<-matrix(numeric(2*100),nrow=100)
  orange<-matrix(numeric(2*100),nrow=100)

  for(i in 1:100){
    k<-firstmeans[i]
    l<-secondmeans[i]
    blue[i,]<-mvrnorm(1,means1[k,],matrix(1/5*c(1,0,0,1),nrow=2))
    orange[i,]<-mvrnorm(1,means2[l,],matrix(1/5*c(1,0,0,1),nrow=2))
  }
  data<-matrix(numeric(200*4),nrow=200)
  data[,1]<-c(rep(1,100),rep(0,100))
  data[,2]<-c(rep(1,200))
  data[1:100,3:4]<-blue
  data[101:200,3:4]<-orange
  data<-data.frame(data)
  plot<-ggplot(data,aes(X3,X4,color=X1)) + geom_point()
  ggsave("Generatedplot.png",plot)
  names(data)<-c("Class","Intercept","X_coordinate","Y_coordinate")
  return(data)


}
data<-generatedata()

linearclass<-function(data){
  Y<-as.matrix(data[,1])
  X<-as.matrix(data[,2:4])
  beta<-solve(t(X)%*%X)%*%t(X)%*%Y
  boundry<-numeric(nrow(X))
  for(i in 1:nrow(X)){
    boundry[i]<-(0.5-beta[1]-beta[2]*X[i,2])/beta[3]
  }
  data<-data.frame(data,"boundry"=boundry)
  plot<-ggplot(data,aes(X_coordinate,Y_coordinate,color=Class))
  + geom_point() + geom_line(aes(y=boundry))
  ggsave("Boundryplot.png",plot)

}
linearclass(data)

  setwd("Desktop")
  mydata<-read.csv("binary.csv",header=T)
  head(mydata)
  #changing to categorial variable
  mydata$admit<-as.factor(mydata$admit)
  mydata$rank<-as.factor(mydata$rank)
  str(mydata)
  #two way table for factor variable
  xtabs(~admit+rank,data=mydata)
  #data partioining
  set.seed(1234)
  ind<-sample(2,nrow(mydata),prob=c(.8,.2),replace=T)
  training<-mydata[ind==1,]
  testing<-mydata[ind==2,]
  head(training)
  #logistic regression model
  model<-glm(admit~gre+gpa+rank,data=training,family="binomial")
  summary(model)
  newmodel<-glm(admit~gpa+rank,data=training,family="binomial")
  summary(newmodel)
  #prediction
  p1<-predict(newmodel,data=training,type='response')
  head(p1)
  head(training)
  #confusion matrix+classification error
  pred1<-ifelse(p1>0.5,1,0)
  tab1<-table(predicted=pred1,actual=training$admit)
  tab1
  #Godness of fit(calculation of p value)
  with(newmodel,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail=F))
  
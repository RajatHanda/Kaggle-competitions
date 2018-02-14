library(plyr)
library(dplyr)
library(plotly)
library(knitr)
library(gridExtra)
library(ggplot2)
library(data.table)
library(plotly)
library(ggplot2)
library(ggthemes)
library(reshape2)
por <- read.csv("/Users/stem/Desktop/alc/student-mat.csv",header=T,stringsAsFactors = F)
por1$Dalc <- as.factor(por$Dalc)
#----------------------school and sex alchol consumption----------------------
g1<-ggplot(por, aes(x=(as.numeric(age))))+ geom_histogram(fill="brown", colour="black",binwidth=1) +xlab("Age")+
  facet_grid(Dalc ~ .)+ggtitle("Workday Consumption")+theme_solarized_2(light = TRUE)+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(colour = "black"),title = element_text(colour = "black"))
g2<-ggplot(por, aes(x=(as.numeric(age))))+ geom_histogram(fill="darkgreen", colour="black",binwidth=1) +xlab("Age")+
  facet_grid(Walc ~ .)+ggtitle("Weekend Consumption")+theme_solarized_2(light = TRUE)+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(colour = "black"),title = element_text(colour = "black"))
grid.arrange(g1,g2,ncol=2)
c3 <- ggplot(por, aes(x=Dalc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("magenta","blue"))+
  theme_igray()+
  xlab("WORKDAY ALCHOL CONSUMPTION")+
  ylab("School")+
  ggtitle("WorkDay.A.C. School & Sex")


c4 <- ggplot(por, aes(x=Walc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("magenta","blue"))+
  theme_solarized_2(light = FALSE)+theme(axis.title.x = element_text(colour = "white"),axis.title.y = element_text(colour = "white"),title = element_text(colour = "white"))+
  xlab("WEEKEND ALCHOL CONSUMPTION")+
  ylab("School")+
  ggtitle("Weekend.A.C. School & Sex")

grid.arrange(c3,c4, nrow=2)

#----------------------------Alchol consumption and grades----------
dalc_count <- por %>% 
  count(Dalc)

walc_count <- por %>% 
  count(Walc)

por %>% 
  ggplot(aes(Walc,G3)) + 
  ylab("Final Grade") +
  xlab("Weekend Alcohol Consumption") +
  geom_boxplot(fill="darkgreen") +
  theme_solarized_2(light = TRUE) +
  geom_text(data=walc_count,aes(x =  Walc, y = 20, label= paste("n =", n)), 
            colour="black", inherit.aes=FALSE, parse=FALSE)+ggtitle("Weekend.A.C.") ->
  g1

por %>% 
  ggplot(aes(Dalc,G3)) + 
  ylab("Final Grade") +
  xlab("Weekday Alcohol Consumption") +
  geom_boxplot(fill="brown") +
  theme_solarized_2(light = TRUE) +
  geom_text(data=dalc_count,aes(x =  Dalc, y = 20, label=paste("n =", n)), 
            colour="black", inherit.aes=FALSE, parse=FALSE)+ggtitle("WeekDay.A.C.") ->
  g2

grid.arrange(g2, g1, ncol = 2)
#---------------------Other Factors afecting Student performance-------
w1<-por %>% mutate(failures=as.factor(failures))%>% group_by(famrel,failures) %>% summarise(n=n()) %>%
  ddply("famrel",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=famrel,y=percent,fill=failures))+
  geom_bar(stat="identity")+ggtitle("Number of failures(%) and Family Relationship")+theme_solarized_2(light = TRUE)+xlab("Family relationship")
w2<-por %>% group_by(famrel,Dalc) %>% summarise(n=n()) %>%
  ddply("famrel",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=famrel,y=percent,fill=Dalc))+
  geom_bar(stat="identity")+ggtitle("Weekday Alchol Consumption and Family Relationship")+theme_solarized_2(light = TRUE)+xlab("Family relationship")
w3<-por %>% group_by(famrel,Walc) %>% summarise(n=n()) %>%
  ddply("famrel",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=famrel,y=percent,fill=Walc))+
  geom_bar(stat="identity")+ggtitle("Weekend Alchol Consumption and Family Relationship")+theme_solarized_2(light = TRUE)+xlab("Family relationship")
grid.arrange(w1,w2,w3, ncol = 3)
#-------Pattern between studytime and grades----------
e1<-por %>% group_by(studytime,G3) %>% summarise(n=n()) %>%
  ddply("studytime",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=studytime,y=percent,fill=G3))+
  geom_bar(stat="identity")+ggtitle("StudyTime and 3 Grades")+theme_solarized_2(light = FALSE)
e2<-por %>% group_by(studytime,G2) %>% summarise(n=n()) %>%
  ddply("studytime",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=studytime,y=percent,fill=G2))+
  geom_bar(stat="identity")+ggtitle("StudyTime and 2 Grades")+theme_solarized_2(light = FALSE)
e3<-por %>% group_by(studytime,G1) %>% summarise(n=n()) %>%
  ddply("studytime",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=studytime,y=percent,fill=G1))+
  geom_bar(stat="identity")+ggtitle("StudyTime and 1 Grades")+theme_solarized_2(light = FALSE)
grid.arrange(e3,e2,e1, ncol = 3)
#--------Internet Access-----STUDENT WITH INTERNET HAS HIGHER GRADES---
ggplot(por, aes(x=G3, fill=internet)) +
  geom_histogram(position="identity", alpha=0.4,binwidth=1.0)+theme_solarized_2(light = TRUE)
#----Location and Grades-----------------------------
h2<-por %>% group_by(address,G2) %>% summarise(n=n()) %>%
  ddply("address",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=address,y=percent,fill=G2))+
  geom_bar(stat="identity")+ggtitle("Location and 2 Grades")+theme_solarized_2(light = FALSE)
h1<-por %>% group_by(address,G1) %>% summarise(n=n()) %>%
  ddply("address",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=address,y=percent,fill=G1))+
  geom_bar(stat="identity")+ggtitle("Location and 1 Grades")+theme_solarized_2(light = FALSE)
h3<-por %>% group_by(address,G3) %>% summarise(n=n()) %>%
  ddply("address",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=address,y=percent,fill=G3))+
  geom_bar(stat="identity")+ggtitle("Location and 3 Grades")+theme_solarized_2(light = FALSE)
grid.arrange(h1,h2,h3, ncol = 3)
#----Occupation and alchol consumption--
d1<-ggplot(por, aes(x=Walc, y=Fjob, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("orange","green"))+
  theme_solarized_2(light = FALSE)+theme(axis.title.x = element_text(colour = "white"),axis.title.y = element_text(colour = "white"),title = element_text(colour = "white"))+
  xlab("WEEKEND ALCHOL CONSUMPTION")+
  ylab("Occupation Domain")+
  ggtitle("Weekend.A.C. Father Occupation & Sex")
d2<-ggplot(por, aes(x=Walc, y=Mjob, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("orange","green"))+
  theme_solarized_2(light = FALSE)+theme(axis.title.x = element_text(colour = "white"),axis.title.y = element_text(colour = "white"),title = element_text(colour = "white"))+
  xlab("WEEKEND ALCHOL CONSUMPTION")+
  ylab("Occupation Domain")+
  ggtitle("Weekend.A.C. Mother Occupation & Sex")
grid.arrange(d1,d2, ncol = 2)
e1<-por %>% group_by(Fedu,Walc) %>% summarise(n=n()) %>%
  ddply("Fedu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Fedu,y=percent,fill=Walc))+
  geom_bar(stat="identity")+ggtitle("Weekend Alcohol Consumption vs Father's Education Levels")+theme_solarized_2(light = TRUE)
e2<-por %>% group_by(Fedu,Dalc) %>% summarise(n=n()) %>%
  ddply("Fedu",transform,percent=n/sum(n)*100) %>%
  ggplot(aes(x=Fedu,y=percent,fill=Dalc))+
  geom_bar(stat="identity")+ggtitle("Daily Alcohol Consumption vs Father's Education Levels")+theme_solarized_2(light = TRUE)
grid.arrange(e2,e1,ncol = 2)
#--------
por1 <- read.csv("/Users/stem/Desktop/alc/student-mat.csv",header=T,stringsAsFactors = T)

por1$avggrades=rowMeans(cbind(por1$G1,por1$G2,por1$G3))
# and drop grades in 3 marking periods.
por1<-por1[,-(31:33)]


#t-test-------
t1 <- t.test(por1$Walc,por1$Dalc,paired = TRUE,alternative = "greater")
t1
library(MASS)
tb1 <- table(por1$Walc,por1$Fjob)
chisq.test(tb1)
tb2 <- table(por1$Walc,por1$Mjob)
chisq.test(tb2)
tb3 <- table(por1$Walc,por1$Fedu)
chisq.test(tb3)
tb4 <- table(por$studytime,por$G3)
chisq.test(tb4)
tb4 <- table(por$studytime,por$G2)
chisq.test(tb4)
tb4 <- table(por$famrel,por$failures)
chisq.test(tb4)
tb1 <- table(por1$Dalc,por1$Fjob)
chisq.test(tb1)
tb2 <- table(por1$Dalc,por1$Mjob)
chisq.test(tb2)
tb3 <- table(por1$Dalc,por1$Fedu)
chisq.test(tb3)
tb4 <- table(por1$Dalc,por1$Medu)
chisq.test(tb4)

# create correlation plot
library(corrplot)
numericColumns <- sapply(por1, is.numeric)
corMatrix<-cor(por1[,numericColumns])
corrplot(corMatrix, method="circle")
#----------- removing highly coorelated columns --------------#
f_rem<-which(names(por1)=="failures")
por1<-por1[,-f_rem]

#Machine Learning---------
#-----------Linear Models--------------------
lm<-lm(avggrades~., data=por1[,1:30])
summary(lm)
plot(lm)

lm.predictions<-predict(lm,por1)
str(lm.predictions)
nmse.lm<-mean((lm.predictions-por1[,"avggrades"])^2)/mean((mean(por1$avggrades)-por1[,"avggrades"])^2)
print(nmse.lm) #0.85
lmpltdata1=data.frame(cbind(lm.predictions,por1[,"avggrades"]))
colnames(lmpltdata1)<-c("lm.predictions","avggrades")

por1$Dalc<-as.factor(por1$Dalc)
library(alluvial)
library(RColorBrewer)
errplt.lt1=ggplot(lmpltdata1,aes(lm.predictions,avggrades))+
  geom_point(aes(color=por1[,"avggrades"]))+
  xlab("Predicted Grades (Linear Model)")+
  ylab("Actual Grades")+ggtitle("Linear Regression")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+scale_colour_gradientn(colours=rainbow(5),name = "Grades")

#--------LDA---------------
library(caret)
library(MASS)
fit <- lda(avggrades ~., data = por1[,1:30])
yhat.lda <- predict(fit,por1)
fit.nmse

# confussion matrix
str(yhat.lda$class)
ki<-as.numeric(as.character(yhat.lda$class))
str(ki)
nmse.lda<-mean((ki-por1[,"avggrades"])^2)/mean((mean(por1$avggrades)-por1[,"avggrades"])^2)
print(nmse.lda) #0.68
LDApltdata1=data.frame(cbind(ki,por1[,"avggrades"]))
colnames(LDApltdata1)<-c("lda.predictions","avggrades")

errplt.lda=ggplot(LDApltdata1,aes(ki,avggrades))+
  geom_point(aes(color=por1[,"avggrades"]))+
  xlab("Predicted Grades (LDA)")+
  ylab("Actual Grades")+ggtitle("LDA")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+scale_colour_gradientn(colours=rainbow(5),name = "Grades")

#---------SVR--------------
library(e1071)
lm3 = svm(avggrades~., data=por1[,1:30],type = 'eps-regression',kernel = 'radial')
summary(lm3)
svr.predictions<-predict(lm3,por1)
nmse.svr<-mean((svr.predictions-por1[,"avggrades"])^2)/mean((mean(por1$avggrades)-por1[,"avggrades"])^2)
print(nmse.svr) #0.63
svrpltdata1=data.frame(cbind(svr.predictions,por1[,"avggrades"]))
colnames(svrpltdata1)<-c("svr.predictions","avggrades")
errplt.svr<-ggplot(svrpltdata1,aes(svr.predictions,avggrades))+
  geom_point(aes(color=por1[,"avggrades"]))+
  xlab("Predicted Grades (SVM)")+
  ylab("Actual Grades")+ggtitle("SupportVectorRegression")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+scale_colour_gradientn(colours=rainbow(5),name = "Grades")
#--------------RF----------------
library(randomForest)
set.seed(4543)
mtry <- sqrt(ncol(por1))
ntree <- 1000
rf <- randomForest(avggrades~.,data = por1[,1:30],mtry=mtry,ntree=ntree)
rf.predictions<-predict(rf,por1)
nmse.rf<-mean((rf.predictions-por1[,"avggrades"])^2)/mean((mean(por1$avggrades)-por1[,"avggrades"])^2)
print(nmse.rf)#0.20
rfpltdata1=data.frame(cbind(rf.predictions,por1[,"avggrades"]))
colnames(rfpltdata1)<-c("rf.predictions","avggrades")

errplt.rf<-ggplot(rfpltdata1,aes(rf.predictions,avggrades))+
  geom_point(aes(color=por1[,"avggrades"]))+
  xlab("Predicted Grades (RandomForest)")+
  ylab("Actual Grades")+ggtitle("RandomForest(1000 Trees)")+
  geom_abline(intercept=0,slope=1,color="black",size=1)+scale_colour_gradientn(colours=rainbow(5),name = "Grades")

grid.arrange(errplt.lt1,errplt.lda,errplt.svr,errplt.rf,nrow=2, ncol =2)

t<-c(nmse.lm,nmse.lda,nmse.svr,nmse.rf)
tm <- as.vector(c("Linear Model","LDA","SVR","RF"))
barplot(t)
ba


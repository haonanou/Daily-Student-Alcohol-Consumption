#read data from student-mat.csv
math=read.csv(file.choose(),header=T)
attach(math)

#number of observations
print(nrow(math))

#distinguish between categorical and continuous variables
str(math)

#Dataset Summary
summary(math)

#convert continuous attribute to categorical one 
math$famrel=as.factor(math$famrel)
math$health=as.factor(math$health)
math$Mjob=as.factor(math$Mjob)
math$Fjob=as.factor(math$Fjob)
math$traveltime=as.factor(math$traveltime)
math$studytime=as.factor(math$studytime)
math$freetime=as.factor(math$freetime)
math$goout=as.factor(math$goout)


#Turn the Y variable into binomial variable
math$Dalc[math$Dalc>2]="Yes"
math$Dalc[math$Dalc<=2]="No"
math$Walc[math$Walc>2]="Yes"
math$Walc[math$Walc<=2]="No"
math$Dalc=as.factor(math$Dalc)
math$Walc=as.factor(math$Walc)

#make two variables contrasts
contrasts(math$Dalc)
contrasts(math$Walc)

#===========================================#

#Stepwise Regression to find optimal predictors for Dalc and Walc

null = glm(math$Dalc ~ 1, family="binomial",data = math)
full = glm(math$Dalc ~ math$school+math$sex+math$age+math$address+math$famsize+math$Pstatus+math$Medu+math$Fedu+math$Mjob+math$Fjob+math$reason+math$guardian+math$traveltime+math$studytime+math$failures+math$schoolsup+math$famsup+math$paid+math$activities+math$nursery+math$higher+math$internet+math$romantic+math$famrel+math$freetime+math$goout+math$health+math$absences, family="binomial", data = math)
step.reg = step(null, scope=list(lower=null, upper=full),direction = 'both')
summary(step.reg)

null2 = glm(math$Walc ~ 1, family="binomial",data = math)
full2 = glm(math$Walc ~ math$school+math$sex+math$age+math$address+math$famsize+math$Pstatus+math$Medu+math$Fedu+math$Mjob+math$Fjob+math$reason+math$guardian+math$traveltime+math$studytime+math$failures+math$schoolsup+math$famsup+math$paid+math$activities+math$nursery+math$higher+math$internet+math$romantic+math$famrel+math$freetime+math$goout+math$health+math$absences, family="binomial",data = math)
step.reg2 = step(null2, scope=list(lower=null2, upper=full2), direction = 'both')
summary(step.reg2)

exp(coef(step.reg))

#===========================================#

#orginal data set(Walc)

glm.fit5=glm(math$Walc ~ math$goout + math$sex + math$Fjob + 
               math$absences + math$famrel + math$paid + math$traveltime + 
               math$address + math$famsize + math$nursery + math$activities, 
             family = "binomial", data = math)

glm.probs5=predict(glm.fit5,math, type="response")
glm.pred5=rep("No",395)
glm.pred5[glm.probs5>.5]="Yes"
test.truevalue=math$Walc
table(glm.pred5,test.truevalue)
mean(glm.pred5==test.truevalue)

#orginal data set(Dalc)

glm.fit5=glm(math$Dalc ~ math$sex + math$goout + math$school + math$absences + 
               math$traveltime + math$activities + math$higher + math$reason + 
               math$famsize + math$nursery, 
             data = math, family = "binomial" )

glm.probs5=predict(glm.fit5,math, type="response")
glm.pred5=rep("No",395)
glm.pred5[glm.probs5>.5]="Yes"
test.truevalue=math$Dalc
table(glm.pred5,test.truevalue)
mean(glm.pred5==test.truevalue)

#===========================================#

#Logistic Regression using Rose Package (for unbalanced data) and Dalc Response Variable

#resmaple use rose package
library(ROSE)

#Randomly Over Sampling Examples using Rose package & "over" sampling
table(math$Dalc)
351*2
over = ovun.sample(Dalc~., data = math,  method = "over", N=702)$data
table(over$Dalc)
summary (over)
test.truevalue2=over$Dalc
accuracy3=rep(1,4)

#bootstrap with resample
set.seed(12385)
train6=sample(nrow(over), 702 ,replace=TRUE)

glm.fit6=glm(over$Dalc ~ over$sex + over$goout + over$school + over$absences + 
               over$traveltime + over$activities + over$higher + over$reason + 
               over$famsize + over$nursery, 
              data = over,subset = train6, family = "binomial" )
glm.probs6=predict(glm.fit6, over, type="response")
glm.pred6=rep("No",702)
glm.pred6[glm.probs6>.5]="Yes"
table(glm.pred6,test.truevalue2)
accuracy3[1]=mean(glm.pred6==test.truevalue2)
accuracy3[1]

#acuuacy 2
set.seed(25185)
train7=sample(nrow(over), 702 ,replace=TRUE)

glm.fit7=glm(over$Dalc ~ over$sex + over$goout + over$school + over$absences + 
               over$traveltime + over$activities + over$higher + over$reason + 
               over$famsize + over$nursery, 
             data = over,subset = train7, family = "binomial" )
glm.probs7=predict(glm.fit7, over, type="response")
glm.pred7=rep("No",702)
glm.pred7[glm.probs7>.5]="Yes"
table(glm.pred7,test.truevalue2)
accuracy3[2]=mean(glm.pred7==test.truevalue2)
accuracy3[2]

#acuuacy 3
set.seed(35815)
train8=sample(nrow(over), 702 ,replace=TRUE)

glm.fit8=glm(over$Dalc ~ over$sex + over$goout + over$school + over$absences + 
               over$traveltime + over$activities + over$higher + over$reason + 
               over$famsize + over$nursery, 
             data = over,subset = train8, family = "binomial" )
glm.probs8=predict(glm.fit8, over, type="response")
glm.pred8=rep("No",702)
glm.pred8[glm.probs8>.5]="Yes"
table(glm.pred8,test.truevalue2)
accuracy3[3]=mean(glm.pred8==test.truevalue2)
accuracy3[3]

#acuuacy 4
set.seed(48521)
train9=sample(nrow(over), 702 ,replace=TRUE)

glm.fit9=glm(over$Dalc ~ over$sex + over$goout + over$school + over$absences + 
               over$traveltime + over$activities + over$higher + over$reason + 
               over$famsize + over$nursery, 
             data = over,subset = train9, family = "binomial" )
glm.probs9=predict(glm.fit9, over, type="response")
glm.pred9=rep("No",702)
glm.pred9[glm.probs9>.5]="Yes"
table(glm.pred9,test.truevalue2)
accuracy3[4]=mean(glm.pred8==test.truevalue2)
accuracy3[4]

accuracy3
mean(accuracy3)

#===========================================#

#Logistic Regression using Rose Package (for unbalanced data) and Walc Response Variable

#Randomly Over Sampling Examples using Rose Package & "Under" sampling
table(math$Walc)
159*2
under = ovun.sample(Walc~., data = math,  method = "under", N=318)$data
table(under$Walc)
summary (under)
test.truevalue4=under$Walc

accuracy4= rep(1,4)

#bootstrap with resample
set.seed(16)
train16=sample(nrow(under), 318 ,replace=TRUE)

glm.fit16=glm(under$Walc ~ under$goout + under$sex + under$Fjob + 
               under$absences + under$famrel + under$paid + under$traveltime + 
               under$address + under$famsize + under$nursery + under$activities, 
             data = under,subset = train16, family = "binomial" )
glm.probs16=predict(glm.fit16, under, type="response")
glm.pred16=rep("No",318)
glm.pred16[glm.probs16>.5]="Yes"
table(glm.pred16,test.truevalue4)
accuracy4[1]=mean(glm.pred16==test.truevalue4)

set.seed(17)
train17=sample(nrow(under), 318 ,replace=TRUE)

glm.fit17=glm(under$Walc ~ under$goout + under$sex + under$Fjob + 
                under$absences + under$famrel + under$paid + under$traveltime + 
                under$address + under$famsize + under$nursery + under$activities, 
              data = under,subset = train17, family = "binomial" )
glm.probs17=predict(glm.fit17, under, type="response")
glm.pred17=rep("No",318)
glm.pred17[glm.probs17>.5]="Yes"
table(glm.pred17,test.truevalue4)
accuracy4[2]=mean(glm.pred17==test.truevalue4)

set.seed(18)
train18=sample(nrow(under), 318 ,replace=TRUE)

glm.fit18=glm(under$Walc ~ under$goout + under$sex + under$Fjob + 
                under$absences + under$famrel + under$paid + under$traveltime + 
                under$address + under$famsize + under$nursery + under$activities, 
              data = under,subset = train18, family = "binomial" )
glm.probs18=predict(glm.fit18, under, type="response")
glm.pred18=rep("No",318)
glm.pred18[glm.probs18>.5]="Yes"
table(glm.pred18,test.truevalue4)
accuracy4[3]=mean(glm.pred18==test.truevalue4)

set.seed(19)
train19=sample(nrow(under), 318 ,replace=TRUE)

glm.fit19=glm(under$Walc ~ under$goout + under$sex + under$Fjob + 
                under$absences + under$famrel + under$paid + under$traveltime + 
                under$address + under$famsize + under$nursery + under$activities, 
              data = under,subset = train19, family = "binomial" )
glm.probs19=predict(glm.fit19, under, type="response")
glm.pred19=rep("No",318)
glm.pred19[glm.probs19>.5]="Yes"
table(glm.pred19,test.truevalue4)
accuracy4[4]=mean(glm.pred19==test.truevalue4)

accuracy4
mean(accuracy4)

#===========================================#

#Decision Tree Model 
library('tree')

library(ROSE)
table(math$Dalc)
over = ovun.sample(Dalc~., data = math, method = "over", N=702)$data
table(over$Dalc)
351*2

set.seed(1)

trainDalc=sample(nrow(over), 702, replace = T)
tree.modelDalc=tree(Dalc~ sex + famsize + Pstatus + Medu+ Fedu + famsup + nursery+ famrel+absences,over,subset =trainDalc)
overDalc.test=over[-trainDalc,]
Dalc.test=over$Dalc[-trainDalc]

cv.modelDalc=cv.tree(tree.modelDalc,K=10,FUN=prune.misclass)
cv.modelDalc

prune.model=prune.tree(tree.modelDalc,best=21)


#since the figure margins is too large, we implement code belove to code this issue 
graphics.off()
par("mar")
par(mar=c(1,1,1,1))

plot(prune.model)
text(prune.model,pretty=1)


prunetree.pred=predict(prune.model,overDalc.test,type="class")

table(prunetree.pred,Dalc.test)

mean(prunetree.pred==Dalc.test)

#--- Random Forest 
library(randomForest)

set.seed(1)
bag.mathDalc=randomForest(Dalc~ sex + famsize + Pstatus + Medu+ Fedu + famsup + nursery+ famrel+absences,data=over,subset=trainDalc,mtry=9,importance=TRUE)
bag.mathDalc

yhat.bag = predict(bag.mathDalc,newdata=over[-trainDalc,])
mean(yhat.bag==Dalc.test)


set.seed(1) 
rf.mathDalc=randomForest(Dalc~ sex + famsize + Pstatus + Medu+ Fedu + famsup + nursery+ famrel+absences,data=over,subset=trainDalc,mtry=3,importance=TRUE) 
yhat.rf = predict(rf.mathDalc,newdata=over[-trainDalc,])
mean(yhat.rf==Dalc.test)


importance(rf.mathDalc) 
varImpPlot(rf.mathDalc)

#--- Evaluate Weekend Achochol Consumption associated with family situation 
set.seed(1)

table(math$Walc)
under = ovun.sample(Walc~., data = math, method = "under", N=318)$data
table(over$Dalc)
159*2

trainWalc=sample(nrow(under), 318, replace = T)
tree.modelWalc=tree(Walc~ sex + famsize + Pstatus + Medu+ Fedu + famsup + nursery+ famrel+absences,under,subset =trainWalc)
mathWalc.test=under[-trainWalc,]
Walc.test=under$Walc[-trainWalc]

cv.modelWalc=cv.tree(tree.modelWalc,K=10,FUN=prune.misclass)
cv.modelWalc

prune.model=prune.tree(tree.modelWalc,best=13)

#since the figure margins is too large, we implement code belove to code this issue 
graphics.off()
par("mar")
par(mar=c(1,1,1,1))

plot(prune.model)
text(prune.model,pretty=1)


prunetree.pred=predict(prune.model,mathWalc.test,type="class")
table(prunetree.pred,Walc.test)

mean(prunetree.pred==Walc.test)

#--- Random Forest 
library(randomForest)

set.seed(1)
bag.mathWalc=randomForest(Walc~ sex + famsize + Pstatus + Medu+ Fedu + famsup + nursery+ famrel+absences,data=under,subset=trainWalc,mtry=9,importance=TRUE)
bag.mathWalc

yhat.bag = predict(bag.mathWalc,newdata=under[-trainWalc,])
mean(yhat.bag==Walc.test)


set.seed(1) 
rf.mathWalc=randomForest(Walc~ sex + famsize + Pstatus + Medu+ Fedu + famsup + nursery+ famrel+absences,data=under,subset=trainWalc,mtry=3,importance=TRUE) 
yhat.rf = predict(rf.mathWalc,newdata=under[-trainWalc,])
mean(yhat.rf==Walc.test)


importance(rf.mathWalc) 
varImpPlot(rf.mathWalc)



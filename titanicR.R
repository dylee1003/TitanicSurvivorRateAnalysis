library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(vcd)
c_data <- read.csv("titanic.csv")


c_data <- c_data[,-1]
c_data$Survived <- factor(c_data$Survived)

#logistic regression

logit <- glm(Survived~.,data=c_data,family=binomial(logit))
roc(c_data$Survived,round(predict(logit)),plot=T,col="#006699",main="ROC curve")
mtext("Area under the curve: 0.8684",side=3)
#classification tree


model0 <- rpart(Survived~.,data=c_data,method="class")
table(c_data$Survived,round(predict(model0,method="response")))

prp(model0,extra=1,type=3,cex=1.2,main="Classification Tree for Titanic Survive",under.col="#006699",split.col="#999999",compress=T,under=TRUE,under.cex=0.6,split.cex=0.5)

#random Forest Tree
c_data$Survived <- factor(c_data$Survived)

model1 <- randomForest(Survived~.,data=c_data,prox=T)

model1
importance(model1)
# The mean decrease in Gini coefficient is a measure of how each variable contributes to the homogeneity of the nodes and leaves in the resulting random forest.
#http://dinsdalelab.sdsu.edu/metag.stats/code/randomforest.html (for refernce)

#advanced graph

processed <- read.csv("processed_titanic.csv")

processed <- processed[,-1]

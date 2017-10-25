wd <- "/Users/zive/GDrive/research/hcl/2016/coop-pred-ec/camera-code"
setwd(wd)
library("glmnet")
library("dplyr")
library("Matrix")
library(foreach)
source("functions.R")
library(ROCR)
#returns a list with the following elements: 1: decision level disposition data (21340x28)
#                                            2: decision level Situation data, (21340x7)
#                                            3: subject ids, (21340x1)
#                                            4: decisions, 0/1 did you cooperate,  (21340x1), used to generate figure 1
#                                            5: dec_num 1..20,
#                                            6: Sit2)
l <- getData()
demo <- l[[1]]
Situation <- l[[2]]
id <- l[[3]]
dec <- as.factor(as.vector(as.matrix(l[[4]])))
dec_num <- l[[5]]

#figure 1
string_id <- c()
for (i in 1:nrow(id)){
  string_id <- toString()
}
coopz <- c()
data <- data.frame(dec,dec_num,as.matrix(id))
i <- 1
for (ids in unique(as.matrix(id))){
  subset <- data[which(data$id == ids),]
  coopz[i] <-sum(as.numeric(subset$dec[which(subset$dec_num <16)])-1)
  i <- i +1
}

hist(coopz, col='chartreuse3', xlab = "Participant's number of cooperative decisions in training set", ylab = 'Density', main="",freq=F, cex.lab=.85)


##2 type, Figure 2
auczD <-c()

for (k in 1:15){

  ridge.S <- runGiverRegressionRedux(dec, Situation,k=k, id, dec_num=dec_num,squares = "false",alpha = 0, nfolds = 5, type= "within", giverType = "double",testOrder = 'false')
  
  model <- ridge.S[[1]]
  ytest <- ridge.S[[2]]
  xtest <- ridge.S[[3]]
  
  yhat <- predict(model, xtest)
  
  auczD[k] <- performance(prediction(yhat,ytest),"auc")@y.values[[1]] 
  print(paste("Finished K=", k, " with AUC of ", auczD[k] , sep=""))
}

##3 type, Figure 3
aucz2D <- matrix(rep(0,15^2),nrow=15)

for (k in 1:15){
  for (K in k:15){
    ridge.S <- runGiverRegressionRedux(dec, Situation,k=k,K=K, id, dec_num=dec_num, 
                                       squares = "false",alpha = 0, nfolds = 5, type= "within", giverType = "triple", testOrder='false')
    
    model <- ridge.S[[1]]
    ytest <- ridge.S[[2]]
    xtest <- ridge.S[[3]]
    
    yhat <- predict(model, xtest)
    
    aucz2D[k,K] <- performance(prediction(yhat,ytest),"auc")@y.values[[1]] 
    print(paste("Finished k=", k, ", K=", K, " with AUC of ", aucz2D[k,K] , sep=""))
  }
}

#optional implementation of 4 types. we found it does not do much better
auc3D <- array(rep(0,15^3),dim=c(15,15,15))

for (i in 1:15){
  for (j in i:15){
    for (k in j:15){
    ridge.S <- runGiverRegression3D(dec, Situation,i=i,j=j,k=k, id, dec_num=dec_num, 
                                       squares = "false",alpha = 0, nfolds = 5, type= "within")
    
    model <- ridge.S[[1]]
    ytest <- ridge.S[[2]]
    xtest <- ridge.S[[3]]
    
    yhat <- predict(model, xtest)
    
    auc3D[i,j,k] <- performance(prediction(yhat,ytest),"auc")@y.values[[1]] 
    print(paste("Finished i=", i", j= ",j,", k=", k, " with AUC of ",  auc3D[i,j,k] , sep=""))
  }
}


###fixed effects X situation
ridge.feXsit <- runFexSitRegression(dec, Situation, id, dec_num)
                                   
model <- ridge.feXsit[[1]]
ytest <- ridge.feXsit[[2]]
xtest <- ridge.feXsit[[3]]

yhat <- predict(model, xtest)
performance(prediction(yhat,ytest),"auc")@y.values[[1]] 

###fixed effects: 0.8317196
ridge.fe <- runFeRegression(dec, id, dec_num)
model <- ridge.fe[[1]]
ytest <- ridge.fe[[2]]
xtest <- ridge.fe[[3]]

yhat <- predict(model, xtest)
performance(prediction(yhat,ytest),"auc")@y.values[[1]] 

###generate heat plot (fig 3 from paper)

aucz2D <-matrix(runif(25),nrow=5)

nad <- matrix(rep(0,nrow(aucz2D)^2),nrow=nrow(aucz2D))

iszero <- function(x){
  if (x==0){
    NA
  }
  else{
    x
  }
}
for (i in 1:nrow(aucz2D)){
  nad[i,] <- map(iszero,aucz2D[i,])
}



library(gplots)
heatmap.2(nad, dendrogram = "none", trace = "none", 
          Rowv = F, Colv=F, col=rev(rainbow(50,start = .08, end=.25)), 
          tracecol="black", cellnote=round(nad,3),   notecol="black", xlab = "k", ylab = 'j', main = "",srtCol=0)

#random forest for class prediction (we tried this but did not include in the paper itself)
giverData <- buildGiverModel(dec, demo,k=1,K=6, id, dec_num, squares = "false",alpha = 0, nfolds = 5, type= "within", interact=FALSE)
giverData$type <- giverData$high*2 + giverData$low*1

charF <- c()
for (i in 2:nrow(id)-1){
  charF[i] = id[i,] == id[i+1,]
}
charF <- append(charF,TRUE)
charF <- !charF

giverData <-giverData[charF,-c((ncol(giverData)-3):(ncol(giverData)-1))]
library(randomForest)
type <- giverData$type
giverData$type <- NULL

isGreed = as.factor(type == 0)#i) predict "gave 0" vs "gave 1-5 or gave 6+"
isMed = as.factor(type ==1) #ii) predict "gave 1-5" vs "gave 0 or gave 6+"
isCoop = as.factor(type == 2)#iii) predict "gave 6+ vs "gave 0 or gave 1-5
extreme = as.factor(type[which(type != 1)] == 0) #iv) predict "gave 0" vs "gave 6+"
extremeData <- giverData[which(type != 1),]

################

train <- sample( 1:length(type),2*length(type)/3)
greedRF <- randomForest(isGreed ~ ., data=cbind(giverData,isGreed)[train,], importance=TRUE,proximity=TRUE)
greedR <- cv.glmnet(y=isGreed[train],x=as.matrix(giverData[train,]),alpha=0,type.measure="auc",family="binomial")
z <- predict(greedR, as.matrix(giverData[-train,]))
greedAUC <- performance(prediction(as.numeric(z),isGreed[-train]),"auc")@y.values[[1]] 


medRF <- randomForest(isMed ~ ., data=cbind(giverData,isMed)[train,], importance=TRUE,proximity=TRUE)
medR <- cv.glmnet(y=isMed[train],x=as.matrix(giverData[train,]),alpha=0,type.measure="auc",family="binomial")
z <- predict(medR, as.matrix(giverData[-train,]))
medAUC <- performance(prediction(as.numeric(z),isMed[-train]),"auc")@y.values[[1]] 


coopRF <- randomForest(isCoop ~ ., data=cbind(giverData,isCoop)[train,], importance=TRUE,proximity=TRUE)
coopR <- cv.glmnet(y=isCoop[train],x=as.matrix(giverData[train,]),alpha=0,type.measure="auc",family="binomial")
z <- predict(coopR, as.matrix(giverData[-train,]))
coopAUC <- performance(prediction(as.numeric(z),isCoop[-train]),"auc")@y.values[[1]] 

c(greedAUC, medAUC,coopAUC)

###############test giver3
giverz <- giverTriple(k=1,K=6,dec,id,dec_num)
high <- giverz[[1]]
low <- giverz[[2]]

givers <- cbind(high,low)
print("GeneratiXng matrices...")
interactX <-generateSituationSetB(Situation)

X <-model.matrix(~(interactX*givers) )

#######

giverData <- buildGiverModel(dec, Situation,k=1,K=6, id, dec_num, squares = "false",alpha = 0, nfolds = 5, type= "within", interact=FALSE)
giverData$type <- giverData$high*2 + giverData$low*1

giverData <-giverData[charF,-c((ncol(giverData)-3):(ncol(giverData)-1))]

type <- giverData$type
giverData$type <- NULL

isGreed = as.factor(type == 0)#i) predict "gave 0" vs "gave 1-5 or gave 6+"
isMed = as.factor(type ==1) #ii) predict "gave 1-5" vs "gave 0 or gave 6+"
isCoop = as.factor(type == 2)#iii) predict "gave 6+ vs "gave 0 or gave 1-5
extreme = as.factor(type[which(type != 1)] == 0) #iv) predict "gave 0" vs "gave 6+"
extremeData <- giverData[which(type != 1),]

################random forest analysis. not included in paper.

train <- sample( 1:length(type),2*length(type)/3)
greedRF <- randomForest(isGreed ~ ., data=cbind(giverData,isGreed)[train,], importance=TRUE,proximity=TRUE)
z <- predict(greedRF, giverData[-train,])
performance(prediction(as.numeric(z),isGreed[-train]),"auc")@y.values[[1]] 


medRF <- randomForest(isMed ~ ., data=cbind(giverData,isMed)[train,], importance=TRUE,proximity=TRUE)
z <- predict(medRF, giverData[-train,])
medAUC <- performance(prediction(as.numeric(z),isMed[-train]),"auc")@y.values[[1]] 
importance(medRF)


coopRF <- randomForest(isCoop ~ ., data=cbind(giverData,isCoop)[train,], importance=TRUE,proximity=TRUE)
z <- predict(coopRF, giverData[-train,])
coopAUC <- performance(prediction(as.numeric(z),isCoop[-train]),"auc")@y.values[[1]] 
importance(coopRF)

##########comparision of individuals across type. not included in paper
giverData <-giverData[charF,-c(1,48:50)]
gtypeRF_no_interaction

gdprep <- giverData
gdprep$type <- NULL
gdprep <- scale(gdprep)
type1 = gdprep[which(type == 0),]
type2 = gdprep[which(type == 1),]
type3 = gdprep[which(type == 2),]

colMeans(type1)
colMeans(type1)-(qt(0.975,df=nrow(type1)-1)*apply(type1,2,sd)/sqrt(nrow(type1)))

output = matrix(nrow=47, ncol=9)
output[,9] = colMeans(type3)+(qt(0.975,df=nrow(type3)-1)*apply(type3,2,sd)/sqrt(nrow(type3)))
colnames(output) <- c("typeA lower bound","typeA mean","typeA upper bound","typeB lower bound","typeB mean","typeB upper bound","typeC lower bound","typeC mean","typeC upper bound")
rownames(output) <- colnames(type3)

barplot(data, col= grDevices::rainbow(5), ylim = c(.5,.9), xpd=F, ylab="AUC")

###fig 5
cluster_data <- c(0,.6+.2*7.85/41,.8+.2*3.125/41,.8+.2*15/41,.8+.2*17.5/41,.8+.2*20.75/41,.8+.2*23.67/41,.8+.2*25.66/41,.8+.2*26.91/41,.8+.2*25.04/41)
plot(cluster_data, ylab = "Percent Variance Explained", xlab = "Number of Clusters", ylim=c(0,1),pch=16)
lines(cluster_data,col= "chartreuse3")
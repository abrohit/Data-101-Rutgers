library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(MASS)
library(nnet)
library(CrossValidation)

bb <- read.csv("BlackBoxtrainApril22.csv")#Read Csv.
#--------------Numerical-Columns----------------
decision <- rep(0,nrow(bb))

decision[bb$SWITCH == "Minimum"] <- 1
decision[bb$SWITCH == "Medium"] <- 2
decision[bb$SWITCH == "Maximum"] <- 3

bb$SwitchNum <- decision

decision2 <- rep(0,nrow(bb))

decision2[bb$SOUND == "Beep"] <- 1
decision2[bb$SOUND == "Gargle"] <- 2
decision2[bb$SOUND == "Hiss"] <- 3
decision2[bb$SOUND == "Kaboom"] <- 4
decision2[bb$SOUND == "Rumble"] <- 5
decision2[bb$SOUND == "Sizzle"] <- 6

bb$SoundNum <- decision2

#--------------------------------------------------

#------------------Split-Data---------------------

dt = sort(sample(nrow(bb), nrow(bb)*.8))

train <- bb[dt,]
test <- bb[-dt,]

#-------------------------------------------------

#-----------------Linear-Regresion----------------
trainLm <- lm(SoundNum ~ INPUT1 + INPUT2 + INPUT3 + INPUT4 + SWITCH, data = train)
predLm <-  predict(trainLm, newdata = test)

#decision3 <- rep(0, nrow(test))

#decision3[predLm < 0.5] <- 'Tick'
#decision3[predLm >= 0.5 & predLm < 1.5] <- 'Beep'
#decision3[predLm >= 1.5 & predLm < 2.5] <- 'Gargle'
#decision3[predLm >= 2.5 & predLm < 3.5] <- 'Hiss'
#decision3[predLm >= 3.5 & predLm < 4.5] <- 'Kaboom'
#ecision3[predLm >= 4.5 & predLm < 5.5] <- 'Rumble'
#decision3[predLm >= 5.5] <- 'Sizzle'

decision3 <- round(predLm)

error <- mean(test$SoundNum != decision3)
error

cv <- train[, c('INPUT1', 'INPUT2', 'INPUT3', 'INPUT4', 'SWITCH', 'SoundNum')]
cross_validate(cv, trainLm, 5, 0.8)

#---------------------------------------------------

#--------------------Decision-Tree------------------

tree <- rpart(SOUND ~ INPUT1 + INPUT2 + INPUT3 + INPUT4 + SWITCH, data = train, control = rpart.control(cp = .001))
rpart.plot(tree)

predtree <- predict(tree, newdata = test,  type = "class")

error <- mean(test$SOUND != predtree)
error

#cross_validate(train, tree, 5, 0.8)

#-------------------------------------------------

#----------------------LDA------------------------

trainLda <- lda(SOUND ~ INPUT1 + INPUT2 + INPUT3 + INPUT4 + SWITCH, data = train)

predLda <- predict(trainLda, newdata = test)$class

error <- mean(test$SOUND != predLda)
error

cv <- train[, c('INPUT1', 'INPUT2', 'INPUT3', 'INPUT4', 'SWITCH', 'SOUND')]
cross_validate(cv, trainLda, 5, 0.8)

#-------------------------------------------------

#----------------------SVM------------------------

#trainSvm <- svm(SoundNum ~ INPUT1 + INPUT2 + INPUT3 + INPUT4 + SWITCH, data = train)

#predSvm <- predict(trainSvm, newdata = test)

#decision4 <- rep(0, nrow(test))
#decision4 <- round(predSvm)

#error <- mean(test$SoundNum != decision4)
#error

#-------------------------------------------------

#------------------Neural-Net---------------------

trainNn <- nnet(SoundNum / 6 ~ INPUT1 + INPUT2 + INPUT3 + INPUT4 + SWITCH, data = train, size = 5)

predNn <- predict(trainNn, newdata = test) * 6

decision4 <- rep(0, nrow(test))
decision4 <- round(predNn)

error <- mean(test$SoundNum != decision4)
error

cv <- train[, c('INPUT1', 'INPUT2', 'INPUT3', 'INPUT4', 'SWITCH', 'SoundNum')]
cross_validate(cv, trainNn, 5, 0.8)

#-------------------------------------------------

#------------------Naive-Bayes---------------------

train$SoundNum <- factor(train$SoundNum)
trainNb <- naiveBayes(SoundNum ~ INPUT1 + INPUT2 + INPUT3 + INPUT4 + SwitchNum, data = train)

predNb <- predict(trainNn, newdata = test)

decision5 <- rep(0, nrow(test))
decision5 <- round(predNb)

error <- mean(test$SoundNum != decision5)
error

cv <- train[, c('INPUT1', 'INPUT2', 'INPUT3', 'INPUT4', 'SwitchNum', 'SoundNum')]
cross_validate(cv, trainNb, 5, 0.8)
#-------------------------------------------------

#---------------Writing-Test-Data-File------------------

bbPred <- read.csv("BlackBoxTestApril22-students.csv")#Read Csv.

bbPred$SOUND <- predict(tree, newdata = bbPred, type = "class")

Submission <- bbPred[, c('ID', 'SOUND')]

write.csv(Submission, file = "FinalSubmission.csv", row.names=FALSE, quote=FALSE)

#--------------------------------------------------------
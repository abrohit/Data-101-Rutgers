library(rpart)
library(rpart.plot)
library(CrossValidation)

moody <- read.csv("M2021train.csv")#Read Csv.
testMoody <- read.csv("M2021test-students.csv")#Read Csv.

#png(filename="Hello.png")
#dev.off()
#-----Splitting-The-Data--------------------------

dt = sort(sample(nrow(moody), nrow(moody)*.8))

train <- moody[dt,]
test <- moody[-dt,]

#-------------------------------------------------

#-----------------Decision-Tree-Model-1-----------

tree1 <- rpart(Grade ~ ., data = train)
rpart.plot(tree1)

test$NewGrade <- predict(tree1, newdata = test, type = "class")

error <- mean(test$NewGrade != test$Grade)
#error

#--------------------------------------------------

#-----------------Decision-Tree-Model-2-----------

tree2 <- rpart(Grade ~ Attendance + Texting + Score, data = train)
rpart.plot(tree2)

test$NewGrade <- predict(tree2, newdata = test, type = "class")

error <- mean(test$NewGrade != test$Grade)
#error

#--------------------------------------------------

#-----------------Decision-Tree-Model-3-----------

tree3 <- rpart(Grade ~ Major + Questions + Score, data = train)
rpart.plot(tree3)

test$NewGrade <- predict(tree3, newdata = test, type = "class")

error <- mean(test$NewGrade != test$Grade)
#error

#--------------------------------------------------

#-----------------Decision-Tree-Model-4-----------

tree4 <- rpart(Grade ~ Major + Questions + Score, data = train, control = rpart.control(minsplit = 700, minbucket = 50))
rpart.plot(tree4)

test$NewGrade <- predict(tree4, newdata = test, type = "class")

error <- mean(test$NewGrade != test$Grade)
#error

#--------------------------------------------------

#-----------------Cross-Validation-----------------

cross_validate(moody, tree4, 5, 0.8)

#--------------------------------------------------

#---------------Writing-Test-Data-File------------------

testMoody$Grade <- predict(tree1, newdata = testMoody, type = "class")

Submission <- testMoody[, c('Studentid', 'Grade')]

write.csv(Submission, file = "FinalSubmission.csv", row.names=FALSE, quote=FALSE)

#--------------------------------------------------------
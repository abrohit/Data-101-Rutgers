library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)

earnings <- read.csv("Earnings_Train2021.csv")#Read Csv.

#png(filename="Hello.png")
#dev.off()

#Columns: GPA(num), Number_Of_Professional_Connections(Num), Major(Num), Graduation_Year(Time), Height(Num), Number_Of_Credits(Num), Number_Of_Parking_Tickets(Num), Earnings(Num)

stemSub <- subset(earnings, earnings$Major == 'Buisness')

#---------------New-Columns-------------------------

earnings$Involvement <- earnings$Number_Of_Professional_Connections / (2021 - earnings$Graduation_Year)

decision <- rep(0,nrow(earnings))

decision[earnings$Major == "Humanities"] <- 1
decision[earnings$Major == "Vocational"] <- 2
decision[earnings$Major == "Professional"] <- 3
decision[earnings$Major == "Buisness"] <- 4
decision[earnings$Major == "Other"] <- 5

earnings$MajorNum <- decision

#--------------------------------------------------

#---------------Plots--------------------------------

boxplot(earnings$Earnings ~ earnings$Major,
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Majors",
ylab = "Earning")

plot(earnings$Earnings ~ earnings$GPA,
xlab = "GPA",
ylab = "Earning")

plot(stemSub$Earnings ~ stemSub$Number_Of_Professional_Connections,
xlab = "Number of Professional Connections",
ylab = "Earning")

plot(earnings$Earnings ~ earnings$Involvement,
xlab = "Involvement",
ylab = "Experience")

#----------------------------------------------------

#------------------Split-Data------------------------

dt = sort(sample(nrow(earnings), nrow(earnings)*.8))

train <- earnings[dt,]
test <- earnings[-dt,]

#----------------------------------------------------

#---------------Decision-Tree------------------------

tree <- rpart(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Graduation_Year + Height + Number_Of_Credits + Number_Of_Parking_Tickets, data = train, method = "anova")
rpart.plot(tree)

test$NewEarnings <- predict(tree, newdata = test)

error <- mean((test$NewEarnings - test$Earnings)^2)
error

#---------------------------------------------------

#---------------Liner-Regression--------------------

trainLm <- lm(Earnings ~ GPA + MajorNum + Height + Number_Of_Parking_Tickets + Number_Of_Credits + Involvement,  data = train)
predLm <-  predict(trainLm, newdata = test)

error <- mean((predLm - test$Earnings)^2)
error

#---------------------------------------------------

#---------------Random-Forest-----------------------

trainRf <- randomForest(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Graduation_Year + Height + Number_Of_Credits + Number_Of_Parking_Tickets,  data = train)
predRf <-  predict(trainRf, newdata = test)

error <- mean((predRf - test$Earnings)^2)
error

#---------------------------------------------------

#-------------------------SVM-----------------------

trainSvm = svm(Earnings ~ GPA + Number_Of_Professional_Connections + Major + Height + Number_Of_Parking_Tickets + Number_Of_Credits + Graduation_Year, data = train)
predSvm <-  predict(trainSvm, newdata = test)

error <- mean((predSvm - test$Earnings)^2)
error

#---------------------------------------------------

#---------------Submission--------------------------

id <- read.csv("earning_submission.csv")
data <- read.csv("Earnings_Test_Students.csv")
id$Earnings <- predict(trainSvm, newdata = data)

write.csv(id, file = "FinalSubmission.csv", row.names=FALSE, quote=FALSE)

#---------------------------------------------------
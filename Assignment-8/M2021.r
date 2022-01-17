moody <- read.csv("M2021train.csv")#Read Csv.

#png(filename="Hello.png")
#dev.off()

#-----Splitting-The-Data--------------------------

dt = sort(sample(nrow(moody), nrow(moody)*.8))

train <- moody[dt,]
test <- moody[-dt,]

#-------------------------------------------------

#-----------------Stat----------------------

SubP <- subset(train, train$Major == 'Stat')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubP$Attendance ~ SubP$Grade,
main = "Attendance for each Grade attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Attendance")
png(filename="Hello.png")
mosaicplot(SubP$Grade ~ SubP$Questions,
main = "Questions and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Question Attributes")
dev.off()
boxplot(SubP$Score ~ SubP$Grade,
main = "Score for Each Grade Attribute Ssss", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubP$Grade ~ SubP$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

mosaicplot(SubP$Grade ~ SubP$Texting,
main = "Texting and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Texting Attributes")

#-------------------------------------------

#-----------------Polsci----------------------

SubP <- subset(train, train$Major == 'Polsci')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubF$Attendance ~ SubF$Grade,
main = "Attendance for each Grade attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Attendance")

mosaicplot(SubP$Grade ~ SubP$Questions,
main = "Questions and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Question Attributes")

boxplot(SubF$Score ~ SubF$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubF$Grade ~ SubF$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")


mosaicplot(SubF$Grade ~ SubF$Texting,
main = "Texting and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Texting Attributes")

#-------------------------------------------

#-----------------Cs----------------------

SubP <- subset(train, train$Major == 'Cs')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubP$Attendance ~ SubP$Grade,
main = "Attendance for each Grade attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Attendance")

mosaicplot(SubP$Grade ~ SubP$Questions,
main = "Questions and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Question Attributes")

boxplot(SubP$Score ~ SubP$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubP$Grade ~ SubP$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

mosaicplot(SubP$Grade ~ SubP$Texting,
main = "Texting and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Texting Attributes")

#-------------------------------------------

#-----------------Communication----------------------

SubP <- subset(train, train$Major == 'Communication')
SubF <- subset(SubP, SubP$Score < 50)
SubPP <- subset(SubP, SubP$Score >= 50)

boxplot(SubP$Attendance ~ SubP$Grade,
main = "Attendance for each Grade attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Attendance")

mosaicplot(SubP$Grade ~ SubP$Questions,
main = "Questions and Grade Attributes comm", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Question Attributes")

boxplot(SubP$Score ~ SubP$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(SubP$Grade ~ SubP$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

mosaicplot(SubP$Grade ~ SubP$Texting,
main = "Texting and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Texting Attributes")

#------------------------------------------------------

#-----------------Overview-----------------------------

boxplot(train$Attendance ~ train$Grade,
main = "Attendance for each Grade attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Attendance")

mosaicplot(train$Grade ~ train$Major,
main = "Major and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Major Attributes")

mosaicplot(train$Grade ~ train$Questions,
main = "Questions and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Question Attributes")

boxplot(train$Score ~ train$Grade,
main = "Score for Each Grade Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade",
ylab = "Score")

mosaicplot(train$Grade ~ train$Seniority,
main = "Seniority and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Seniority Attributes")

mosaicplot(train$Grade ~ train$Texting,
main = "Texting and Grade Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grade Attributes",
ylab = "Texting Attributes")

#-------------------------------------------------------

#------------------Model--------------------------------

Predict <- test
decision <- rep('Fail',nrow(Predict))

decision[test$Score >= 70] = "Pass"
decision[test$Score < 50 & test$Major != 'Cs' & test$Questions == 'Always' & test$Major != 'Communication'] = "Pass"
decision[test$Score < 50 & test$Questions == 'Always' & test$Major == 'Communication' & test$Score > 42] = "Pass"
decision[test$Score >= 50 & test$Score < 70 & test$Major != "Cs"] = "Pass"
decision[test$Score >= 50 & test$Score < 70 & test$Major == "Cs" & test$Seniority != "Junior" & test$Seniority != "Senior"] = "Pass"

Predict$GradePredict <- decision
error <- mean(Predict$Grade != Predict$GradePredict)
error
#-------------------------------------------------------

#---------------Writing-Test-Data-File------------------

Submission <- read.csv("M2021test-students.csv")#Read Csv.

Predict <- Submission
decision <- rep('Fail',nrow(Predict))

decision[Predict$Score >= 70] = "Pass"
decision[Predict$Score < 50 & Predict$Major != 'Cs' & Predict$Questions == 'Always' & Predict$Major != 'Communication'] = "Pass"
decision[Predict$Score < 50 & Predict$Questions == 'Always' & Predict$Major == 'Communication' & Predict$Score > 42] = "Pass"
decision[Predict$Score >= 50 & Predict$Score < 70 & Predict$Major != "Cs"] = "Pass"
decision[Predict$Score >= 50 & Predict$Score < 70 & Predict$Major == "Cs" & Predict$Seniority != "Junior" & Predict$Seniority != "Senior"] = "Pass"

Predict$Grade <- decision

Submission <- Predict[, c("Studentid", "Grade")]

write.csv(Submission, file = "FinalSubmission.csv", row.names=FALSE, quote=FALSE)

#--------------------------------------------------------
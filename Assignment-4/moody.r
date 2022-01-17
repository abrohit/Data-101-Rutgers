moody <- read.csv("moody2020b.csv")#Read Moody Csv.

#Columns: score(Num),grade(Cat),texting(Cat),questions(Cat),participation(Num)

#png(filename="Hello.png")
#dev.off()

barplot(tapply(moody$score, moody$questions, mean),
main = "Average Score for Each Question Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Question Attributes",
ylab = "Average Score")

neverAskQuestions <- subset(moody, moody$questions == "never")#Subset of students who never ask questions.
rarelyAskQuestions <- subset(moody, moody$questions == "rarely")#Subset of students who rarely ask questions.

neverScore <- neverAskQuestions$score#Stores only the score attribute.
rarelyScore <- rarelyAskQuestions$score#Stores only the score attribute.

neverSD <- sd(neverScore)
rarelySD <- sd(rarelyScore)

neverMean <- mean(neverScore)
rarelyMean <- mean(rarelyScore)

neverLength <- length(neverScore)
rarelyLength <- length(rarelyScore)

bothSD <- sqrt((neverSD^2) / neverLength + (rarelySD^2) / rarelyLength)

Zscore <- (neverMean - rarelyMean) / bothSD

p = (1 - pnorm(Zscore)) / 2
moody <- read.csv("moody2020b.csv")#Read Moody Csv.
sub_df <- subset(moody, grade == 'A')#Subset of students that got an A.

#Columns: score(Num),grade(Cat),texting(Cat),questions(Cat),participation(Num)

#png(filename="Hello.png")
#dev.off()

#--------Intro------------------
boxplot(moody$score ~ moody$grade, 
main = "Score Distribution for Each Grade", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Score")#Score distribution for each letter grade.

barplot(table(moody$grade),
main = "Grade Distribution",
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Number of Students")#Grade distribution.
#-------------------------------

#--------Texting------------------
barplot(table(subset(moody, texting == "always")$grade),
main = "Grade Distribution of Always Texting",
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Number of Students")#Always texting plot.

barplot(table(subset(moody, texting == "never")$grade),
main = "Grade Distribution of Never Texting",
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Number of Students")#Never texting plot.

barplot(table(subset(moody, texting == "sometimes")$grade),
main = "Grade Distribution of Texting Sometimes",
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Number of Students")#Sometimes texting plot.

mosaicplot(sub_df$texting ~ sub_df$grade, 
main="Subset of students who got an A",
col = "green",
xlab = "Texting",
ylab = "Grade")#Subset of grade A against texting.
#----------------------------------

#--------Questions------------------
barplot(table(subset(moody, questions == "always")$grade),
main = "Grade Distribution of Always Asking Questions",
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Number of Students")#Always asking questions plot.

barplot(table(subset(moody, questions == "never")$grade),
main = "Grade Distribution of Never Asking Questions",
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Number of Students")#Never asking questions plot.

barplot(table(subset(moody, questions == "rarely")$grade),
main = "Grade Distribution of Rarely Asking Questions",
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Number of Students")#Rarely asking questions plot.

mosaicplot(sub_df$questions ~ sub_df$grade, 
main="Subset of students who got an A",
col = "purple",
xlab = "Questions",
ylab = "Grade")#Subest of grade A against asking questions.
#----------------------------------

#--------Participation------------------
boxplot(moody$participation ~ moody$grade, 
main = "Score Distribution for Participation", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Grades",
ylab = "Participation")
#---------------------------------------
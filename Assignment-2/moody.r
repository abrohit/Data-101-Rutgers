moody <- read.csv("MOODY-2019.csv")#Read Moody Csv.

#--------------Required---------------------
boxplot(moody$SCORE ~ moody$GRADE, main="Score Distribution for Each Grade")#Score distribution for each letter grade(1).

sub_df <- subset(moody, ASKS_QUESTIONS=="always")#Letter grade frequency distribution of always asking questions(2).
barplot(table(sub_df$GRADE), main="Always Asks Questions", xlab="Grades", ylab="Number of Students")#Display.

sub_df <- subset(moody, ASKS_QUESTIONS=="never")#Letter grade frequency distribution of never asking questions(2).
barplot(table(sub_df$GRADE), main="Never Asks Questions", xlab="Grades", ylab="Number of Students")#Display.

sub_df <- subset(moody, ASKS_QUESTIONS=="sometimes")#Letter grade frequency distribution of asking questions sometimes(2).
barplot(table(sub_df$GRADE), main="Sometimes Asks Questions", xlab="Grades", ylab="Number of Students")#Display.

boxplot(moody$SCORE>80 ~ moody$ON_SMARTPHONE, main="Frequency distribution for Students who Scored More Than 80 Points")#Frequency distribution of attribute  "On Smartphone" for students who scored more than 80 points(3).

Late_in_class <- tapply(moody$SCORE, moody$LATE_IN_CLASS, mean)#Average score for each value of attribute Late_in_class(4).
barplot(Late_in_class, main="Average Score for each attribute of 'LATE_IN_CLASS'", xlab="LATE_IN_CLASS", ylab="Average Score")#Display.

#-------------------------------------------

sub_df <- subset(moody, GRADE == 'A')#Gets subset of all students that achieved an A.

mosaicplot(sub_df$LEAVES_EARLY ~ sub_df$GRADE, main="Subset of students who got an A")#Mosaic graph of students that leave early and got an A grade.

mosaicplot(sub_df$GRADE ~ sub_df$ON_SMARTPHONE, main="Subset of students who got an A")#Mosaic graph of students that are on smartphone and got an A grade.

check <- subset(moody, LEAVES_EARLY=="always" & ASKS_QUESTIONS=="never" & ON_SMARTPHONE=="frequently" & LATE_IN_CLASS=="Yes")#Create a subset of moody with the written parameters.
pie(table(check$GRADE), main="Grades of Students")#Display.
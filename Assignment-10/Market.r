library(PermutationTestManual)#Calls specified library.
library(PermutationTestSecond)#Calls specified library.

market <- read.csv("HomeworkMarket.csv")#Read Csv.

#-----------------Day-Sub-1-------------------------

market$bar <- 0
market$slice <- 0

market[market$Day == "Weekday",]$bar <- 1
market[market$Location == "Princeton" & market$Snacks == "Popcorn",]$slice <- 1

p <- Permutation(market, "slice", "bar", 1000, 0, 1)#0, 1 if blue bar bigger then red.
p

#-----------------------------------------------------

#-----------------Day-Sub-2-------------------------

market$bar <- 0
market$slice <- 0

market[market$Day == "Weekend",]$bar <- 1
market[market$Location == "New Brunswick" & market$Snacks == "Crackers",]$slice <- 1

p <- Permutation(market, "slice", "bar", 1000, 0, 1)
p

#-----------------------------------------------------

#-----------------Day-Sub-3-------------------------

market$bar <- 0
market$slice <- 0

market[market$Day == "Weekend",]$bar <- 1
market[market$Location == "New Brunswick" & market$Snacks == "Pretzels" & market$Beer == "Lager",]$slice <- 1

p <- Permutation(market, "slice", "bar", 1000, 0, 1)
p

#-----------------------------------------------------

#-----------------Location-Sub-1-------------------------

market$bar <- 0
market$slice <- 0

market[market$Location == "Princeton",]$bar <- 1
market[market$Snacks == "Popcorn" & market$SoftDrinks == "Cola",]$slice <- 1

p <- Permutation(market, "slice", "bar", 1000, 0, 1)
p

#-----------------------------------------------------

#-----------------Location-Sub-2-------------------------

market$bar <- 0
market$slice <- 0

market[market$Location == "New Brunswick",]$bar <- 1
market[market$Snacks == "Crackers" & market$Beer == "Lager",]$slice <- 1

p <- Permutation(market, "slice", "bar", 1000, 0, 1)
p

#-----------------------------------------------------

#-----------------Beer-Sub-1-------------------------

market$bar <- 0
market$slice <- 0

market[market$Beer == "Lager",]$bar <- 1
market[market$Snacks == "Potato Chips" & market$Location == "New Brunswick" & market$Wine == "None",]$slice <- 1

p <- Permutation(market, "slice", "bar", 1000, 0, 1)
p

#-----------------------------------------------------

#-----------------Beer-Sub-1-Fix----------------------

market$bar <- 0
market$slice <- 0

market[market$Beer == "Lager",]$bar <- 1
market[market$Snacks == "Potato Chips" & market$Location == "New Brunswick",]$slice <- 1
png(filename="Hello.png")
p <- Permutation(market, "slice", "bar", 1000, 0, 1)
p
dev.off()
#-----------------------------------------------------
gun <- read.csv("Gun_Control.csv")#Read Gun Control Csv.

#Columns: Weapon_Used(Cat), Monetary_Damage(Num), Gun_Laws(Cat), Moon_Phase(Cat), Weather(Cat)

#png(filename="Hello.png")
#dev.off()

library(PermutationTestManual)#Calls specified library.
library(PermutationTestSecond)#Calls specified library.

barplot(tapply(gun$Monetary_Damage, gun$Gun_Laws, mean),
main = "Monetary Damage for Gun Laws Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Monetary Damage")

barplot(table(gun$Gun_Laws),
main = "Number of Crimes for Gun Laws Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Number of Crimes")

boxplot(gun$Monetary_Damage ~ gun$Gun_Laws,
main = "Monetary Damage Done by Each Gun Law Attribute", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Monetary Damage")

onlyGun <- subset(gun, gun$Weapon_Used == 'Gun')
onlyBW <- subset(gun, gun$Weapon_Used == 'Blunt_Weapon')
onlyKnife <- subset(gun, gun$Weapon_Used == 'Knife')
onlyNW <- subset(gun, gun$Weapon_Used == 'No_Weapon')
onlyTG <- subset(gun, gun$Weapon_Used == 'Toy_Gun')

barplot(table(onlyGun$Gun_Laws),
main = "Number of Crimes Using Guns for Gun Laws Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Number of Crimes")

barplot(table(onlyBW$Gun_Laws),
main = "Number of Crimes Using Blunt Weapon for Gun Laws Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Number of Crimes")

barplot(table(onlyKnife$Gun_Laws),
main = "Number of Crimes Using Knives for Gun Laws Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Number of Crimes")

barplot(table(onlyNW$Gun_Laws),
main = "Number of Crimes Using no Weapons for Gun Laws Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Number of Crimes")

barplot(table(onlyTG$Gun_Laws),
main = "Number of Crimes Using Toy Guns for Gun Laws Attributes", 
col = c("#33FFFF", "green", "yellow", "orange", "red"),
xlab = "Gun Laws Attributes",
ylab = "Number of Crimes")

#----------------------------------------------
looseMonetary <- (subset(gun, gun$Gun_Laws == "Loose_Gun_Laws"))$Monetary_Damage
looseMean <- mean(looseMonetary)
looseMean

strictMonetary <- (subset(gun, gun$Gun_Laws == "Strict_Gun_Laws"))$Monetary_Damage
strictMean <- mean(strictMonetary)
strictMean

pValue <- Permutation(gun, "Gun_Laws", "Monetary_Damage", 10000, "Loose_Gun_Laws", "Strict_Gun_Laws")
pValue

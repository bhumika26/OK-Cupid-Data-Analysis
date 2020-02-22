# OKCUPID dataset

#loading data
#install.packages('dplyr')
library(dplyr)
profiles <- read.csv(file = 'profiles.csv', header = TRUE, stringsAsFactors = FALSE)

#to remove the essay columns from the dataset
profiles <- profiles[, -which(substr(names(profiles),1,5) == "essay")]
dim(profiles)
names(profiles)
head(profiles)

n <- nrow(profiles)
m <- ncol(profiles)

#contemplating sex of the users
profiles$sex
table(profiles$sex)
table(profiles$sex)/n
barplot(table(profiles$sex),col='red', xlab = 'sex', ylab = 'count', main='Sex of the users', sub='f = female users , m = male users')

#contemplating sexual orientation of the users
profiles$orientation
table(profiles$orientation)
table(profiles$orientation)/n
barplot(table(profiles$orientation),col='blue', main='Sexual Orientation of the users', border=TRUE, xlab = 'sexual orientation',ylab='count')

#crosstabs between "sex" and the "sexual orientation" via table
table(profiles$orientation,profiles$sex)
table(profiles$orientation,profiles$sex)/n

#using a mosaicplot
png("Sex_Vs_Orientation.png", height = 660,width = 660)
mosaicplot(table(profiles$sex, profiles$orientation), xlab='sex', ylab = 'sexual orientation',
           main=paste("SEX VS ORIENTATION (number of users=",n,")"), color = TRUE, shade = FALSE)
dev.off()

#contemplating Height factor
hist(profiles$height, xlab = "Height (inches)" , main = 'Heights of the Users',
     ylab = 'count', col = 'orange', border = 'black')
profiles$height >= 55
table(profiles$height>=55)
table(profiles$height<=80)
table(profiles$height==72)
table(profiles$height>=55 & profiles$height<=80)
profilessubset <- subset(profiles, profiles$height>=55 & profiles$height<=80)
dim(profiles)
dim(profilessubset)
hist(profilessubset$height,main = "Heights of Users ( between 55 and 80 inches)", xlab="Height (inches)", col='yellow', border = 'black')

#Contemplating Height by gender
males <- subset(profilessubset$height, profilessubset$sex=='m')
females <- subset(profilessubset$height, profilessubset$sex=='f')

par(mar=c(4,4,3,2))
hist(females, col = 'pink', main = paste("Height of Female users"),
     xlab = "Height (inches)", ylab='count')
hist(males, col = 'purple', main = paste("Height of Male users"),
     xlab = 'Height (inches)', ylab='count')

par(mfrow=c(2,1))
hist(females, col = 'pink', xlab='height (inches)', ylab='count')
hist(males, col = 'purple', xlab='height (inches)', ylab='count')

#Comparing heights and sex of users
png("height_vs_sex.png", width = 620, height = 620)
par(mfrow=c(2,1))
hist(females, main="Female Users' Heights", xlab = "Heights (inches)",
     xlim = c(55,80), breaks = 25, col = "red")
hist(males, main="Male Users' Heights", xlab = "Heights (inches)",
     xlim = c(55,80), breaks = 25, col = "blue")
dev.off()

#contemplating age of the users
profiles$age
table(profiles$age)
table(profiles$age)/n
par(mar=c(4,4,2,2))
barplot(table(profiles$age),col='red', xlab = 'age', ylab = 'count', main='Ages of the users')

#contemplating drinking habits
profiles$drinks
table(profiles$drinks)
table(profiles$drinks)/n
par(mar=c(4,4,2,2))
barplot(table(profiles$drinks)/n,col='purple', main='Drnking habits of the users')

#crosstabs between "age" and the "drinking habits" via table
table(profiles$age,profiles$drinks)
table(profiles$age,profiles$drinks)/n
#using a mosaicplot
png("Age_vs_drinkinghabits.png", height = 660,width = 660)
mosaicplot(table(profiles$drinks, profiles$age), ylab = 'age',
           main=paste("AGE VS DRINKING HABITS (number of users=",n,")"), color = TRUE, shade = FALSE)
dev.off()

#contemplating drug use
profiles$drugs
table(profiles$drugs)
table(profiles$drugs)/n
par(mar=c(4,4,2,2))
barplot(table(profiles$drugs)/n,col='purple', main='Drug usage by the users')

#crosstabs between drinkers and drug users via table
table(profiles$drinks,profiles$drugs)
table(profiles$drinks,profiles$drugs)/n
#using a mosaicplot
png("Drugs_vs_Drinks.png", height = 660,width = 660)
mosaicplot(table(profiles$drinks, profiles$drugs),
           main=paste("Drinks and Drug Usage (number of users=",n,")"), color = TRUE, shade = FALSE)
dev.off()

#contemplating status of users
profiles$status
table(profiles$status)
table(profiles$status)/n
par(mar=c(4,4,2,2))
barplot(table(profiles$status)/n,col='purple', main='Relationship status of users',
        xlab= 'current status', ylab='frequency')
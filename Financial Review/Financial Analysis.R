#Dataset
fin <- read.csv(file.choose(),na.strings = c(""))
summary(fin)
str(fin)

#Change non-factor to a factor
fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)
summary(fin)
str(fin)

#sub() and gsub(): Automatically converts Factor into Char
fin$Expenses <- gsub(" Dollars","",fin$Expenses)
fin$Expenses <- gsub(",","",fin$Expenses)
str(fin)

fin$Revenue <- gsub("\\$","",fin$Revenue)
fin$Revenue <- gsub(",","",fin$Revenue)
str(fin)

fin$Growth <- gsub("\\%","",fin$Growth)
str(fin)

#convert char to int
fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
str(fin)
summary(fin)

#Locating Missing Values
complete.cases(fin)
fin[!complete.cases(fin),]
str(fin)

#Filter using which() for non-missing data
fin[which(fin$Revenue == 9746272),]

#Filter using is.na() for non-missing data
fin[is.na(fin$Expenses),]

#Removing records with missing data
fin_backup <- fin
fin <- fin[!is.na(fin$Industry),]

#Resetting the dataframe index
fin
rownames(fin) <- 1:nrow(fin) 
#OR
rownames(fin) <- NULL

#Replace Value: Factual Analysis Method
fin <- fin_backup
fin[!complete.cases(fin),]

fin[is.na(fin$State) & fin$City=="New York","State"] <- "NY"
fin[is.na(fin$State) & fin$City=="San Francisco","State"] <- "CA"
fin

#Replace value: Median Imputation method
fin[!complete.cases(fin),]

median(fin[,"Employees"],na.rm = TRUE)

med_empl_retail <- median(fin[fin$Industry=="Retail","Employees"],na.rm = TRUE)

fin[is.na(fin$Employees) & fin$Industry=="Retail","Employees"] <- med_empl_retail
#Check
fin[3,]

med_empl_fin <- median(fin[fin$Industry=="Financial Services","Employees"],na.rm = TRUE)

fin[is.na(fin$Employees) & fin$Industry=="Financial Services","Employees"] <- med_empl_fin
#check
fin[330,]

med_growth_constr <- median(fin[fin$Industry=="Construction","Growth"],na.rm = TRUE)
fin[is.na(fin$Growth) & fin$Industry=="Construction","Employees"] <- med_growth_constr
#check
fin[8,]
fin[!complete.cases(fin),]

med_revenue_constr <- median(fin[fin$Industry=="Construction","Revenue"],na.rm = TRUE)
fin[is.na(fin$Revenue) & fin$Industry=="Construction","Revenue"] <- med_revenue_constr
#check
fin[42,]
fin[!complete.cases(fin),]

med_exp_constr <- median(fin[fin$Industry=="Construction","Expenses"],na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry=="Construction","Expenses"] <- med_exp_constr
#check
fin[8,]
fin[!complete.cases(fin),]

med_exp_it <- median(fin[fin$Industry=="IT Services","Expenses"],na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry=="IT Services","Expenses"] <- med_exp_it
#check
fin[15,]
fin[!complete.cases(fin),]

fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]
#check
fin[c(8,42),]
fin[!complete.cases(fin),]

fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses),"Revenue"] - fin[is.na(fin$Expenses),"Profit"]
#check
fin[15,]
fin[!complete.cases(fin),]


#Visualization
install.packages("ggplot2")
library(ggplot2)

#scatter plot for Revenue,Expenses, Profit by Industry
p <- ggplot(data = fin)
p + geom_point(aes(x=Revenue,y=Expenses,
                   colour=Industry,size=Profit))

#scatter plot that includes industry trends for expenses
d <- ggplot(data = fin, aes(x=Revenue,y=Expenses,
                            colour=Industry))
d + geom_point() + geom_smooth(fill=NA, size=1.2)

#Boxplot showing growth by industry
f <- ggplot(data = fin, aes(x=Industry,y=Growth,
                            colour=Industry))
f + geom_jitter() +
  geom_boxplot(size=1, alpha=0.5, outlier.color = NA)





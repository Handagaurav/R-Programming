#Expllore Data
util <- read.csv(file.choose())
str(util)
summary(util)
head(util,15)

#Derive Utilization Column
util$Utilization <- 1 - util$Percent.Idle
head(util,15)

#Handling Date-Times in R
tail(util)
#whetehr it is european or American format
?POSIXct
util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y %H:%M")
head(util,15)
summary(util)
#TIP: To rearrange columns in df
util$Timestamp <- NULL
head(util,15)
util <- util[,c(4,1,2,3)]
head(util,15)

#List?
summary(util)
RL1 <- util[util$Machine=="RL1",]
summary(RL1)
RL1$Machine <- factor(RL1$Machine)
summary(RL1)

#Construct List:
#Character: Machine Name
#Vector: (min,max,mean) Utilization for month(excluding unkown hours
#Logical: Has utilization ever fallen below 90%? True/False
util_stats_r1 <- c(min(RL1$Utilization, na.rm = T),
                   mean(RL1$Utilization, na.rm = T),
                   max(RL1$Utilization, na.rm = T))
  
util_stats_r1
util_under_90_flag <- length(which(RL1$Utilization < 0.90)) > 0

list_rl1 <- list("RL1", util_stats_r1, util_under_90_flag)  
list_rl1  
  
#Naming componenets of Lists
list_rl1
names(list_rl1) <- c("Machine","Stats","LowThreshold")
list_rl1

#Another way: Like with dataframes
list_rl1 <- list(Machine="RL1", Stats=util_stats_r1, LowThreshold=util_under_90_flag)
list_rl1

#Extract components of a list
#[]: will always return a list
#[[]]: will always return the actual object
#$: same as [[]] but prettier
list_rl1
list_rl1[1]
list_rl1[[1]]
list_rl1$Machine

list_rl1[2]
typeof(list_rl1[2])

list_rl1[[2]]
typeof(list_rl1[[2]])

list_rl1$Stats
typeof(list_rl1$Stats)

#How would u access 3rd element of the vector
list_rl1
list_rl1[[2]][3]
list_rl1$Stats[3]

#Add and Delete list Components
list_rl1
list_rl1[4] <- "New Information"
list_rl1

#Another way using $
#Vector: All hours where utilizatio is unkown (NA's)
RL1$Utilization
is.na(RL1$Utilization)
RL1[is.na(RL1$Utilization),]
RL1[is.na(RL1$Utilization),"PosixTime"]
list_rl1$UnknownHours <- RL1[is.na(RL1$Utilization),"PosixTime"]
list_rl1

#Remove a componenet
list_rl1[c(4,6,7)] <- NULL

#Add another component
#Dataframe: For this machine
list_rl1$Data <- RL1
list_rl1
summary(list_rl1)
str(list_rl1)

#Subsetting a list
list_rl1
list_rl1$UnknownHours[1]
list_rl1[[4]][1]

#Visulaization
library(ggplot2)
p <- ggplot(data = util)
p + geom_line(aes(x=PosixTime, y=Utilization,
                  color=Machine),size=1.2) +
  facet_grid(Machine~.) +
  geom_hline(yintercept = 0.90,
             color="Gray",size=1.2,
             linetype=3)
myplot <- p + geom_line(aes(x=PosixTime, y=Utilization,
                            color=Machine),size=1.2) +
  facet_grid(Machine~.) +
  geom_hline(yintercept = 0.90,
             color="Gray",size=1.2,
             linetype=3)
list_rl1$Plot <- myplot  
list_rl1  
summary(list_rl1)  
str(list_rl1)  
list_rl1
  
  
  
  






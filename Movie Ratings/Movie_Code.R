#Data
mydata <- read.csv(file.choose())
colnames(mydata) = c("Film","Genre","CriticRating","AudienceRating","BudgetMillions","Year")
summary(mydata)
mydata$Year <- factor(mydata$Year)
str(mydata)

#Visualization
library(ggplot2)

#Plot
p <- ggplot(data = mydata, aes(x=CriticRating, y=AudienceRating,
                          colour=Genre, size=BudgetMillions))
#Scatter
p + geom_point()

#Line
p + geom_line()

#Multiple Layer
p + geom_line() + geom_point()



#Overriding Aesthetics
q <- ggplot(data = mydata, aes(x=CriticRating, y=AudienceRating,
                               colour=Genre, size=BudgetMillions))
q + geom_point(size=1,aes(x=BudgetMillions,size=CriticRating,colour=BudgetMillions)) +
  xlab("Budget Millions $$$")

#Mapping and Setting
r <- ggplot(data = mydata, aes(x=CriticRating, y=AudienceRating,
                               colour=Genre, size=BudgetMillions))
r + geom_point(aes(colour=Genre,size=BudgetMillions))
r + geom_point(colour="DarkGreen",size=1)

#scatterplot
r + geom_point() + facet_grid(Genre~.)
r + geom_point() + facet_grid(.~Year)
r + geom_point() + facet_grid(Genre~Year)
r + geom_point() + 
  facet_grid(Genre~Year) + 
  geom_smooth() +
  coord_cartesian(ylim = c(0,100))

  #Histograms and Density Charts
s <- ggplot(data=mydata , aes(x=BudgetMillions))
s + geom_histogram(binwidth = 10)

#with colour
s + geom_histogram(binwidth = 10, aes(fill=Genre)) 

#add colour and border
snew <- s + geom_histogram(binwidth = 10, aes(fill=Genre), colour="Black")

#add label
snew + 
  xlab("Money Axis")+
  ylab("Number of Movies")+
  ggtitle("Movie Budget Distribution")+
  theme(axis.title.x = element_text(colour="DarkGreen", size=30),
        axis.title.y = element_text(colour="Red", size=30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour="DarkBlue",
                                  size=40,
                                  family="Courier"))

#Density charts
s + geom_density(aes(fill=Genre))
s + geom_density(aes(fill=Genre),position="Stack")

#Starting Layer Tips
t <- ggplot(data=mydata)
t + geom_histogram(bindwidth=30,
                   aes(x=CriticRating),
                   fill="White",colour="Blue")

#Statistical Transformation
?geom_smooth()
u <- ggplot(data=mydata, aes(x=CriticRating, y=AudienceRating,
                             colour=Genre))
u + geom_point() + geom_smooth(fill=NA)

#Boxplots
u <- ggplot(data = mydata, aes(x=Genre, y=AudienceRating,
                               colour=Genre))
u + geom_boxplot(size=1.2) + geom_point(size=0.5)
u + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)

#Facets
v <- ggplot(data=mydata, aes(x=BudgetMillions))
v + geom_histogram(binwidth = 10, aes(fill=Genre),
                   colour="Black") + 
  facet_grid(Genre~.)


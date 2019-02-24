# US-Traffic-Safety
### Libraries
This analysis was done in R  
ggplot2 (graphing)  
gridExtra (displaying multiple plots on same graphic)
```
library(ggplot2) 
library(gridExtra)
```
### Data prep
Data is read in from file
```
data <- read.table(".../data.txt",header=T)
```
Numeric data has commas, shich need to be removed
```
data$Deaths <- gsub(",","",data$Deaths)
data$VMT<- gsub(",","",data$VMT)
data$Population <- gsub(",","",data$Population)
```
Converting variables to numeric
```
data$Deaths <- as.numeric(data$Deaths)
data$VMT <- as.numeric(data$VMT)
data$Population <- as.numeric(data$Population)
```
Converting from people to millions of people for convenience
```
data$Population <- data$Population/10^6
```
### Exploratory Analysis (including R base graphics vs. ggplot2)
#### Plotting variables over time using base R graphics
```
par(mfrow=c(3,2))
plot(data$Year, data$Deaths, 
     type="b",
     xlab="Year",
     ylab="Deaths",
     main="US Motor Vehicle Deaths Over Time",
     pch=16)

plot(data$Year, data$VMT,
     type="b",
     xlab="Year",
     ylab="Miles Traveled (in billions)",
     main="Miles Travelled by Year in the US",
     pch=16)

plot(data$Year, data$Fatalities,
     type="b",
     xlab="Year",
     ylab="Deaths (per 100 million miles travelled)",
     main="Deaths per 100 Million Miles Travelled",
     pch=16)

plot(data$Year, data$Population,
     type="b",
     xlab="Year",
     ylab="Population (in millions)",
     main="US Population Over Time",
     pch=16)

plot(data$Year, data$Fatalities.2,
     type="b",
     xlab="Year",
     ylab="Deaths (per 100,000 people)",
     main="US Motor Vehicle Fatalities Per 100,000 people",
     pch=16)

par(mfrow=c(1,1))
```
#### Plotting variables over time using base ggplot2
```
plot1 <- ggplot(data=data, aes(x=Year, y=Deaths))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="US Motor Vehicle Deaths Over Time",
       x="Year",
       y="Deaths")

plot2 <- ggplot(data=data, aes(x=Year, y=VMT))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="Miles Traveled by Year in the US",
       x="Year",
       y="Miles Travelled (in billions)")

plot3 <- ggplot(data=data, aes(x=Year, y=Fatalities))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="Deaths per 100 Million Miles Travelled",
       x="Year",
       y="Deaths (per 100 million miles travelled)")

plot4 <- ggplot(data=data, aes(x=Year, y=Population))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="US Population Over Time",
       x="Year",
       y="Population (in millions)")

plot5 <- ggplot(data=data, aes(x=Year, y=Fatalities.2))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="US Motor Vehicle Fatalities Per 100,000 people",
       x="Year",
       y="Deaths (per 100,000 people)")


grid.arrange(plot1,plot2,plot3,plot4,plot5, nrow=3)

```
## Base R (all 5 plots)
![Rplot all 5](/Images/Rplot.png)

## ggplot (all 5 plots)
![ggplot all 5](/Images/ggplot02.png)

## Base R (single plot)
![Rplot 1st](/Images/Rplot01.png)

## ggplot (single plot)
![ggplot 1st](/Images/ggplot04.png)

ggplot definitely looks better. There are also more customizations to be made where base R starts to get limited:
```

```

#### Analysis
Create percent change variables for deaths and deaths per 100 million miles driven
```
data$deathPct <- NA
data$deathPerPct <- NA

for (i in 2:(length(data$Deaths)-1)){
  data$deathPct[i] <- ((data$Deaths[i+1]-data$Deaths[i])/data$Deaths[i])*100
}

for (i in 2:(length(data$Fatalities)-1)){
  data$deathPerPct[i] <- ((data$Fatalities[i+1]-data$Fatalities[i])/data$Fatalities[i])*100
}
```
Return table of top negative percent changes
```
topTable <- cbind(head(data[order(data$deathPct),c(1,7)],10),
      head(data[order(data$deathPerPct),c(1,8)],10))
```
Overlay years on top of plots
```
ggplot(data=data, aes(x=Year, y=Deaths))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  geom_vline(xintercept=topTable[,1])+
  labs(title="US Motor Vehicle Deaths Over Time",
       x="Year",
       y="Deaths")

ggplot(data=data, aes(x=Year, y=Fatalities))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  geom_vline(xintercept=topTable[,3])+
  labs(title="Deaths per 100 Million Miles Travelled",
       x="Year",
       y="Deaths (per 100 million miles travelled)")
```
Plots with vertical lines

Analyze top years

#1937:

#DEATHS PER
#1923
#Traffic signals, general awareness of safe vehicle design
#Brakes on all wheels

#1937
#No more sharp/protruding objects on dashboard, padded interior
#First turn signals (buick)
#Automotive Safety Foundation founded
#First highways
#Safety studies

#1941
#Rise of turnpikes and highways in 1940s
#War, dip in miles driven

#1945-46
#More studies

#1973 
#Better crash testing, dummy development


#DEATHS
#1931
#Manufacturers awareness of safety
#Safety glass
#Some seatbelt awareness

#1937
#First highways
#Safety studies
#No more sharp/protruding objects on dashboard, padded interior
#First turn signals (buick)
#Automotive Safety Foundation founded


#1941-42
#Rise of turnpikes and highways in 1940s
#War, dip in miles driven

#1973 
#Better crash testing, dummy development

#1981
#Crash testing being published 2 years before

#1990
#Seat belt laws and awareness
#Air bags becoming widespread

#2008-2009
# Vehicle safety mandates (roof strength)
# Recession


#Analysis of both plots seems lackluster. Deaths per million is so smooth after
#1950s that an analysis of that plot yeilds no results after 1950
```



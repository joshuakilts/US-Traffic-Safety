# Libraries
library(ggplot2)
library(gridExtra)

# Read in data
data <- read.table(".../data.txt",header=T)

# Remove commas from numeric values and convert to numeric
data$Deaths <- gsub(",","",data$Deaths)
data$VMT<- gsub(",","",data$VMT)
data$Population <- gsub(",","",data$Population)

data$Deaths <- as.numeric(data$Deaths)
data$VMT <- as.numeric(data$VMT)
data$Population <- as.numeric(data$Population)

# Population to milions
data$Population <- data$Population/10^6

#Regular plots
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

#Deaths plot
plot1 <- ggplot(data=data, aes(x=Year, y=Deaths))+
  geom_line()+
  geom_point()+
  labs(title="US Motor Vehicle Deaths Over Time",x="Year",y="Deaths")+
  theme_bw()
#ggsave("MVdeaths.png",dpi=300,width=4,height=3)

#Miles travelled plot
plot2 <- ggplot(data=data, aes(x=Year, y=VMT))+
  geom_line()+
  geom_point()+
  labs(title="Miles Traveled by Year in the US",x="Year",y="Miles Travelled (in billions)")+
  theme_bw()
#ggsave("MVmiles.png",dpi=300,width=4,height=3)

#Fatalities per 100 million plot
plot3 <- ggplot(data=data, aes(x=Year, y=Fatalities))+
  geom_line()+
  geom_point()+
  labs(title="Deaths per 100 Million Miles Travelled",x="Year",y="Deaths (per 100 million miles travelled)")+
  theme_bw()
#ggsave("MVfatalitiesPer100mil.png",dpi=300,width=4,height=3)

#Population plot
plot4 <- ggplot(data=data, aes(x=Year, y=Population))+
  geom_line()+
  geom_point()+
  labs(title="US Population Over Time",x="Year",y="Population (in millions)")+
  theme_bw()
#ggsave("USpop.png",dpi=300,width=4,height=3)

#Fatalities per capita plot
plot5 <- ggplot(data=data, aes(x=Year, y=Fatalities.2))+
  geom_line()+
  geom_point()+
  labs(title="US Motor Vehicle Fatalities Per 100,000 people",x="Year",y="Deaths (per 100,000 people)")+
  #geom_vline(xintercept=c(1930,1950,1970,1974))+
  #geom_text(aes(x=2008, label="2008",y=20), angle=270)+
  theme_bw()
#ggsave("MVdeathsPC.png",dpi=300,width=4,height=3)

grid.arrange(plot1,plot2,plot3,plot4,plot5, nrow=3)

# ggplot wins in my book
# customize ggplot

#Deaths plot
plot1 <- ggplot(data=data, aes(x=Year, y=Deaths))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="US Motor Vehicle Deaths Over Time",
       x="Year",
       y="Deaths")
#ggsave("MVdeaths.png",dpi=300,width=4,height=3)

#Miles travelled plot
plot2 <- ggplot(data=data, aes(x=Year, y=VMT))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="Miles Traveled by Year in the US",
       x="Year",
       y="Miles Travelled (in billions)")
#ggsave("MVmiles.png",dpi=300,width=4,height=3)

#Fatalities per 100 million plot
plot3 <- ggplot(data=data, aes(x=Year, y=Fatalities))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="Deaths per 100 Million Miles Travelled",
       x="Year",
       y="Deaths (per 100 million miles travelled)")
#ggsave("MVfatalitiesPer100mil.png",dpi=300,width=4,height=3)

#Population plot
plot4 <- ggplot(data=data, aes(x=Year, y=Population))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="US Population Over Time",
       x="Year",
       y="Population (in millions)")
#ggsave("USpop.png",dpi=300,width=4,height=3)

#Fatalities per capita plot
plot5 <- ggplot(data=data, aes(x=Year, y=Fatalities.2))+
  geom_line(col="darkblue",size=1)+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_x_continuous(breaks=seq(1920,2020,by=10))+
  labs(title="US Motor Vehicle Fatalities Per 100,000 people",
       x="Year",
       y="Deaths (per 100,000 people)")
  
#ggsave("MVdeathsPC.png",dpi=300,width=4,height=3)


grid.arrange(plot1,plot2,plot3,plot4,plot5, nrow=3)


# Create and view top decreases in overall deaths and deaths per 100 million miles
data$deathPct <- NA
data$deathPerPct <- NA



for (i in 2:(length(data$Deaths)-1)){
  data$deathPct[i] <- ((data$Deaths[i+1]-data$Deaths[i])/data$Deaths[i])*100
}

for (i in 2:(length(data$Fatalities)-1)){
  data$deathPerPct[i] <- ((data$Fatalities[i+1]-data$Fatalities[i])/data$Fatalities[i])*100
}

topTable <- cbind(head(data[order(data$deathPct),c(1,7)],10),
      head(data[order(data$deathPerPct),c(1,8)],10))

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

topYears <- rbind(as.matrix(topTable[,1]),as.matrix(topTable[,3]))
sort(topYears)

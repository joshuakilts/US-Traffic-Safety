# US-Traffic-Safety
```
library(ggplot2)


data <- read.table(".../data.txt",header=T)
data$Deaths <- gsub(",","",data$Deaths)
data$VMT<- gsub(",","",data$VMT)
data$Population <- gsub(",","",data$Population)

data$Deaths <- as.numeric(data$Deaths)
data$VMT <- as.numeric(data$VMT)
data$Population <- as.numeric(data$Population)

data$Population <- data$Population/10^6


plot(data$Year, data$Deaths, type="b")
plot(data$Year, data$VMT, type="b")
plot(data$Year, data$Fatalities, type="b")
plot(data$Year, data$Population, type="b")
plot(data$Year, data$Fatalities.2, type="b")
plot(data$Year, data$pctChange, type="b")


lineTheme <- function(textSize){
  
  ##Formats ggplot charts
  library(RColorBrewer)
  library(scales)
  library(grid)
  
  #picking color palettes
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  #start constructing chart
  theme_bw(base_size = 9)+
    
    #set entire region color
    theme(panel.background=element_rect(fill=color.background,color=color.background))+
    theme(plot.background=element_rect(fill=color.background,color=color.background))+
    theme(panel.border=element_rect(color=color.background))+
    
    #set grid colors 
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25))+
    theme(panel.grid.minor=element_blank())+
    theme(axis.ticks=element_blank())+
    
    #format legend (turned off by default)
    theme(legend.position="none")+
    theme(legend.background = element_rect(fill=color.background))+
    theme(legend.text = element_text(size=7,color=color.axis.title))+
    
    #Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    #Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))+
    
    #centers main title  
    theme(plot.title = element_text(hjust = 0.5, size = textSize))
}

#Deaths plot
ggplot(data=data, aes(x=Year, y=Deaths))+
  geom_line()+
  geom_point()+
  labs(title="US Motor Vehicle Deaths by Year",x="Year",y="Deaths")+
  lineTheme(14)
ggsave("MVdeaths.png",dpi=300,width=4,height=3)

#Miles travelled plot
ggplot(data=data, aes(x=Year, y=VMT))+
  geom_line()+
  geom_point()+
  labs(title="Miles Traveled by Year in the US",x="Year",y="Miles Travelled (in billions)")+
  lineTheme(14)
ggsave("MVmiles.png",dpi=300,width=4,height=3)

#Fatalities per 100 million plot
ggplot(data=data, aes(x=Year, y=Fatalities))+
  geom_line()+
  geom_point()+
  labs(title="US Motor Vehicle Deaths per 100 Million Miles Driven",x="Year",y="Deaths (per 100 million miles driven)")+
  lineTheme(14)
ggsave("MVfatalitiesPer100mil.png",dpi=300,width=4,height=3)

#Population plot
ggplot(data=data, aes(x=Year, y=Population))+
  geom_line()+
  geom_point()+
  labs(title="US Population Over Time",x="Year",y="Population (in millions)")+
  lineTheme(14)
ggsave("USpop.png",dpi=300,width=4,height=3)

#Fatalities per capita plot
ggplot(data=data, aes(x=Year, y=Fatalities.2))+
  geom_line()+
  geom_point()+
  labs(title="US Motor Vehicle Deaths per Capita",x="Year",y="Deaths (per capita)")+
  geom_vline(xintercept=c(1930,1950,1970,1974))+
  #geom_text(aes(x=2008, label="2008",y=20), angle=270)+
  lineTheme(14)
ggsave("MVdeathsPC.png",dpi=300,width=4,height=3)

#pctChange plot
ggplot(data=data, aes(x=Year, y=pctChange))+
  geom_line()+
  geom_point()+
  labs(title="US Motor Vehicle Deaths per Capita",x="Year",y="Deaths (per capita)")+
  #geom_vline(xintercept=c(1930,1950,1970,1974))+
  #geom_text(aes(x=2008, label="2008",y=20), angle=270)+
  lineTheme(14)
ggsave("MVdeathsPC.png",dpi=300,width=4,height=3)



# Population and deaths
cor(data$Population,data$Deaths)
# 0.5097535
cor(subset(data$Population,data$Year>1972),subset(data$Deaths,data$Year>1972))
# -0.8306276
cor(subset(data$Population,data$Year<1972),subset(data$Deaths,data$Year<1972))
# 0.8732502


cor(subset(data$VMT,data$Year>1972),subset(data$Deaths,data$Year>1972))

cor(subset(data$VMT,data$Year<1972),subset(data$Deaths,data$Year<1972))
```

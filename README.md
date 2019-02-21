# US-Traffic-Safety

library(ggplot2)


data <- read.table(header=T, text="
Year	Deaths  VMT Fatalities	Population	Fatalities.2 pctChange
                   1921	13,253	55.03	  24.09	108,538,000	12.210	6.95
                   1922	14,859	67.70	  21.95	110,049,000	13.502	10.58
                   1923	17,870	85.00	  21.03	111,947,000	15.963	18.22
                   1924	18,400	104.84	17.55	114,109,000	16.125	1.01
                   1925	20,771	122.35	16.98	115,829,000	17.932	11.21
                   1926	22,194	140.74	15.77	117,397,000	18.905	5.42
                   1927	24,470	158.45	15.44	119,035,000	20.557	8.74
                   1928	26,557	172.86	15.36	120,509,000	22.037	7.20
                   1929	29,592	197.72	14.97	121,767,000	24.302	10.28
                   1930	31,204	206.32	15.12	123,076,741	25.353	4.33
                   1931	31,963	216.15	14.79	124,039,648	25.768	1.64
                   1932	27,979	200.52	13.95	124,840,471	22.412	-13.03
                   1933	29,746	200.64	14.83	125,578,763	23.687	5.69
                   1934	34,240	215.56	15.88	126,373,773	27.094	14.38
                   1935	34,494	228.57	15.09	127,250,232	27.107	0.05
                   1936	36,126	252.13	14.33	128,053,180	28.212	4.07
                   1937	37,819	270.11	14.00	128,824,829	29.357	4.06
                   1938	31,083	271.18	11.46	129,824,939	23.942	-18.44
                   1939	30,895	285.40	10.83	130,879,718	23.606	-1.41
                   1940	32,914	302.19	10.89	132,122,446	24.912	5.53
                   1941	38,142	333.61	11.43	133,402,471	28.592	14.77
                   1942	27,007	268.22	10.07	134,859,553	20.026	-29.96
                   1943	22,727	208.19	10.92	136,739,353	16.621	-17.00
                   1944	23,165	212.71	10.89	138,397,345	16.738	0.71
                   1945	26,785	250.17	10.71	139,928,165	19.142	14.36
                   1946	31,874	340.88	9.35	141,388,566	22.544	17.77
                   1947	31,193	370.89	8.41	144,126,071	21.643	-4.00
                   1948	30,775	397.96	7.73	146,631,302	20.988	-3.03
                   1949	30,246	424.46	7.13	149,188,130	20.274	-3.40
                   1950	33,186	458.25	7.24	152,271,417	21.794	7.50
                   1951	35,309	491.09	7.19	154,877,889	22.798	4.61
                   1952	36,088	513.58	7.03	157,552,740	22.905	0.47
                   1953	36,190	544.43	6.65	160,184,192	22.593	-1.36
                   1954	33,890	561.96	6.03	163,025,854	20.788	-7.99
                   1955	36,688	605.65	6.06	165,931,202	22.110	6.36
                   1956	37,965	627.84	6.05	168,903,031	22.477	1.66
                   1957	36,932	647.00	5.71	171,984,130	21.474	-4.46
                   1958	35,331	664.65	5.32	174,881,904	20.203	-5.92
                   1959	36,223	700.48	5.17	177,829,628	20.369	0.83
                   1960	36,399	718.76	5.06	180,671,158	20.147	-1.09
                   1961	36,285	737.42	4.92	183,691,481	19.753	-1.95
                   1962	38,980	766.73	5.08	186,537,737	20.897	5.79
                   1963	41,723	805.25	5.18	189,241,798	22.047	5.51
                   1964	45,645	846.30	5.39	191,888,791	23.787	7.89
                   1965	47,089	887.81	5.30	194,302,963	24.235	1.88
                   1966	50,894	925.90	5.50	196,560,338	25.892	6.84
                   1967	50,724	964.01	5.26	198,712,056	25.526	-1.41
                   1968	52,725	1,015.87	5.19	200,706,052	26.270	2.91
                   1969	53,543	1,061.79	5.04	202,676,946	26.418	0.56
                   1970	52,627	1,109.72	4.74	205,052,174	25.665	-2.85
                   1971	52,542	1,178.81	4.46	207,660,677	25.302	-1.42
                   1972	54,589	1,259.79	4.33	209,896,021	26.008	2.79
                   1973	54,052	1,313.11	4.12	211,908,788	25.507	-1.92
                   1974	45,196	1,280.54	3.53	213,853,928	21.134	-17.14
                   1975	44,525	1,327.66	3.35	215,973,199	20.616	-2.45
                   1976	45,523	1,402.38	3.25	218,035,164	20.879	1.27
                   1977	47,878	1,467.03	3.26	220,239,425	21.739	4.12
                   1978	50,331	1,544.70	3.26	222,584,545	22.612	4.02
                   1979	51,093	1,529.13	3.34	225,055,487	22.702	0.40
                   1980	51,091	1,527.30	3.35	227,224,681	22.485	-0.96
                   1981	49,301	1,552.80	3.18	229,465,714	21.485	-4.45
                   1982	43,945	1,595.01	2.76	231,664,458	18.969	-11.71
                   1983	42,589	1,652.79	2.58	233,791,994	18.217	-3.97
                   1984	44,257	1,720.27	2.57	235,824,902	18.767	3.02
                   1985	43,825	1,774.18	2.47	237,923,795	18.420	-1.85
                   1986	46,087	1,834.87	2.51	240,132,887	19.192	4.19
                   1987	46,390	1,921.20	2.42	242,288,918	19.147	-0.24
                   1988	47,087	2,025.96	2.32	244,498,982	19.259	0.58
                   1989	45,582	2,096.46	2.17	246,819,230	18.468	-4.11
                   1990	44,599	2,144.36	2.08	249,464,396	17.878	-3.19
                   1991	41,508	2,172.05	1.91	252,153,092	16.461	-7.92
                   1992	39,250	2,247.15	1.75	255,029,699	15.390	-6.51
                   1993	40,150	2,296.38	1.75	257,782,608	15.575	1.20
                   1994	40,716	2,358	1.73	260,327,021	15.6403	0.42
                   1995	41,817	2,423	1.73	262,803,276	15.9119	1.74
                   1996	42,065	2,486	1.69	265,228,572	15.8599	-0.33
                   1997	42,013	2,562	1.64	267,783,607	15.6892	-1.08
                   1998	41,501	2,632	1.58	270,248,003	15.3566	-2.12
                   1999	41,717	2,691	1.55	272,690,813	15.2983	-0.38
                   2000	41,945	2,747	1.53	282,216,952	14.8627	-2.85
                   2001	42,196	2,797	1.51	285,226,284	14.794	-0.46
                   2002	43,005	2,856	1.51	288,125,973	14.926	0.89
                   2003	42,884	2,890	1.48	290,796,023	14.747	-1.75
                   2004	42,836	2,965	1.44	293,638,158	14.588	-0.52
                   2005	43,510	2,989	1.46	296,507,061	14.674	0.44
                   2006	42,708	3,014	1.42	299,398,484	14.265	-2.79
                   2007	41,259	3,031	1.36	301,139,947	13.701	-3.85
                   2008	37,423	2,977	1.26	303,824,640	12.317	-11.0
                   2009	33,883	2,957	1.15	306,700,000	11.048	-9.7
                   2010	32,999	2,967	1.11	309,326,000	10.668	-3.5
                   2011	32,479	2,950	1.10	311,588,000	10.42	-2.3
                   2012	33,782	2,969	1.14	313,914,000	10.75	2.6
                   2013	32,893	2,988	1.10	316,129,000	10.40	-3.3
                   2014	32,744	3,026	1.08	318,860,000	10.28	-0.9
                   2015	35,485	3,095	1.15	321,370,000	11.06	10.5
                   2016	37,806	3,174	1.19	323,121,000	11.59	5.6
                   2017	37,133	3,213	1.16	325,719,000	11.40	-1.8")


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

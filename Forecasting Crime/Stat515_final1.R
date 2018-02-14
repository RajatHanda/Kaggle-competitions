library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(viridis)
library(plotly)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)
library(ggthemes)
mydata <- read.csv("/Users/stem/Desktop/Terr/crime.csv")

newdata <- mydata[ which(mydata$Text_General_Code=='Narcotic / Drug Law Violations' & mydata$Year=='2015'), ]
ndata<-mydata[ which(mydata$Text_General_Code=='Narcotic / Drug Law Violations' & mydata$Year=='2016'), ]
m1<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = newdata, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = newdata, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("2015_Narcotic / Drug Law Violations")+theme_solarized(light = TRUE)
m2<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = ndata, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = ndata, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("2016_Narcotic / Drug Law Violations")+theme_solarized(light = TRUE)
grid.arrange(m1,m2,ncol=2)
newdata2 <- mydata[ which(mydata$Text_General_Code=='Thefts' & mydata$Year=='2015'), ]
newdata3 <- mydata[ which(mydata$Text_General_Code=='Vandalism/Criminal Mischief' & mydata$Year=='2015'), ]
newdata4 <- mydata[ which(mydata$Text_General_Code=='Theft from Vehicle' & mydata$Year=='2015'), ]
newdata5 <- mydata[ which(mydata$Text_General_Code=='Narcotic / Drug Law Violations' & mydata$Year=='2015'), ]
library(ggmap)
library(ggplot2)
library(tidyr)
library(devtools)
devtools::install_github("dkahle/ggmap")
devtools::install_github("hadley/ggplot2")
Myd <- mydata %>% group_by(Text_General_Code)
chi_map <- get_map(location = "Philadelphia", maptype = "roadmap", zoom = 11)
m1<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = newdata, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = newdata, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("2015_All Other Offenses")+theme_solarized(light = TRUE)
m2<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = newdata1, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = newdata1, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("2015_Other Assaults")+theme_solarized(light = TRUE)
m3<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = newdata2, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = newdata2, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("2015_Thefts")+theme_solarized(light = TRUE)
m4<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = newdata3, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = newdata3, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("Vandalism/Criminal Mischief")+theme_solarized(light = TRUE)
m5<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = newdata4, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = newdata4, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("2015_Theft from Vehicle")+theme_solarized(light = TRUE)
m6<-ggmap(chi_map, extent = "device") + 
  geom_density2d(data = newdata5, aes(x = Lon, y = Lat), size = 0.3) + 
  stat_density2d(data = newdata5, aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)+ggtitle("2015_Narcotic / Drug Law Violations")+theme_solarized(light = TRUE)
grid.arrange(m1,m2,m3,m4,m5,m6,nrow=3,ncol=3)
dim(mydata)
summary(mydata)
str(mydata)
table(is.na(mydata))

mydata <- separate(mydata, col = Month, into = c("Year", "Month"), sep = "-")
mydata$Year <- as.factor(mydata$Year)
mydata$Month <- as.factor(mydata$Month)
mydata$Dispatch_Date <- as.Date(mydata$Dispatch_Date)
mydata$Day <- day(mydata$Dispatch_Date)

by_date <- mydata %>% group_by(Dispatch_Date) %>% dplyr::summarise(Total = n())

a1<-ggplot(by_date, aes(Dispatch_Date, Total, color = Dispatch_Date)) + geom_line(color="BROWN")+theme_solarized(light = TRUE)
#----------- 
by_hour <- mydata %>% 
  group_by(Hour) %>% 
  dplyr::summarise(Total = n())
by_hour

theme(axis.title.x = element_text(colour = "white"),axis.title.y = element_text(colour = "white"),title = element_text(colour = "white"))+
by_day <- mydata %>% 
  group_by(Day) %>% 
  dplyr::summarise(Total = n())
by_day

ggplot(by_day, aes(Day, Total, color = Day)) + 
  geom_line() + 
  ggtitle("Crimes By Day") + 
  xlab("Day of the Month") + 
  ylab("Total Crimes") 


by_month <- mydata %>% 
  group_by(Month) %>% 
  dplyr::summarise(Total = n())

by_month$Percent <- by_month$Total/dim(mydata)[1] * 100
by_month

a2<-ggplot(by_month, aes(Month, Total, fill = Month)) + 
  geom_bar(stat = "identity") + theme_solarized(light = TRUE)+
  ggtitle("Crimes By Month") + 
  xlab("Month") + 
  ylab("Count") + 
  theme(legend.position = "none")+geom_text(aes(label=Total), vjust=1.6, color="white", size=3.5)
by_year <- mydata %>% 
  group_by(Year) %>% 
  dplyr::summarise(Total = n())
by_year$Percent <- by_year$Total/dim(mydata)[1] * 100
by_year

a1<-ggplot(by_year, aes(Year, Total, fill = Year)) + 
  geom_bar(stat = "identity")+
  ggtitle("Crimes By Year ") + 
  xlab("Year") + ylab("Count") + 
  theme(legend.position = "none")+theme_solarized(light = TRUE)+geom_text(aes(label=Total), vjust=1.6, color="white", size=3.5)
grid.arrange(a2,a1,nrow=2)
ggplot(by_year, aes(Year, Total, color = Year)) + 
  geom_line() + 
  ggtitle("Crimes By Day") + 
  xlab("Day of the Month") + 
  ylab("Total Crimes")


by_hour_year <- mydata %>% 
  group_by(Year,Hour) %>%
  dplyr::summarise(Total = n())

ggplot(by_hour_year, aes(Hour, Total, color = Year)) + 
  geom_line(size = 1) + theme_solarized(light = TRUE)+
  ggtitle("Crimes By Year and Hour") + 
  xlab("Hour of the Day") + 
  ylab("Total Crimes") 

by_hour_month <- mydata %>% 
  group_by(Month,Hour) %>% 
  dplyr::summarise(Total = n())

g1<-ggplot(by_hour_month, aes(Hour, Total, color = Month)) + 
  geom_line(size = 1) + theme_solarized(light = TRUE)+
  ggtitle("Crimes By Month and Hour") + 
  xlab("Hour of the Day") + 
  ylab("Total Crimes") 

by_month_day <- mydata %>% 
  group_by(Month, Day) %>% 
  dplyr::summarise(Total = n())
library(gridExtra)
g1<-ggplot(by_month_day, aes(Day, Total, color = Month)) + 
  geom_line(size = 2) + 
  ggtitle("Crimes By Month and Day") + 
  xlab("Year") + 
  ylab("Count")


by_month_year <- mydata %>% 
  group_by(Year, Month) %>% 
  dplyr::summarise(Total = n())



g2<-ggplot(by_month_year, aes(Year, Month, fill = Total)) + 
  geom_tile(color = "white") + theme_solarized(light = FALSE) +
  ggtitle("Crimes By Year and Month") + 
  xlab("Year") + 
  ylab("Month") 

grid.arrange(g1,g2,ncol=2)

by_code_year <- mydata %>% group_by(Year, Text_General_Code) %>% 
  dplyr::summarise(Total = n())

by_code_year[1:10,]

S1<-ggplot(by_code_year, aes(reorder(Text_General_Code, Total), Total, fill = Year)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,450000,50000)) + 
  coord_flip() + ggtitle("COUNT OF VARIETIES OF CRIME FOR EACH YEAR ") + 
  xlab("TYPE OF CRIME") + 
  ylab("COUNT")+theme_solarized(light = TRUE)
by_code_month <- mydata %>% 
  group_by(Month, Text_General_Code) %>% 
  dplyr::summarise(Total = n())

by_code_month[1:10,]

S2<-ggplot(by_code_month, aes(reorder(Text_General_Code, Total), Total, fill = Month)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,450000,50000)) + 
  coord_flip() + 
  ggtitle("COUNT OF VARIETIES OF CRIME FOR EACH MONTH") + 
  xlab("TYPE OF CRIME") + 
  ylab("COUNT")+theme_solarized(light = TRUE)

grid.arrange(S1,S2,ncol=2)

-------------------------------
phildata <- fread("/Users/stem/Desktop/Terr/crime.csv")


# summarise latitude and longitude data
summary(phildata$Lon)
summary(phildata$Lat)


# checkin for NAs
sapply(phildata, function(x) sum(is.na(x)))
# note, even with 16722 NAs (missing values), it is less than ~0.8% of missing data.


# Summarise the data for crime categories
summary(phildata$Text_General_Code)

# checking for date time manipulation
phildata$dt = as.Date(phildata$Dispatch_Date)
phildata$year = as.numeric(format(phildata$dt, "%Y"))
phildata$mth = as.numeric(format(phildata$dt, "%m"))
phildata$day = as.numeric(format(phildata$dt, "%d"))
library(sqldf)

# Group crime by dates and Police Districts
crimephilly = sqldf("select Police_Districts as 'District', 
                    day, year, mth, Hour,
                    Text_General_Code as 'Category', 
                    count(*) as 'count' 
                    from phildata
                    group by Police_Districts, day, year, mth, Hour, Text_General_Code")
s3<-ggplot(data=crimephilly, aes(x=year)) +   geom_bar(colour="black", fill="brown") +
  ylab('Count') +   facet_wrap(~Category)+theme_solarized(light = TRUE)
grid.arrange(S1,s3,ncol=2)
crime2016 <- subset(crimephilly, year == 2016)
crime2015 <- subset(crimephilly, year == 2015)
# Crime chart for year 2014
ggplot(data=crime2016, aes(x=District)) +
  geom_bar(colour="black", fill="brown") +
  ylab('Count') +
  facet_wrap(~Category) +
  ggtitle("Crime Report 2016")+theme_solarized(light = TRUE)
ggplot(data=crime2015, aes(x=District)) +
  geom_bar(colour="black", fill="brown") +
  ylab('Count') +
  facet_wrap(~Category) +
  ggtitle("Crime Report 2015")+theme_solarized(light = TRUE)
grid.arrange(f2,f1,ncol=2)

dc_by_crime <- mydata  %>% 
  group_by(Dc_Dist, Text_General_Code) %>% 
  dplyr::summarise(Total = n()) %>% 
  arrange(desc(Total)) %>% top_n(n = 1)

dc_by_crime <- as.data.frame(dc_by_crime)
dc_by_crime$Dc_Dist <- factor(dc_by_crime$Dc_Dist)
dc_by_crime$Text_General_Code <- factor(dc_by_crime$Text_General_Code)

ggplot(dc_by_crime, aes(Dc_Dist, Total, fill = Text_General_Code)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top Crime by District Police HeadQuarters") + 
  xlab("District Police HeadQuarters") + 
  ylab("Total") 

crime_by_psa <- mydata  %>% 
  group_by(Psa, Text_General_Code) %>% 
  dplyr::summarise(Total = n()) %>% 
  arrange(desc(Total)) %>% top_n(n = 1)

crime_by_psa <- as.data.frame(crime_by_psa)
crime_by_psa$Psa <- factor(crime_by_psa$Psa)
crime_by_psa$Text_General_Code <- factor(crime_by_psa$Text_General_Code)

head(crime_by_psa)
ggplot(crime_by_psa, aes(Psa, Total, fill = Text_General_Code)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top crime in Every Police Service Area") + 
  xlab("Police Service Area") + 
  ylab("Total")
orig_philly_crime<-read.csv("/Users/stem/Desktop/Terr/crime.csv")
philly_crime_year <- orig_philly_crime %>% 
  separate(Dispatch_Date, c("year", "date"), sep="-") %>%
  select(year, Text_General_Code) %>% mutate(num_crime = 1) %>%
  group_by(year) %>% 
  summarise(freq = sum(num_crime)) 
philly_crime_year$year <- as.integer(philly_crime_year$year)
library(zoo)
ggplot(philly_crime_year, aes(x=year, y = freq)) +
  geom_point(col="darkgreen") + 
  geom_smooth(se=FALSE, col="brown") +
  scale_y_continuous("Frequency",labels = comma) + 
  scale_x_continuous("Year") +
  ggtitle("Crime over the past 10 years")+theme_solarized(light = TRUE)
month1 <- mydata %>%
  filter(Text_General_Code == "All Other Offenses" | Text_General_Code == "Thefts" |
           Text_General_Code == "Narcotic / Drug Law Violations" | Text_General_Code == "Theft from Vehicle"|Text_General_Code == "Other Assaults"|Text_General_Code == "Vandalism/Criminal Mischief") %>%
  group_by(Text_General_Code, Month) %>%summarise(n = n())
month1$Month <- as.yearmon(month1$Month)
ggplot(month1, aes(as.Date(Month), n)) +
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  labs(x = "Year", y = "No. of Crimes", title = "Violent Crime in Philadelphia, 2006-2017") +
  facet_wrap( ~ Text_General_Code, scales = "free")+theme_solarized(light = TRUE)

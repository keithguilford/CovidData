library(dplyr)
library(ggplot2)
library(sf)
library(scales)
library(leaflet)
library(RColorBrewer)

setwd('C:/Users/phaedrus/Documents/R Projects/CovidMaps')

url <- c("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv?_sm_au_=iVVqLPMMPWSDkHHq6jkjvKQBQqvNG")
counties <- read.csv(url)
rm(url)

counties$county <- as.factor(counties$county)
counties$state  <- as.factor(counties$state)
counties$fips <- as.numeric(counties$fips)
counties$date <- as.Date(counties$date)

counties <- counties %>% group_by(state,county)%>%
  mutate (newcases = cases- lag(cases),
          newdeaths = deaths - lag(deaths))%>%
  ungroup()



counties$newcases <- ifelse(is.na(counties$newcases)==TRUE, 0,counties$newcases)
counties$newdeaths <- ifelse(is.na(counties$newdeaths)==TRUE, 0,counties$newdeaths)



countyShp<-st_read('UScounties.shp')


FipsCases14Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
    tempDF <- counties %>%
    filter(fips == i, date >= max(date)-14) %>%
    select(date,fips,newcases)
  slope<-lm(newcases ~ date, data=tempDF)$coefficients[2]
  df<-data.frame(fips = i, slope=slope)
  FipsCases14Slope <-rbind(FipsCases14Slope, df,make.row.names = FALSE)
}

rm(df,i,slope,tempDF)

FipsDeaths14Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
  tempDF <- counties %>%
    filter(fips == i, date >= max(date)-14) %>%
    select(date,fips,newdeaths)
  slope<-lm(newdeaths ~ date, data=tempDF)$coefficients[2]
  df<-data.frame(fips = i, slope=slope)
  FipsDeaths14Slope <-rbind(FipsDeaths14Slope, df,make.row.names = FALSE)
}

rm(df,i,slope,tempDF)

FipsCases28Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
  tempDF <- counties %>%
    filter(fips == i, date >= max(date)-28) %>%
    select(date,fips,newcases)
  slope<-lm(newcases ~ date, data=tempDF)$coefficients[2]
  df<-data.frame(fips = i, slope=slope)
  FipsCases28Slope <-rbind(FipsCases28Slope, df,make.row.names = FALSE)
}

rm(df,i,slope,tempDF)

FipsDeaths28Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
  tempDF <- counties %>%
    filter(fips == i, date >= max(date)-28) %>%
    select(date,fips,newdeaths)
  slope<-lm(newdeaths ~ date, data=tempDF)$coefficients[2]
  df<-data.frame(fips = i, slope=slope)
  FipsDeaths28Slope <-rbind(FipsDeaths28Slope, df,make.row.names = FALSE)
}

rm(df,i,slope,tempDF)

# summary(FipsCasesSlope$slope)
# ggplot(FipsDeathsSlope,aes(x=slope))+
#   geom_histogram(binwidth = .1)+
#   scale_x_continuous(limits=c(-2,2))


FipsCases14Slope$outslopeCases14<-ifelse(FipsCases14Slope$slope < -1 , -1,
                                ifelse(FipsCases14Slope$slope > 1, 1, FipsCases14Slope$slope))

FipsDeaths14Slope$outslopedeaths14<-ifelse(FipsDeaths14Slope$slope < -1 , -1,
                                ifelse(FipsDeaths14Slope$slope > 1, 1, FipsDeaths14Slope$slope))

FipsCases28Slope$outslopecases28<-ifelse(FipsCases28Slope$slope < -1 , -1,
                                  ifelse(FipsCases28Slope$slope > 1, 1, FipsCases28Slope$slope))

FipsDeaths28Slope$outslopedeaths28<-ifelse(FipsDeaths28Slope$slope < -1 , -1,
                                   ifelse(FipsDeaths28Slope$slope > 1, 1, FipsDeaths28Slope$slope))

#previewColors(colorBin("RdYlGn", domain = FipsCasesSlope$outslope,bins=10,reverse = TRUE),sort(unique(FipsCasesSlope$outslope)))
CasesPal<-colorBin("RdYlGn", domain = FipsCases14Slope$outslopeCases14,bins=10,reverse=TRUE)

DeathsPal<-colorBin("RdYlGn", domain = FipsDeaths28Slope$outslopedeaths28,bins=8,reverse=TRUE)



summaryStats <- counties%>%
  group_by(fips,county,state)%>%
  summarise(TotalCases = max(cases),
            TotalDeaths = max(deaths))%>%
  left_join(FipsCases14Slope,by=c("fips"))%>%
  left_join(FipsDeaths14Slope,by=c("fips"))%>%
  left_join(FipsCases28Slope,by=c("fips"))%>%
  left_join(FipsDeaths28Slope,by=c("fips"))

countyShp$fips<-as.integer(as.character(countyShp$FIPS))

countyShpJ<-countyShp%>%
  left_join(summaryStats,by="fips")%>%
  na.omit(fips)



leaflet(countyShpJ)%>%
  #addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,group = 'Deaths28',
              fillColor = ~DeathsPal(countyShpJ$outslopedeaths28),
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>",NAME,", ",STATE_NAME,"<br>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>")) %>%
  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,group = 'Deaths14',
              fillColor = ~DeathsPal(countyShpJ$outslopedeaths14),
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>",NAME,", ",STATE_NAME,"<br>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>")) %>%
  
  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,
              fillColor = ~CasesPal(countyShpJ$outslopecases28), group = 'Cases28',
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>",NAME,", ",STATE_NAME,"<br>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>")) %>%
  
  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,group = 'Cases14',
              fillColor = ~CasesPal(countyShpJ$outslopeCases14),
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<b>",NAME,", ",STATE_NAME,"<br>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>")) %>%

  addLegend("bottomright",pal=DeathsPal,values = ~countyShpJ$outslopedeaths28, group = 'Deaths28',
            title = paste0("Past 28 Day Linear","<br>","Slope (Deaths)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLegend("bottomright",pal=CasesPal,values = ~countyShpJ$outslopecases28, group = 'Cases28',
            title = paste0("Past 28 Day Linear","<br>","Slope (Cases)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLegend("bottomright",pal=DeathsPal,values = ~countyShpJ$outslopedeaths14, group = 'Deaths14',
            title = paste0("Past 14 Day Linear","<br>","Slope (Deaths)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLegend("bottomright",pal=CasesPal,values = ~countyShpJ$outslopeCases14, group = 'Cases14',
            title = paste0("Past 14 Day Linear","<br>","Slope (Cases)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLayersControl(overlayGroups = c("Deaths28", "Cases28","Deaths14","Cases14"),
            options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(group=c('Deaths28','Deaths14','Cases28'))

  
  
  

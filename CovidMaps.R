
library(dplyr)
library(ggplot2)
library(sf)
library(scales)
library(leaflet)
library(RColorBrewer)
library(leafpop)
library(zoo)

setwd('C:/Users/phaedrus/Documents/RProjects/CovidMaps')

url <- c("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv?_sm_au_=iVVqLPMMPWSDkHHq6jkjvKQBQqvNG")
counties <- read.csv(url)
rm(url)

counties$county <- as.factor(counties$county)
counties$state  <- as.factor(counties$state)
counties$fips <- as.numeric(counties$fips)
counties$date <- as.Date(counties$date)

## fixing new york city into just one polygon to join to 
counties$fips<-ifelse(counties$county == 'New York City', 999999, counties$fips)


counties <- counties %>% group_by(state,county)%>%
  mutate (newcases = cases- lag(cases),
          newdeaths = deaths - lag(deaths),
          rolling7cases=rollmean(newcases,k=7,fill=NA,align = "right"),
          rolling7deaths=rollmean(newdeaths,k=7,fill=NA,align = "right"),)%>%
  ungroup()


counties$newcases <- ifelse(is.na(counties$newcases)==TRUE, 0,counties$newcases)
counties$newdeaths <- ifelse(is.na(counties$newdeaths)==TRUE, 0,counties$newdeaths)
counties$rolling7cases <- ifelse(is.na(counties$rolling7cases)==TRUE, 0,counties$rolling7cases)
counties$rolling7deaths <- ifelse(is.na(counties$rolling7deaths)==TRUE, 0,counties$rolling7deaths)


countyShp<-st_read('UScounties.shp')
countyShp$fips<-as.integer(as.character(countyShp$FIPS))

####  Just for New York City ##########
ny<- st_combine(countyShp%>%
                  filter(STATE_NAME=='New York' & NAME %in% c('Bronx','Queens','Kings','Richmond')))

countyShp<-countyShp%>%
  filter(!fips %in% c(36005,36081,36047,36085))

nyprep<-st_as_sf(data.frame(
  NAME = 'New York City',
  STATE_NAME = 'New York',
  STATE_FIPS = '36',
  CNTY_FIPS = '999999',
  FIPS = '999999',
  geometry = ny,
  fips = 999999),crs=4326)

### putting New York back into the main DF 
countyShp<-rbind(countyShp,nyprep)



FipsCases14Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
    tempDF <- counties %>%
    filter(fips == i, date >= max(date)-14) %>%
    select(date,fips,rolling7cases)
  slope<-lm(rolling7cases ~ date, data=tempDF)$coefficients[2]
  df<-data.frame(fips = i, slope=slope)
  FipsCases14Slope <-rbind(FipsCases14Slope, df,make.row.names = FALSE)
}

rm(df,i,slope,tempDF)

FipsDeaths14Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
  tempDF <- counties %>%
    filter(fips == i, date >= max(date)-14) %>%
    select(date,fips,rolling7deaths)
  slope<-lm(rolling7deaths ~ date, data=tempDF)$coefficients[2]
  df<-data.frame(fips = i, slope=slope)
  FipsDeaths14Slope <-rbind(FipsDeaths14Slope, df,make.row.names = FALSE)
}

rm(df,i,slope,tempDF)

FipsCases28Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
  tempDF <- counties %>%
    filter(fips == i, date >= max(date)-28) %>%
    select(date,fips,rolling7cases)
  slope<-lm(rolling7cases ~ date, data=tempDF)$coefficients[2]
  df<-data.frame(fips = i, slope=slope)
  FipsCases28Slope <-rbind(FipsCases28Slope, df,make.row.names = FALSE)
}

rm(df,i,slope,tempDF)

FipsDeaths28Slope<-data.frame(FIPS=integer(),slope=double())
for (i in na.omit(unique(counties$fips))){
  tempDF <- counties %>%
    filter(fips == i, date >= max(date)-28) %>%
    select(date,fips,rolling7deaths)
  slope<-lm(rolling7deaths ~ date, data=tempDF)$coefficients[2]
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



###making the plots  
for(i in unique(na.omit(counties$fips))){  
  temp<-counties%>%filter(fips==i)
  
  plotOut<-ggplot(temp)+
    geom_point(aes(x=date,y=rolling7cases,color='rolling7cases'))+
    geom_point(aes(x=date,y=rolling7deaths,color='rolling7deaths'))+
    geom_smooth(aes(x=date,y=rolling7cases),method='loess',se=FALSE)+
    geom_smooth(aes(x=date,y=rolling7deaths),method='loess',se=FALSE)+
    labs(title=paste0(temp$county,' ',temp$state,' -- As of  ', max(temp$date)), x="Date",y="Count")+
    scale_color_discrete(name='Legend',
                         breaks=c('rolling7cases','rolling7deaths'),
                         labels=c('7 Day Avg. Cases', '7 Day Avg. Deaths'))+
    theme_minimal()
  ggsave(filename=paste0(i,".svg"),plot=plotOut,device="svg")
}

for(i in unique(na.omit(counties$fips))){  
  temp<-counties%>%filter(fips==i)
  
  plotOut<-ggplot(temp)+
    geom_point(aes(x=date,y=rolling7deaths,color='rolling7deaths'))+
    geom_smooth(aes(x=date,y=rolling7deaths),method='loess')+
    labs(title=paste0(temp$county,' ',temp$state,' -- As of  ', max(temp$date)), x="Date",y="Count")+
    scale_color_discrete(name='Legend',
                         breaks=c('rolling7deaths'),
                         labels=c('7 Day Avg. Deaths'))+
    theme_minimal()
  ggsave(filename=paste0(i,"d.svg"),plot=plotOut,device="svg")
}


summaryStats <- counties%>%
  group_by(fips,county,state)%>%
  summarise(TotalCases = max(cases),
            TotalDeaths = max(deaths))%>%
  left_join(FipsCases14Slope,by=c("fips"))%>%
  left_join(FipsDeaths14Slope,by=c("fips"))%>%
  left_join(FipsCases28Slope,by=c("fips"))%>%
  left_join(FipsDeaths28Slope,by=c("fips"))%>%
  mutate(plot = paste0("<img src=http://keithguilford.com/maps/",fips,".svg style='width:600px;height:500px;'>" ),
         plotd =paste0("<img src=http://keithguilford.com/maps/",fips,"d.svg style='width:600px;height:500px;'>"))






countyShpJ<-countyShp%>%
  left_join(summaryStats,by="fips")


  
leaflet(countyShpJ)%>%
  #addTiles()%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,group = 'Death Slope 28 Days',
              fillColor = ~DeathsPal(countyShpJ$outslopedeaths28),
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                              "<h3 style = width:600px;>",NAME,", ",STATE_NAME,"</h3>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>",plotd)) %>%

  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,group = 'Death Slope 14 Days',
              fillColor = ~DeathsPal(countyShpJ$outslopedeaths14),
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                              "<h3 style = width:600px;>",NAME,", ",STATE_NAME,"</h3>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>",plotd)) %>%
  
  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,
              fillColor = ~CasesPal(countyShpJ$outslopecases28), group = 'Case Slope 28 Days',
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                              "<h3 style = width:600px;>",NAME,", ",STATE_NAME,"</h3>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>",plot)) %>%
  
  addPolygons(data=countyShpJ  ,color="#444444",weight = .1,smoothFactor = .5,
              opacity = .5, fillOpacity = .4 ,group = 'Case Slope 14 Days',
              fillColor = ~CasesPal(countyShpJ$outslopeCases14),
              highlightOptions = highlightOptions(color="white",weight = 2,
                                                  bringToFront = TRUE),
              popup = ~paste0("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                              "<h3 style = width:600px;>",NAME,", ",STATE_NAME,"</h3>",
                              "<b>Total Cases:  ",TotalCases,"<br>",
                              "<b>Total Deaths:  ",TotalDeaths,"<br>",
                              "<b>28 Day Cases Trend:  ",round(slope.x.x,digits=3),"<br>",
                              "<b>28 Day Deaths Trend:  ",round(slope.y.y,digits=3),"<br>",
                              "<b>14 Day Cases Trend:  ",round(slope.x,digits=3),"<br>",
                              "<b>14 Day Deaths Trend:  ",round(slope.y,digits=3),"<br>",plot)) %>%

  addLegend("bottomright",pal=DeathsPal,values = ~countyShpJ$outslopedeaths28, group = 'Death Slope 28 Days',
            title = paste0("Past 28 Day Linear","<br>","Slope (Deaths)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLegend("bottomright",pal=CasesPal,values = ~countyShpJ$outslopecases28, group = 'Case Slope 28 Days',
            title = paste0("Past 28 Day Linear","<br>","Slope (Cases)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLegend("bottomright",pal=DeathsPal,values = ~countyShpJ$outslopedeaths14, group = 'Death Slope 14 Days',
            title = paste0("Past 14 Day Linear","<br>","Slope (Deaths)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLegend("bottomright",pal=CasesPal,values = ~countyShpJ$outslopeCases14, group = 'Case Slope 14 Days',
            title = paste0("Past 14 Day Linear","<br>","Slope (Cases)","<br>",
                           "Date: ",Sys.Date()))%>%
  addLayersControl(overlayGroups = c("Death Slope 28 Days", "Case Slope 28 Days","Death Slope 14 Days","Case Slope 14 Days"),
            options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(group=c('Death Slope 28 Days','Death Slope 14 Days','Case Slope 28 Days'))

  
  


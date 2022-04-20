#Package
library(ggmap)
library(tidyverse)
library(dplyr)
library(sf) 
library(readxl)
library(RPostgreSQL)

#Register google api key
register_google(key = "Your Google API Key")

#Read Data
raw_data <- read_excel('path/your_file.xlsx')

#Geocode
get_lon_lat <- geocode(location = raw_data$alamat, output = "latlona", source = c("google", "dsk"))
data <- cbind(raw_data, get_lon_lat)

#Unlist
df_unlist<-function(df){
  df<-as.data.frame(df)
  nr<-nrow(df)
  c.names<-colnames(df)
  lscols<-as.vector(which(apply(df,2,is.list)==TRUE))
  if(length(lscols)!=0){
    for(i in lscols){
      temp<-as.vector(unlist(df[,i]))
      if(length(temp)!=nr){
        adj<-nr-length(temp)
        temp<-c(rep(0,adj),temp)
      }
      df[,i]<-temp
    } #end for
    df<-as.data.frame(df)
    colnames(df)<-c.names
  }
  return(df)
}

#Dataframe
newDF<-df_unlist(data)

#write to csv
write.csv(newDF,"your directory folder/name_file.csv")

#geodataframe
my.sf.point <- st_as_sf(x = newDF, 
                        coords = c("lon", "lat"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#write shapefile
st_write(my.sf.point,dsn="your directory folder", layer="layer", driver="ESRI Shapefile")

#Import Data to PostgreSQL
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "database", user = "username", host = "host", port = port, password = "pass")
st_write(my.sf.point,con,layer=c('skema','layer'))

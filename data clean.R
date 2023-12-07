library(tidyverse)
library(geojsonio)
library(leaflet)
library(dplyr)
library(IDDA)
library(maps)
library(sp)
library(htmlwidgets)
library(lubridate)
library(shiny)
library(ncdf4)
library(reshape2)
library(mapdata)
library(sp)
library(rgeos)
library(ggplot2)
library(ggmap)
library(sf)
library(spData)
library(raster)
library(rasterVis)
Sys.setlocale("LC_ALL", "English")
i <- 1:51
data <- NULL
for(i in 1:51){
  name <- paste0('2010_','ad_viz_plotval_data', i)
  path <- paste0(name, ".csv")
  data <- rbind(data, read.csv(file = path, header = TRUE))
}
for(i in 1:51){
  name <- paste0('2011_','ad_viz_plotval_data', i)
  path <- paste0(name, ".csv")
  data <- rbind(data, read.csv(file = path, header = TRUE))
}
write.csv(x = data,file = "data.csv")
ncname <- "prec.2011"
ncfname <- paste0(ncname, ".nc")

# Open the NetCDF file
ncin <- nc_open(ncfname)

# Get the variables from the file
prec <- ncvar_get(ncin, "prec")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")

# Close the NetCDF file
nc_close(ncin)

# Convert the 3D array to a 2D matrix where each row is a [lat, lon] pair
# and each column is a time point
prec_matrix <- array(prec, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}
# Create a data frame for lat and lon combinations
lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)
# Combine lat_lon_df with prec_matrix to create a data frame
# Each row corresponds to a [lat, lon, time, prec] combination
prec_df <- cbind(lat_lon_df, prec_matrix)

# Now, use melt from the reshape2 package to convert this to a long data frame
df <- melt(prec_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "precipitation")

# The 'time' column is a factor, convert it to a numeric representation
df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))%>%
  group_by(STATE,time)%>%
  mutate(Mean_precipitation=mean(precipitation))%>%
  slice(1)
df_prec_2011 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2010-12-31")), .before = precipitation)
write.csv(x = df_prec_2011,file = "df_prec_2011.csv")
#prec.2010
ncname <- "prec.2010"
ncfname <- paste0(ncname, ".nc")
ncin <- nc_open(ncfname)
prec <- ncvar_get(ncin, "prec")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")
nc_close(ncin)

# Convert the 3D array to a 2D matrix where each row is a [lat, lon] pair
# and each column is a time point
prec_matrix <- array(prec, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)
# Combine lat_lon_df with prec_matrix to create a data frame
# Each row corresponds to a [lat, lon, time, prec] combination
prec_df <- cbind(lat_lon_df, prec_matrix)

# Now, use melt from the reshape2 package to convert this to a long data frame
df <- melt(prec_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "precipitation")

# The 'time' column is a factor, convert it to a numeric representation
df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))%>%
  group_by(STATE,time)%>%
  mutate(Mean_precipitation=mean(precipitation))%>%
  slice(1)
df_prec_2010 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2009-12-31")), .before = precipitation)
write.csv(x = df_prec_2010,file = "df_prec_2010.csv")
#wind.2010
ncname <- "wind.2010"
ncfname <- paste0(ncname, ".nc")
ncin <- nc_open(ncfname)
wind <- ncvar_get(ncin, "wind")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")
nc_close(ncin)

wind_matrix <- array(wind, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)

wind_df <- cbind(lat_lon_df, wind_matrix)

df <- melt(wind_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "wind")

df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(wind=ifelse(is.na(wind),0,wind))%>%
  group_by(STATE,time)%>%
  mutate(Mean_wind=mean(wind))%>%
  slice(1)
df_wind_2010 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2009-12-31")), .before = wind)
write.csv(x = df_wind_2010,file = "df_wind_2010.csv")
#wind.2011
ncname <- "wind.2011"
ncfname <- paste0(ncname, ".nc")
ncin <- nc_open(ncfname)
wind <- ncvar_get(ncin, "wind")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")
nc_close(ncin)

wind_matrix <- array(wind, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)

wind_df <- cbind(lat_lon_df, wind_matrix)


df <- melt(wind_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "wind")

df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(wind=ifelse(is.na(wind),0,wind))%>%
  group_by(STATE,time)%>%
  mutate(Mean_wind=mean(wind))%>%
  slice(1)
df_wind_2011 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2010-12-31")), .before = wind)
write.csv(x = df_wind_2011,file = "df_wind_2011.csv")

#tmax.2010
ncname <- "tmax.2010"
ncfname <- paste0(ncname, ".nc")
ncin <- nc_open(ncfname)
tmax <- ncvar_get(ncin, "tmax")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")
nc_close(ncin)

# Convert the 3D array to a 2D matrix where each row is a [lat, lon] pair
# and each column is a time point
tmax_matrix <- array(tmax, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)
# Combine lat_lon_df with prec_matrix to create a data frame
# Each row corresponds to a [lat, lon, time, prec] combination
tmax_df <- cbind(lat_lon_df, tmax_matrix)

# Now, use melt from the reshape2 package to convert this to a long data frame
df <- melt(tmax_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "tmax")

# The 'time' column is a factor, convert it to a numeric representation
df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(tmax=ifelse(is.na(tmax),0,tmax))%>%
  group_by(STATE,time)%>%
  mutate(Mean_tmax=mean(tmax))%>%
  slice(1)
df_tmax_2010 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2009-12-31")), .before = tmax)
write.csv(x = df_tmax_2010,file = "df_tmax_2010.csv")

#tmax.2011
ncname <- "tmax.2011"
ncfname <- paste0(ncname, ".nc")
ncin <- nc_open(ncfname)
tmax <- ncvar_get(ncin, "tmax")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")
nc_close(ncin)

tmax_matrix <- array(tmax, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)

tmax_df <- cbind(lat_lon_df, tmax_matrix)


df <- melt(tmax_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "tmax")


df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(tmax=ifelse(is.na(tmax),0,tmax))%>%
  group_by(STATE,time)%>%
  mutate(Mean_tmax=mean(tmax))%>%
  slice(1)
df_tmax_2011 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2010-12-31")), .before = tmax)
write.csv(x = df_tmax_2011,file = "df_tmax_2011.csv")

#tmin.2010
ncname <- "tmin.2010"
ncfname <- paste0(ncname, ".nc")
ncin <- nc_open(ncfname)
tmin <- ncvar_get(ncin, "tmin")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")
nc_close(ncin)


tmin_matrix <- array(tmin, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)

tmin_df <- cbind(lat_lon_df, tmin_matrix)

df <- melt(tmin_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "tmin")

df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(tmin=ifelse(is.na(tmin),0,tmin))%>%
  group_by(STATE,time)%>%
  mutate(Mean_tmin=mean(tmin))%>%
  slice(1)
df_tmin_2010 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2009-12-31")), .before = tmin)
write.csv(x = df_tmin_2010,file = "df_tmin_2010.csv")

#tmin.2011
ncname <- "tmin.2011"
ncfname <- paste0(ncname, ".nc")
ncin <- nc_open(ncfname)
tmin <- ncvar_get(ncin, "tmin")
lat <- ncvar_get(ncin, "lat")
lon <- ncvar_get(ncin, "lon")
nc_close(ncin)


tmin_matrix <- array(tmin, dim = c(length(lat) * length(lon), length(ncin$dim$time$vals)))

lat_lon_df <- expand.grid(lon = lon, lat = lat)
lat_lon_df$lon=lat_lon_df$lon-360
lat_lon_df$STATE=lonlat_to_state(lat_lon_df)

tmin_df <- cbind(lat_lon_df, tmin_matrix)

df <- melt(tmin_df, id.vars = c("lat", "lon","STATE"), 
           variable.name = "time", 
           value.name = "tmin")

df$time <- as.numeric(as.character(df$time))
df=df%>%
  filter(!is.na(STATE))%>%
  mutate(tmin=ifelse(is.na(tmin),0,tmin))%>%
  group_by(STATE,time)%>%
  mutate(Mean_tmin=mean(tmin))%>%
  slice(1)
df_tmin_2011 <- df %>%
  mutate(Date = as.character(as.Date(time, origin = "2010-12-31")), .before = tmin)
write.csv(x = df_tmin_2011,file = "df_tmin_2011.csv")
df_prec=rbind(df_prec_2010,df_prec_2011)
df_wind=rbind(df_wind_2010,df_wind_2011)
df_tmax=rbind(df_tmax_2010,df_tmax_2011)
df_tmin=rbind(df_tmin_2010,df_tmin_2011)
data=data%>%
  mutate(Date=gsub('([0-9]{2})/([0-9]{2})/([0-9]{4})', '\\3-\\1-\\2', Date))%>%
  left_join(df_prec%>%dplyr::select(3,5,7),by=c('STATE','Date'))
data= data%>%
  left_join(df_wind%>%dplyr::select(3,5,7),by=c('STATE','Date'))
data= data%>%
  left_join(df_tmax%>%dplyr::select(3,5,7),by=c('STATE','Date'))
data=data%>%
  left_join(df_tmin%>%dplyr::select(3,5,7),by=c('STATE','Date'))
data=data%>%
  dplyr::select(-21,-23,-25,-27)
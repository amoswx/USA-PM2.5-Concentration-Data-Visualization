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
library(plotly)
date.max=as.Date('2011-12-31')
date.min=as.Date('2010-01-01')
shinyUI(fluidPage(
  
  div(class="outer"),
  titlePanel(h1("Spatiotemporal Dynamics of PM2.5 and Influence of Meteorological Variables", align = "center")),
  absolutePanel(id = "control", class = "panel panel-default",
                top = 500, left = 26, width = 1010, fixed=TRUE,
                draggable = FALSE, height = 115, style = "opacity: 1",
                sliderInput("Date",
                            label = h5("Select date"),
                            min = date.min,
                            max = date.max,
                            width=1000,
                            value = date.max,
                            step = 1,
                            timeFormat = "%Y %m %d",
                            animate=animationOptions(interval = 2000, loop = FALSE)
                )), # end of sliderInput1
  mainPanel(width = 8,
    tabsetPanel(
      tabPanel("daily_map", leafletOutput("daily_map", width = "126%", height = "350px")),
      tabPanel("prec_map", plotOutput("prec_map", width = "126%", height = "350px")),
      tabPanel("prec_map_State", leafletOutput("prec_map_State", width = "126%", height = "350px")),
      tabPanel("prec_smooth", plotOutput("prec_smooth", width = "126%", height = "350px")),
      tabPanel("wind_map", plotOutput("wind_map", width = "126%", height = "350px")),
      tabPanel("wind_map_State", leafletOutput("wind_map_State", width = "126%", height = "350px")),
      tabPanel("wind_smooth", plotOutput("wind_smooth", width = "126%", height = "350px")),
      tabPanel("tmax_map", plotOutput("tmax_map", width = "126%", height = "350px")),
      tabPanel("tmax_map_State", leafletOutput("tmax_map_State", width = "126%", height = "350px")),
      tabPanel("tmax_smooth", plotOutput("tmax_smooth", width = "126%", height = "350px")),
      tabPanel("tmin_map", plotOutput("tmin_map", width = "126%", height = "350px")),
      tabPanel("tmin_map_State", leafletOutput("tmin_map_State", width = "126%", height = "350px")),
      tabPanel("tmin_smooth", plotOutput("tmin_smooth", width = "126%", height = "350px")),
    )
  )
  
))
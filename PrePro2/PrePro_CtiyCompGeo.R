library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(rgdal)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggplot2)
library(readr)
library(tidyverse)

ccg = read_csv("CityCompGeo.csv")
View(ccg)

ccg[2] = c("Wifi Speed (Mbps/sec)", "Num of Co-Working Spaces", "Coffee (USD/cup)", "Taxi (USD/km)",
           "Beer (USD/cup)", "Studio Rentals (1BR/mth, USD)", "Meals (USD)", "Sunshine Hours (Annual)",
           "TripAdvisor Attractions", "Instagrammability (#pics in mil)")
ccg[2]

ccg


write.csv(ccg, 'citycomp_geo2.csv')

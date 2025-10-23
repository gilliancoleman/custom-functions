##################
#Gillian 
#10/23/2025
#Custom Functions
##################

#load libraries 
library(tidyverse)
library (rvc)
library (patchwork)

#install new packages 
install.packages('devtools')

devtools::install_github('jeremiaheb/rvc')

#The Fish R-package (rvc) has a lot of functions available for estimating common metrics including density, occurrence, biomass, length frequency…and much more.

#You will first need to get the data using the function

#getRvcData(years = , regions = )

#years = list or sequence of years

#regions = vector of regions you

#You will use “STTSTJ” and “STX”
#For example: The following code will get all the data from 2017, 2019, 2021 and 2023 in both the St. Thomas/John and St. Croix regions and set it to the value ‘USVI’

USVI <- getRvcData(years = 2017:2023, regions =  c("STTSTJ", "STX"))

glimpse(USVI) #list of three data frames (sample, strata, and taxo)

#Now explore a few of the basic functions to see what the function outputs.

getDomainBiomass(x = USVI, species = "EPI GUTT")
getDomainDensity(x = USVI, species = "BAL VETU")
getDomainOccurrence(x = USVI, species = "SPA VIRI")

##############
#Exercise
##############

#I would like for you to create a function(s) that creates the following set of plots.

#hints:
  
  #Use grid() and gridExtra() or patchwork() packages arrange the 3 plots on one page
#All plots will need error bars (standard error).
#𝑆𝐸=𝑣𝑎𝑟‾‾‾‾√
#Use the ‘rvc’ package functions and some ‘tidyverse’ functions for all your needs

metrics_graph <- function (df, species){
  # Get each metric summary from your functions
  dens <- getDomainDensity(x = df, species = species) %>%
    mutate(SE = sqrt(var))
  
  occ <- getDomainOccurrence(x = df, species = species) %>%
    mutate(SE = sqrt(var))
  
  bio <- getDomainBiomass(x = df, species = species) %>%
    mutate(SE = sqrt(var))
  
  #density plot
  p1 <- ggplot(dens, aes(x = YEAR, y = density, color = REGION, group = REGION)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = density - SE, ymax = density + SE), width = 0.2)
  
  p2 <- ggplot(occ, aes(x = YEAR, y = occurrence , color = REGION, group = REGION)) + geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = occurrence - SE, ymax = occurrence + SE), width = 0.2)
  
  p3 <- ggplot(bio, aes(x = YEAR, y = biomass, color = REGION, group = REGION)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = biomass - SE, ymax = biomass + SE), width = 0.2)
  
  #Stack vertically
  combined <- (p1 / p2 / p3) + plot_layout(guides = "collect") + plot_annotation(species,theme=theme(plot.title=element_text(hjust=0.5)))
  
  return(combined)
  
}

metrics_graph(df = USVI, species = 'EPI GUTT')

metrics_graph(df = USVI, species = 'OCY CHRY')

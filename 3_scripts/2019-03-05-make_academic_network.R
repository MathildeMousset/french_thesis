
# Setup -------------------------------------------------------------------

#library(rvest) # To scrap PhD pages to get committe members
#library(curl)

library(tidyverse)
library(hrbrthemes)
library(extrafont)

library(igraph)
library(ggraph)
library(ggrepel)

source("4_functions.R")


# 2015 - 2018 -------------------------------------------------------------

load("scrapped_2015_2018.RData")

graphe_1 <- make_network_full(scrapped_2015_2018,
                              my_waltrap = 10,
                              my_edge    = 3,
                              my_degree  = 2,
                              my_title   = "Network of evolutionary biologists",
                              my_subtitle = "Based on French PhD advising and commitees, 2015 - 2018")


graphe_1

ggsave(graphe_1,
       filename = "evolution_2015_2018.png",width=30,height = 20)

# Montpellier -------------------------------------------------------------

load( "scrapped_Montpellier.RData")

graphe_2 <- make_network_full(scrapped_Montpellier,
                              my_waltrap = 10,
                              my_edge    = 4,
                              my_degree  = 5,
                              my_title   = "Network of evolutionary biologists",
                              my_subtitle = "Based on French PhD advising and committes, Montpellier")

graphe_2

ggsave(graphe_2,
       filename = "evolution_Montpellier.png",width=30,height = 20)

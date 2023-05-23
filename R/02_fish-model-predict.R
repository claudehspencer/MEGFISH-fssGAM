###
# Project: MEGFISH GAM Example
# Data:    BOSS & BRUV fish, habitat
# Task:    Modelling fish abundance w/ FSSGAM
# Author:  Claude
# Date:    May 2023
##

rm(list=ls())

library(mgcv)
library(ggplot2)
library(viridis)
library(terra)
library(dplyr)
library(tidyverse)
library(patchwork)

# set study name - used in filenames (campaignID)
name <- "2021-05_Abrolhos"  

# Read in the formatted data
dat <- readRDS(paste0("data/", name, "_fish-abundance.rds")) %>%
  glimpse()

# Predictors in gridded form (UTM projection)
preds  <- readRDS(paste0("data/", name,"_habitat_predictions.rds")) %>%
  dplyr::select(x, y, tpi, roughness, detrended, biog, macroalgae, mean.relief) %>%
  glimpse()

# Use formula from 'top' model from FSSGam model selection
# Total abundance
m_totabund <- gam(maxn ~ s(mean.relief, k = 3, bs = "cr"), 
               data = dat %>% dplyr::filter(scientific %in% "total.abundance"), 
               family = tw())
summary(m_totabund)

# Species richness
m_richness <- gam(maxn ~ s(mean.relief, k = 3, bs = "cr") + s(tpi, k = 3, bs = "cr"),  
                     data = dat %>% dplyr::filter(scientific %in% "species.richness"), 
                     family = tw())
summary(m_richness)

# Predict, rasterise and plot
preds <- cbind(preds, 
                "p_totabund" = predict(m_totabund, preds, 
                                       type = "response", se.fit = T),
                "p_richness" = predict(m_richness, preds, 
                                       type = "response", se.fit = T))

# Rasterize and reproject into lat long for plotting
prasts <- rast(preds %>% dplyr::select(x, y, p_totabund.fit, p_totabund.se.fit,
                                       p_richness.fit, p_richness.se.fit),
               crs = "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs") %>%
  project("epsg:4326")
plot(prasts) # Check that everything looks ok
preds <- as.data.frame(prasts, xy = T)

# Plot the fitted values
p1 <- ggplot() +
  geom_tile(data = preds, aes(x = x, y = y, fill = p_totabund.fit)) +
  scale_fill_viridis(option = "D") +
  labs(x = "Longitude", y = "Latitude", fill = "Total abundance") +
  theme_minimal() +
  coord_sf()
p1

# Plot the standard error
p2 <- ggplot() +
  geom_tile(data = preds, aes(x = x, y = y, fill = p_totabund.se.fit)) +
  scale_fill_viridis(option = "B") +
  labs(x = "Longitude", y = "Latitude", fill = "Standard error") +
  theme_minimal() +
  coord_sf()
p2

# Combine using patchwork
p1 + p2

library(rgdal)
library(tidyr)
library(maptools)
library(ggplot2)
library(dplyr)
library(reshape2)

setwd('D:/Masters/Thesis/R workspaces/TravelTimeAnalysis')

attr <- read.table("WaterlooDA.txt", header=TRUE, sep=',', stringsAsFactors=FALSE)

attr <- data.frame(attr$DAUID, attr$HuffExp, attr$HuffExp2, attr$HuffExp3,
                   attr$LA5min, attr$LA10min, attr$LA15min, attr$LA4_exp,
                   attr$ParcelCoun, attr$Shape_Area)

colnames(attr) <- c("DAUID", "HuffExp", "HuffExp2", "HuffExp3", "LA5min", "LA10min", "LA15min", "LA19min", "ParcelCount", "Area")

## Produces expenditure measurements by ID variable DAUID, using reshape2/melt
latidy <- melt(attr, id.vars=c("DAUID"), measure.vars = c("LA5min", "LA10min", "LA15min", "LA19min"))
colnames(latidy) <- c("DAUID", "TravelTime", "Expenditure")

latidy$DAUID <- as.factor(latidy$DAUID) # for subsequent join with shp_f

### READ SPATIAL DATA ###
shp <- readOGR(".", "WaterlooDAs")
shp$id <- row.names(shp)

shp_f <- fortify(shp)
shp_f <- left_join(shp_f, shp@data, by="id")

# Join shp fortified (shp_f) to latidy
shp_f <- left_join(shp_f, latidy, by="DAUID")

ggplot(data = shp_f, # the input data
       aes(x = long.x, y = lat.x, fill = Expenditure/1000, group = group)) + # define variables
  geom_polygon() + # plot the DAs
  geom_path(colour="black", lwd=0.025) + # DA borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ TravelTime, ncol = 2) + # one plot per buffer size
  scale_fill_gradient2(low = "#F0F9E8", mid = "#7BCCC4", high = "#002673", # colors converted from RGB (ArcGIS) to Hex
                       midpoint = 10000, name = "Expenditures \n(thousands $ CDN)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

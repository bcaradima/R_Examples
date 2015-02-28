library(rgdal)
library(tidyr)
library(maptools)
library(ggplot2)
library(dplyr)
library(reshape2)

setwd('D:/Masters/Thesis/R workspaces/WaterlooFacetPlot')

waterloo <- read.table("waterloo-data.txt", header=TRUE, sep=',', stringsAsFactors=FALSE)
waterloo <- data.frame(waterloo$DAUID, waterloo$LA0km, waterloo$LA4_exp, waterloo$LA20km, waterloo$LA30km, waterloo$LA40km, waterloo$LA50km)
colnames(waterloo) <- c("DAUID", "LA0km", "LA10km","LA20km", "LA30km", "LA40km", "LA50km")

## Produces expenditure measurements by ID variable DAUID, using reshape2/melt
wtidy <- melt(waterloo, id.vars=c("DAUID"), measure.vars = c("LA0km", "LA10km", "LA20km", "LA30km", "LA40km", "LA50km"))
colnames(wtidy) <- c("DAUID", "BufferSize", "Expenditure")

wtidy$DAUID <- as.factor(wtidy$DAUID) # for subsequent join with wtrl_f

### READ SPATIAL DATA ###
#wtrl <- readOGR(".", "Waterloo_DA_2011_new")
wtrl <- readShapeSpatial("WaterlooDAs")
wtrl$id <- row.names(wtrl)

wtrl_f <- fortify(wtrl)
wtrl_f <- left_join(wtrl_f, wtrl@data, by="id")

# Join wtrl fortified (wtrl_f) to either twaterloo or wtidy
wtrl_f <- left_join(wtrl_f, wtidy, by="DAUID")

ggplot(data = wtrl_f, # the input data
       aes(x = long.x, y = lat.x, fill = Expenditure/1000, group = group)) + # define variables
  geom_polygon() + # plot the DAs
  geom_path(colour="black", lwd=0.025) + # DA borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ BufferSize, ncol = 2) + # one plot per buffer size
  scale_fill_gradient2(low = "#F0F9E8", mid = "#7BCCC4", high = "#002673", # colors
                       midpoint = 10000, name = "Expenditure\n(thousands $)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

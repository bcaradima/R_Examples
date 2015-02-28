############ READ DATA ###################

library(rgdal)

setwd('D:/Masters/Thesis/R workspaces/ModelComparison')

df <- read.table('WaterlooDA.txt', sep=',', header=TRUE, na.strings="NA", stringsAsFactors=FALSE)

shp <- readOGR(".", "WaterlooDAs")

data <- data.frame(df$DAUID, df$HuffExp3, df$Shape_Area, df$ParcelCoun)

colnames(data) <- c("DAUID", "HuffExp","Area", "ParcelCount")

data$Area <- data$Area/1000000 # convert DA areas from square meters to square kilometers

############ REMOVE OUTLIERS ####################

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

############ VISUALISATION ###################
dArea <- density(data$Area)
dCount <- density(data$ParcelCount)

plot(dArea, main="Density of CDA Area", xlab="CDA area (sq km)", xlim=c(0,1.5))
plot(dCount, main="Density of Parcel Count by CDA", xlab="Number of Parcels within a CDA", xlim=c(0,600))

boxplot(data$ParcelCount, horizontal=TRUE, ylim=c(0,1000))

# plot(x,y)
# Remove outliers and plot
x <- remove_outliers(data$Area)
y <- remove_outliers(data$ParcelCount)

plot(x, y, xlim=c(0,1), ylim=c(0,500),
     main="Parcel Count by CDA Polygon vs CDA Area",
     xlab="CDA area (sq km)", 
     ylab="Number of Parcels within a CDA")

dim(subset(data, Area < 1)) # number of rows indicate number of DAs smaller than 1 sq km.
onekm <- subset(data, Area < 1) 
sum(onekm$ParcelCount) # number of parcels within DAs smaller than 1 sq km

















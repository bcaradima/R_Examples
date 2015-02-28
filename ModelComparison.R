library(sm)
library(ggplot2)
library(reshape2)
library(e1071) # for measuring Pearson's skewness()
setwd('D:/Masters/Thesis/R workspaces/ModelComparison')

### IMPORT DATA ###
df <- read.table('WaterlooDA.txt', sep=',', header=TRUE, na.strings="NA", stringsAsFactors=FALSE)

# huff <- df$HuffExp3
# huff <- huff[-which(huff==0)] # remove Huff expenditure that equals 0
# la <- df$LA4_exp

Huff <- df$HuffExp3
LA <- df$LA4_exp

Huff <- Huff/1000000
LA <- LA/1000000

# Expenditures <- c(huff, la)
# Expenditures <- Expenditures/1000000
# Groups <- c(rep("Huff", length(huff)), rep("LA", length(la)))
# 
# groupHuff <- c(rep("Huff", length(huff)))
# groupLA <- c(rep("LA", length(la)))

### DENSITY PLOT

# Kernel density estimation is non-parametric method for estimating
# the probability density function of a random variable
# For single plot of two densities, use 'sm' package: see R in Action
huffD <- density(Huff)
laD <- density(LA)

plot(huffD,
     xlim=c(5,20), ylim=c(0,0.25),
     xlab="Expenditures (millions $ CDN)", 
     ylab="Density", 
     main="Density of Huff's Model Expenditures")
polygon(huffD, col="grey", border="black")

lines(laD, xlim=c(5,20), ylim=c(0,0.25))
polygon(laD, col="grey", border="black")


# Plot by group
# sm.density.compare(Expenditures, DensityFactors, col=c("blue","black"), xlab="Expenditures")
# title(main="Distribution of Expenditures by Method")
# 
# colfill<-c(2:(1+length(levels(DensityFactors))))
# # locator(1) option means you click on graph to place legend!
# legend(locator(1), levels(DensityFactors), fill=colfill, col="black")

data <- data.frame(Huff, LA)
data$Huff <- data$Huff
data$LA <- data$LA
data <- melt(data)
colnames(data) <- c("Method", "Expenditure")

ggplot(data = data, aes(x=Expenditure, color=Method)) + geom_density() +
  scale_colour_manual(values = c("#002673", "black")) +
  labs(title = "Density of Expenditures by Method") +
  ylab("Density") +
  xlab("Expenditures (millions $ CDN)")
#+  coord_cartesian(xlim = c(5, 17)) 

### BOXPLOT
Expenditures <- c(Huff, LA)
Method <- as.factor(c("Huff", "LA"))
bpdata <- data.frame(Expenditures, Method)

ggplot(bpdata, aes(Method, Expenditures)) + 
  geom_boxplot() +
  coord_flip() +
  labs(title = "Summary of Expenditures by Method") +
  ylab("Expenditures (millions $ CDN)") +
  xlab("Model Type")

boxplot(LA, Huff,
        horizontal=TRUE,
        names=c("LA", "Huff"),
        xlab="Expenditures (millions $ CDN)",
        ylab="Method",
        main="Summary of Expenditures by Method",
        outliers=FALSE)

### QQ PLOT
# Test for normality
plot(qqnorm(huff))
plot(qqnorm(la))

# Test data for comparison of means (t-test) and variance (ex, ANOVA)
# If data is not normal, use non-parametric tests (Kruskal-Wallis)

### Kruskal-Wallis: non-normal test for comparison of variance
kt <- c(huff, la)
ktGroups <- c(rep("huff", length(huff)), rep("la", length(la)))
ktGroups <- factor(ktGroups)

kruskal.test(kt, ktGroups)




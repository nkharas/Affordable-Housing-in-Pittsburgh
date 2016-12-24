# Author : Nick Kharas
# Description : Explore data, use clustering to find reasonable classification boundaries

library(ggplot2)
library(gridExtra)

# Function to remove outliers, anything greater or less than 1.5 times the interquartile range
# Borrowed from
# http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
identify_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  #y <- x
  #y[x < (qnt[1] - H)] <- NA
  #y[x > (qnt[2] + H)] <- NA
  y <- (x < (qnt[1] - H) | x > (qnt[2] + H))
  return(y)
}

options(scipen = 999)

################## DATA EXPLORATION #########################################################
data.set <- read.csv('property_assessments_clean.csv', sep="," ,header=TRUE)

# BOX PLOT TO COMPARE MEDIAN SALES PRICES BASED ON PROPERTY CONDITION

# Remove records where sales prices lie among outliers outliers in sales prices
data.set.sales <- data.set[-which(identify_outliers(data.set$SALEPRICE)) , ]

# Refactor the Condition Description to set a meaningful sort order 
# instead of the default alphabetical order
data.set.sales$CONDITIONDESC <- factor(data.set.sales$CONDITIONDESC, 
                                        levels=c("UNSOUND", "VERY POOR", "POOR",
                                                 "FAIR", "AVERAGE", "GOOD",
                                                 "VERY GOOD", "EXCELLENT"))

# Remove rows with missing condition description
data.set.sales <- data.set.sales[-which(is.na(data.set.sales$CONDITIONDESC)) , ]

# Data set for Hazelwood Only
data.set.haz <- data.set.sales[which(data.set.sales$PROPERTYZIP == 15207) , ]
data.set.rest <- data.set.sales[which(data.set.sales$PROPERTYZIP != 15207) , ]


require(gridExtra)

# Function to create box plots using ggplot
createBoxplot <- function(data, title){
  p <- ggplot(data,  aes(x=CONDITIONDESC, y=SALEPRICE, col=CONDITIONDESC)) 
  p <- p + geom_boxplot()
  p <- p + theme(axis.text.x = element_text(angle=45), legend.position="none") # Set x axis label style and angle, remove legend
  p <- p + labs(title = title, x = "Property Condition", y = "Sale Price") # Axis titles
  p <- p + ylim(c(0,300000)) # Setting scale on the y-axis, ensures same scale for both plots put together
  return(p)
}

haz <- createBoxplot(data.set.haz, 'Hazelwood') # Boxplot for hazelwood
rest <- createBoxplot(data.set.rest, 'Rest of Allegheny County') # Boxplot for rest of Allegheny County

# Arrange plots side by side
grid.arrange(haz, rest, ncol=2, top = 'Comparing Average Sale Prices by Property Condition')

################## ################ #########################################################


################## CLUSTERING ###############################################################

# Import Data 
data.set <- import.csv('price_date_diff.csv')

# Fix string date differences
data.set$DateDiff1 <- substr(data.set$DateDiff1, 0, regexpr(pattern = ' ', data.set$DateDiff1) - 1 )
data.set$DateDiff1 <- as.numeric(data.set$DateDiff1) / 365

# Remove rows with outliers to get reasonable results
data.set.no.out <- data.set[-which(identify_outliers(data.set$PriceDiff1_per)) , ]
data.set.no.out <- data.set.no.out[-which(identify_outliers(data.set.no.out$DateDiff1)) , ]

# Separate data frame to remove ouliers from sale price only
data.set.no.out.sp <- data.set[-which(identify_outliers(data.set$SALEPRICE)) , ]
data.set.no.out.sp <- data.set.no.out.sp[-which(identify_outliers(data.set.no.out.sp$PriceDiff1_per)) , ]

set.seed(100)

# Function to create clusters and plot based on clusters
cluster <- function(data, col.name, nclusters, title){
  price.date.cluster <- kmeans(data[,c(col.name)], nclusters, 20)
  
  p <- ggplot(data, aes(DateDiff1, PriceDiff1_per,
             color = as.factor(price.date.cluster$cluster) )) + geom_point()
  p <- p + labs(title = title, x = "% Appreciation in Price from Last Sale", y = "Sale Price") # Axis titles
  p <- p + theme(legend.position = "none")
  
  return(p)
}

require(gridExtra)

# Cluster based on ownership duration
date.diff.plot <- cluster(data.set.no.out, "DateDiff1", 7, "Ownership Duration")

# Cluster based on price appreciation
price.diff.plot <- cluster(data.set.no.out, "PriceDiff1_per", 4, "Price Appreciation")

# Arrange both plots side by side
grid.arrange(date.diff.plot, price.diff.plot, ncol=2, top = 'Clustering by Ownership Duration and Price Appreciation')

# Cluster based on sale price
sale.price.kmeans <- kmeans(data.set.no.out.sp[,c("SALEPRICE")], 4, 20)
sale.price.plot <- ggplot(data.set.no.out.sp, 
                          aes(PriceDiff1_per, SALEPRICE,
                          color = as.factor(sale.price.kmeans$cluster) )) + geom_point()
sale.price.plot <- sale.price.plot + labs(title = "Clustering by Sale Price", x = "% Appreciation in Price from Last Sale", y = "Sale Price") # Axis titles
sale.price.plot <- sale.price.plot + theme(legend.position = "none")



########## Code ends here ###############







# EXPERIMENTAL CODE


# Create two plots next to each other for aesthetics
par(mfrow=c(1,2))

# Set color scheme for box plot
c1 <- rainbow(9)
c2 <- rainbow(9, alpha=0.2)
c3 <- rainbow(9, v=0.7)

# Box plot for Hazelwood
haz <- boxplot(SALEPRICE ~ CONDITIONDESC, data= data.set.haz, 
               col=c2, 
               medcol=c3, 
               whiskcol=c1, 
               staplecol=c3, 
               boxcol=c3, 
               outcol=c3, 
               pch=23, cex=2,
               ylab = "Sale Price",
               main = "Hazelwood",
               ylim=c(0, 300000)
)

# Box plot for the rest of Allegheny County
boxplot(SALEPRICE ~ CONDITIONDESC, data= data.set.rest, 
        col=c2, 
        medcol=c3, 
        whiskcol=c1, 
        staplecol=c3, 
        boxcol=c3, 
        outcol=c3, 
        pch=23, cex=2,
        xlab = "Property condition",
        ylab = "Sale Price",
        main = "Rest of Allegheny County",
        ylim=c(0, 300000)
)

# Main header
mtext("Comparing Average Sale Prices by Property Condition", side=3, outer=TRUE, line = -1, cex = 1.5)

# Common label for x axis
mtext("Property Condition", side=3, outer=TRUE, line = -31, cex = 1)



#ggplot(data.set.no.out, 
#       aes(DateDiff1, PriceDiff1_per, col = SALEPRICE)) + geom_point()


# Cluster by Ownership duration and price difference (in %)
price.date.cluster <- kmeans(data[,c("DateDiff1")], nclusters, 20)

# Plot the clusters
ggplot(data.set.no.out, 
       aes(DateDiff1, PriceDiff1_per,
           color = as.factor(price.date.cluster$cluster) )) + geom_point()

# Cluster by price difference (in %) only
price.date.cluster <- kmeans(data.set.no.out[,c("PriceDiff1_per")], 4, 20)
ggplot(data.set.no.out, 
       aes(DateDiff1, PriceDiff1_per,
           color = as.factor(price.date.cluster$cluster) )) + geom_point()

# Cluster by sale price only
data.set.no.out.sp <- data.set[-which(identify_outliers(data.set$SALEPRICE)) , ]
price.date.cluster <- kmeans(data.set.no.out.sp[,c("SALEPRICE")], 4, 20)
ggplot(data.set.no.out.sp, 
       aes(DateDiff1, SALEPRICE,
           color = as.factor(price.date.cluster$cluster) )) + geom_point()

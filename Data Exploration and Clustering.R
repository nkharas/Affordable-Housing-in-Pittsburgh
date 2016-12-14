# Author : Nick Kharas
# Description : Explore data, find clusters

import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

# Function to normalize numeric data
normlz <- function(x){
  return((x / sum(x,na.rm = TRUE) ) * 100000)
}

# Function to remove outliers, anything greater or less than 1.5 times the interquartile range
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
data.set <- import.csv('property_assessments_clean.csv')

# BOX PLOT TO COMPARE MEDIAN SALES PRICES BASED ON PROPERTY CONDITION

# Remove outliers in sales prices
data.set.no.out <- data.set[-which(identify_outliers(data.set$SALEPRICE)) , ]

# Refactor the Condition Description to set a meaningful sort order 
# instead of the default alphabetical order
data.set.no.out$CONDITIONDESC <- factor(data.set.no.out$CONDITIONDESC, 
                                        levels=c("UNSOUND", "VERY POOR", "POOR",
                                                 "FAIR", "AVERAGE", "GOOD",
                                                 "VERY GOOD", "EXCELLENT"))
# Remove rows with missing condition description
data.set.no.out <- data.set.no.out[-which(is.na(data.set.no.out$CONDITIONDESC)) , ]

# Data set for Hazelwood Only
data.set.haz <- data.set.no.out[which(data.set.no.out$PROPERTYZIP == 15207) , ]

# Set color scheme for box plot
c1 <- rainbow(9)
c2 <- rainbow(9, alpha=0.2)
c3 <- rainbow(9, v=0.7)

# Box plot for all of Allegheny County
boxplot(SALEPRICE ~ CONDITIONDESC, data= data.set.no.out, 
        col=c2, 
        medcol=c3, 
        whiskcol=c1, 
        staplecol=c3, 
        boxcol=c3, 
        outcol=c3, 
        pch=23, cex=2,
        xlab = "Property condition",
        ylab = "Sale Price",
        main = "Comparing Average Sale Prices by Property Condition (Allegheny County)"
)

# Box plot for Hazelwood Only
boxplot(SALEPRICE ~ CONDITIONDESC, data= data.set.haz, 
        col=c2, 
        medcol=c3, 
        whiskcol=c1, 
        staplecol=c3, 
        boxcol=c3, 
        outcol=c3, 
        pch=23, cex=2,
        xlab = "Property condition",
        ylab = "Sale Price",
        main = "Comparing Average Sale Prices by Property Condition (Hazelwood)"
)

################## ################ #########################################################

################## CLUSTERING ###############################################################

# Import Data 
data.set <- import.csv('price_date_diff.csv')

# Fix string date differences
data.set$DateDiff1 <- substr(data.set$DateDiff1, 0, regexpr(pattern = ' ', data.set$DateDiff1) - 1 )
data.set$DateDiff1 <- as.numeric(data.set$DateDiff1)

# Normalize the data
#data.set <- scale(data.set)
#data.set <- apply(data.set, 2, normlz)

# Remove rows with outliers
data.set.no.out <- data.set[-which(identify_outliers(data.set$PriceDiff1_per)) , ]
data.set.no.out <- data.set.no.out[-which(identify_outliers(data.set.no.out$DateDiff1)) , ]

library(ggplot2)
ggplot(data.set.no.out, 
       aes(DateDiff1, PriceDiff1_per, col = SALEPRICE)) + geom_point()


set.seed(100)

# Cluster by Ownership duration and price difference (in %)
price.date.cluster <- kmeans(data.set.no.out[,c("DateDiff1","PriceDiff1_per")], 7, 20)
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
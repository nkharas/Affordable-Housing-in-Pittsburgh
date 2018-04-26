# Author : Nick Kharas
# Description : Explore data, use clustering to find reasonable classification boundaries

library(ggplot2)
library(gridExtra)

# Function to identify outliers in a data frame
# Outliers are anything greater or less than 1.5 times the interquartile range
# Borrowed from - http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
identify_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- (x < (qnt[1] - H) | x > (qnt[2] + H))
  return(y)
}

options(scipen = 999)


################## HYPOTHESIS TEST #########################################################
# Compare property characteristics in Hazelwood with other neigborhoods in Pittsburgh
################## ################ #########################################################

data.set <- read.csv('hypothesis_test_data.csv', sep="," ,header=TRUE)

# Distinguish between Hazelwood (pin code 15207) and Rest of Allegheny County
data.set$Neighborhood <- ifelse(data.set$PROPERTYZIP == 15207,
                                'Hazelwood',
                                'Rest of Allegheny County')

# Convert the date difference into a valid number, change days to years
data.set$DateDiff1 <- substr(data.set$DateDiff1, 0, regexpr(pattern = ' ', data.set$DateDiff1) - 1 )
data.set$DateDiff1 <- as.numeric(data.set$DateDiff1) / 365

# Remove outliers to reduce bias
data.set <- data.set[-which(identify_outliers(data.set$DateDiff1)) , ]

# One tailed Hypothesis test
# Alternate hypothesis: avg_duration(Hazelwood) > avg_duration(Rest)
# Null Hypothesis: avg_duration(Hazelwood) <= avg_duration(Rest)
# Ignore NULL durations to avoid misleading results (nrow(data.set[is.na(data.set$DateDiff1),]))
t.test(formula =  DateDiff1 ~ Neighborhood, data = data.set,
       alternative = "greater", 
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE, 
       conf.level = 0.95,
       na.action = na.omit)
# p-value = 0.002951

# Box plot to capture above result

# Color schemes for box plot - https://tomizonor.wordpress.com/2013/04/18/color-boxplot/
c1 <- rainbow(2)
c2 <- rainbow(2, alpha=0.2)
c3 <- rainbow(2, v=0.7)

boxplot(formula =  DateDiff1 ~ Neighborhood, data = data.set,
        col=c2, 
        medcol=c3, 
        whiskcol=c1, 
        staplecol=c3, 
        boxcol=c3, 
        outcol=c3, 
        pch=23, cex=2,
        xlab = "Neighborhood", 
        ylab = "Ownership Duration (in Years)", 
        main = "Comparing Property Ownership Tenure between Hazelwood and Rest of Pittsburgh")



################## DATA EXPLORATION #########################################################
# Evaluate property condiions and  draw comparisons between Hazelwood and other neigborhoods in Pittsburgh
################## ################ #########################################################
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



################## CLUSTERING ###############################################################

# Normally, flippers buy a house to sell it quickly and cheaply and make a quick profit, without improving its condition.
# The idea of clustering is to identify those properties that were sold within a short duration from the last purchase.
# We pay particular attention to those clusters whose centroids are closer to zero.
# We run a k-means cluster on time difference and price difference of a property from the last sale, 
#  and use the output to build our dependent variable for classification modeling.

################## ################ #########################################################

# Import Data 
data.set <- read.csv('price_date_diff.csv', sep="," ,header=TRUE)

# Convert string date differences (in days) to numeric, and divide by 365 to measure in years
data.set$DateDiff1 <- substr(data.set$DateDiff1, 0, regexpr(pattern = ' ', data.set$DateDiff1) - 1 )
data.set$DateDiff1 <- as.numeric(data.set$DateDiff1) / 365

# Some date differences are negative (~ 45 entries), indicating that the previous sale occured after the last sale.
# We assume that this is a data entry mistake, and simply flip the negative sign
data.set$DateDiff1 <- ifelse(data.set$DateDiff1 < 0, -1 * data.set$DateDiff1, data.set$DateDiff1)

# Remove rows with price difference = infinity (divide by 0 result)
data.set <- data.set[is.finite(data.set$PriceDiff1_per), ]

# Remove outliers to get rid of unrealistic values that would othewise skew our results
data.set <- data.set[-which(identify_outliers(data.set$PriceDiff1_per)) , ]
data.set <- data.set[-which(identify_outliers(data.set$DateDiff1)) , ]

# Scale the data before running the k-means clustering algorithm
data.set.scale <- cbind(data.set[1],scale(data.set[-1]))

set.seed(100)

# Tried different outputs with different number of clusters,
# The output from the run with 16 centers and 20 max iterations made the most sense
# cluster <- kmeans(data.set.scale[, c(2:3)], 5) # 4, 5, 7, 10, 11
# cluster <- kmeans(data.set.scale[, c(2:3)], 20,20)
cluster <- kmeans(data.set.scale[, c(2:3)], 16, 20)

# Observe centers and number of iterations
cluster$centers
cluster$iter

# Visualize the segments according to the identified clusters
p <- ggplot(data.set, aes(DateDiff1, PriceDiff1_per,
            color = as.factor(cluster$cluster) )) + geom_point()
p <- p + labs(title = "Clustering by Ownership Duration and Price", x = "Ownership Duration (in Years)"
                           , y = "% Appreciation in Price from Last Sale") # Axis titles
p <- p + theme(legend.position = "none")
p

# Bind clusters to the actual data set and use as dependent variable in predictive modeling
data.set.cluster <- cbind(data.set, cluster = cluster$cluster)

# Fetch clusters whose centers are closer to zero 
required.cluster <- unique(data.set.cluster[which((data.set.cluster$PriceDiff1_per == 0 & data.set.cluster$DateDiff1 == 0) | (data.set.cluster$PriceDiff1_per == 20 & data.set.cluster$DateDiff1 == 0))
                                                  , ncol(data.set.cluster)])

# Plot to check if we targeted the right clusters
q <- ggplot(data.set.cluster[which(data.set.cluster$cluster %in% required.cluster),]
                 , aes(DateDiff1, PriceDiff1_per )) + geom_point()
q

# Label data points within the targeted clusters as potential flips 
data.set.cluster$ClassLabel <- ifelse(data.set.cluster$cluster %in% required.cluster, 1, 0)

# Write the results to a flat file to be used in STEP 3
write.table(data.set.cluster[, c(1, ncol(data.set.cluster))], "data_with_clusters.csv", quote=FALSE, sep=",", row.names=FALSE)
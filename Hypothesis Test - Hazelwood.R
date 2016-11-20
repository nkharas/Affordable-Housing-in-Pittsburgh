# Author: Nick Kharas
# Description: Run a hypothesis test to evaluate difference in prices between properties in Hazelwood as against the rest of Pittsburgh

library(ggplot2)

import.csv <- function(filename){
  return(read.csv(filename,sep="," ,header=TRUE))
}

write.csv <- function(ob, filename) {
  write.table(ob, filename, quote = FALSE, sep = ",", row.names = FALSE)
}

# Function to remove outliers, anything greater or less than 1.5 times the interquartile range
# http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}


flippers <- import.csv('flippers1.csv')
#flippers1 <- import.csv('flippers1.csv')
#flippers2 <- import.csv('flippers2.csv')
total <- import.csv('totaldataset.csv')

# flippers <- rbind(flippers1, flippers2)

# Remove Hazelwood data from flippers to avoid double counting
flippers <- flippers[which(flippers$PROPERTYZIP_x != 15207) , ] # 15207 is the Zip code for Hazelwood


braddock <- total[which(total$PROPERTYZIP_x == 15104) , ] # Zip code for Braddock
hazelwood <- total[which(total$PROPERTYZIP_x == 15207) , ] # Zip code for Hazelwood
all <-  total[which(total$PROPERTYZIP_x != 15207) , ] # All except Hazelwood

# Fetch the number of days
hazelwood_days <- substr(hazelwood$DateDiff1, 0, regexpr(pattern = ' ', hazelwood$DateDiff1) - 1 )
hazelwood_days <- as.numeric(hazelwood_days)
hazelwood_days <- remove_outliers(hazelwood_days) # Remove outliers
hazelwood_days <- hazelwood_days[hazelwood_days >= 0] # Remove negative day count to remove noise

braddock_days <- substr(braddock$DateDiff1, 0, regexpr(pattern = ' ', braddock$DateDiff1) - 1 )
braddock_days <- as.numeric(braddock_days)
braddock_days <- remove_outliers(braddock_days) # Remove outliers
braddock_days <- braddock_days[braddock_days >= 0] # Remove negative day count to remove noise

total_days <- substr(all$DateDiff1, 0, regexpr(pattern = ' ', all$DateDiff1) - 1 )
total_days <- as.numeric(total_days)
total_days <- remove_outliers(total_days) # Remove outliers
total_days <- total_days[total_days >= 0] # Remove negative day count to remove noise

flipper_days <- substr(flippers$DateDiff1, 0, regexpr(pattern = ' ', flippers$DateDiff1) - 1 )
flipper_days <- as.numeric(flipper_days)

# boxplot(hazelwood_days)
# boxplot(flipper_days)
# boxplot(total_days)
# t.test(x = hazelwood_days, y = flipper_days, alternative = "greater", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
# t.test(x = hazelwood_days, y = braddock_days, alternative = "greater", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(x = hazelwood_days, y = total_days, alternative = "greater", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# Color schemes for box plot - https://tomizonor.wordpress.com/2013/04/18/color-boxplot/
c1 <- rainbow(2)
c2 <- rainbow(2, alpha=0.2)
c3 <- rainbow(2, v=0.7)

boxplot(hazelwood_days, total_days, names = c('Hazelwood', 'Rest of Allegheny County'), 
        col=c2, 
        medcol=c3, 
        whiskcol=c1, 
        staplecol=c3, 
        boxcol=c3, 
        outcol=c3, 
        pch=23, cex=2,
        xlab = "Neighborhood", 
        ylab = "Number of Days Property Was Held", 
        main = "Comparing Property Ownership Tenure between Hazelwood and Rest of Pittsburgh")
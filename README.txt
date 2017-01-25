The goal is to assess investor behavior in Pittsburgh's housing and real estate market, and identify potentially bad investors. Bad investors are flippers and milkers, who buy properties only to sell them off as quickly as possible to make a profit. They do not maintain or repair the property, and deteriorate the quality of the neighborhood the house is located in.

For our analysis, we used the Allegheny county property assessments data maintained by the WPRDC. Below is a summary of how we ran our analysis.

- Step 1 - Data Cleaning - Python script to download and clean the CSV data from WPRDC's website. Some data cleaning steps involve
	- Filters - Since the focus is on the housing market, the filter includes only those residential properties whose sale transaction is considered valid.
	- Impute missing values for categorical variables - For yes/no variables, the values are either yes or null. This is fixed.
	- Calculate the duration for which the property was owned before it got sold - This would help identify flippers.
	- Calculate the absolute and % appreciation (or depreciation) of the property sale value.
	- For the above, the file has fields "PrevSaleDate" and "PrevSaleValue".
  OUTPUT - Files for modeling and visualizations
  
  
- Step 2 - Data Exploration and Clustering - R script to gather insights from and identify patterns in the property assessments data.
	
	- The HYPOTHESIS TEST section runs a hypothesis test to see if ownership durations in Hazelwood are significantly higher than that in the rest of Allegheny County.
		- The purpose of this analysis is to confirm if Hazelwood has a tighter community spirit, as longer house ownership durations reflect more positively on the neighborhood.
		- The Hazelwood neighborhood in Pittsburgh is a good pilot to implement any suggestions arising from this analysis.
		- Hazelwood has been hit hard after the steel mills shut down, and it represents the socio-economic challenges Pittsburgh faces. 
		- The difference in average house ownership durations between Hazelwood and the rest of alegheny county is only marginal and not significant.
		- For this analysis, we convert the ownership duration from days to years to make visualizations more interpretable.
	
	- The DATA EXPLORATION section compares the average house sale values based on house condition, and compares the averages between Hazelwood and the rest of Allegheny County.
		- Some houses in poor or unsound conditions have been sold for highly inflated values, indicating the presence of bad investors in the housing market.
		- However, we do not find such cases in Hazelwood. This is a god sign that bad investors have not shaken Hazelwood's housing market and community spirit.
		- The result is a box plot. The edges of each box indicate the interquartile range of sale values. The flat line within each box is the median.
		
	- The CLUSTERING section runs a k-means clustering algorithm on ownership duration, % appreciation or depreciation in house values, and sale price.
		- To identify bad investors, we are looking at houses that were sold off quickly. The data does no come with class labels.
		- Applying general definitions of flippers and milkers to mark class labels could ignore hidden patterns in the data.
		- Clusterig helps identify boundaries to mark class labels. We flagged properties potentially under the hands of bad investors when
			- The property was owned for less than three years, and
			- The property value depreciated, or appreciated less than 15%, or was sold for less than $90,000.


- Step 3 - Classification Models - R Script to identify bad investor behavior. This script compares the results of various classification models.
	- We use the results of clustering to set class labels for good and bad investors.
	- The data is split into train and test, and the rows are shuffled.
		- 80% of the data is used for training the model, while the remaining 20% is used for testing and validation.
		- Since only about 10% of the transactions are labeled as possible bad investments, the split ensures that the train and test data sets have data from both classes in the same proportion. This is expected, as the housing market is not completely overrun by flippers and milkers.
	- Gradient Boosting, Random Forest, and Conditional Inference Trees (a modeling technique based on unbiased recursive partitioning) with an AUC of 0.70-0.73
	- Logistic regression is computationally expensive with a large number of categorical variables.
	- Decision trees perform poorly, as the data is biased against bad investments, as explained above.
	- The program returns results from various models and compares them through a ROC curve.


Challenges in modeling:
- The data is limited in volume. It covers residential properties in Allegheny County alone.
- The assessments data has missing values for some fields, including those which determine our class labels. 
	- For example, some parcels do not have previous sale records, making it difficult to determine how long the house was held before it was sold.
	- We cannot assume that this is missing data, as the property may not have changed ownership more than once in its lifetime.
- The assessments data set gives us the parcel characteristics as of now, and not as when the property was last sold.
	- For example, the condition of a property may have either improved or deteriorated after it was last sold four years ago, but we have no way to find out.
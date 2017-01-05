# Author: Max Haytovich
# Changes and comments by Nick Kharas
# Import packages
import pandas as pd
import re

# Note: The callable API does not return all the data as a chunk. The URL downloads the data as of Dec 2016.
# For data as of other time periods, please refer to the WPRDC data set https://data.wprdc.org/dataset/property-assessments
# The download is broken into separate chunks and concatenated again, for below reasons.
# 1. Pandas allocates the data type of a field based on initial values. This will break if the initial values of an alphanumeric column are numbers
# 2. Another workaround for the problem in (1) is to import the file in a single chunk. However, I did not have enough memory for that brute force approach
propAssessment = pd.read_csv('https://data.wprdc.org/dataset/2b3df818-601e-4f06-b150-643557229491/resource/7ef45883-68d6-48ee-a466-e625e31809f6/download/alleghenycountymasterfile01022017.csv', low_memory=False, iterator = True, chunksize = 50000)
propAssessment = pd.DataFrame(pd.concat(propAssessment))

# Since we are evaluating the housing problem in Pitsburgh, we only want to analyze valid sales of residential properties
propAssessment = propAssessment[(( (propAssessment['SALEDESC'] == 'VALID SALE') | (propAssessment['SALEDESC'] == 'OTHER VALID') ) & (propAssessment['CLASSDESC'] == 'RESIDENTIAL'))]
non_decimal = re.compile(r'[^\d.]+')

# Added by Nick
# Null filling - Only values that are flagged are given in the file, rest are null. For example, HOMESTEADFLAG = "HOM" if true, NULL otherwise. This is fixed
propAssessment.loc[(propAssessment.HOMESTEADFLAG != 'HOM') | (pd.isnull(propAssessment['HOMESTEADFLAG'])),'HOMESTEADFLAG'] = 'None'
propAssessment.loc[(propAssessment.FARMSTEADFLAG != 'FRM') | (pd.isnull(propAssessment['FARMSTEADFLAG'])),'FARMSTEADFLAG'] = 'None'
propAssessment.loc[(propAssessment.CLEANGREEN != 'Y') | (pd.isnull(propAssessment['CLEANGREEN'])),'CLEANGREEN'] = 'N'

# Find the sale price appreciation / depreciation when the property was last sold
propAssessment['PriceDiff1'] = propAssessment.SALEPRICE - propAssessment.PREVSALEPRICE
propAssessment['PriceDiff1_per'] = (((propAssessment.SALEPRICE - propAssessment.PREVSALEPRICE) * 1.0) / (propAssessment.SALEPRICE * 1.0)) * 100.0
propAssessment['PriceDiff2'] = propAssessment.PREVSALEPRICE - propAssessment.PREVSALEPRICE2
propAssessment['PriceDiff2_per'] = (((propAssessment.PREVSALEPRICE - propAssessment.PREVSALEPRICE2) * 1.0) / (propAssessment.PREVSALEPRICE * 1.0)) * 100.0

# Find he ownership duraton before the property was last sold
propAssessment['SALEDATE'] = pd.to_datetime(propAssessment['SALEDATE'], errors='coerce')
propAssessment['PREVSALEDATE'] = pd.to_datetime(propAssessment['PREVSALEDATE'], errors='coerce')
propAssessment['PREVSALEDATE2'] = pd.to_datetime(propAssessment['PREVSALEDATE2'], errors='coerce')
propAssessment['DateDiff1'] = propAssessment['SALEDATE'] - propAssessment['PREVSALEDATE']
propAssessment['DateDiff2'] = propAssessment['PREVSALEDATE'] - propAssessment['PREVSALEDATE2']

# Added by Nick
# There is missing data on Previous Sale Date
# Some transactions have the previous sale date coming after the most recent sale date, leading to negative ownership duration
# These are considered as errors and ignored, 45 out of 180000 records
propAssessment = propAssessment[(propAssessment['DateDiff1'] >= pd.Timedelta('0 days')) | (pd.isnull(propAssessment['DateDiff1']))]

# Use the logic given in the property assesments data dictionary to highlight vacant properties
propAssessment['isVacant'] = 0
propAssessment.loc[(propAssessment.FAIRMARKETBUILDING == 0) ,'isVacant'] = 1

###### This is the output class label
# This line of code was updated to use the results of clustering in STEP 2 to determine class boundaries, and flag the class labels for training and testing predctive models.
propAssessment['ClassLabel'] = 0
propAssessment.loc[(propAssessment.DateDiff1 < pd.Timedelta('1100 days')) & (propAssessment.DateDiff1 >= pd.Timedelta('0 days')) & ((propAssessment.PriceDiff1_per < 15) | (propAssessment.SALEPRICE < 90000)),'ClassLabel'] = 1

# Added by Nick
# Output file for classification modeling
propAssessment[['PROPERTYZIP','MUNIDESC','SCHOOLDESC','NEIGHDESC','TAXDESC','OWNERDESC','USEDESC','LOTAREA','HOMESTEADFLAG','FARMSTEADFLAG','CLEANGREEN','SALEPRICE','COUNTYBUILDING','COUNTYLAND','COUNTYTOTAL','COUNTYEXEMPTBLDG','LOCALBUILDING','LOCALLAND','LOCALTOTAL','FAIRMARKETBUILDING','FAIRMARKETLAND','FAIRMARKETTOTAL','STYLEDESC','STORIES','YEARBLT','EXTFINISH_DESC','ROOFDESC','BASEMENTDESC','GRADE','CONDITIONDESC','CDU','TOTALROOMS','BEDROOMS','FULLBATHS','HALFBATHS','HEATINGCOOLINGDESC','FIREPLACES','BSMTGARAGE','FINISHEDLIVINGAREA','isVacant','ClassLabel']].to_csv('property_assessments_clean.csv', index = False, index_label = False)

# Output file for clustering
price_date_diff = propAssessment[['PriceDiff1_per','DateDiff1','SALEPRICE']][propAssessment['PriceDiff1_per'].notnull()] # Get price and date differences where price is not null
price_date_diff[price_date_diff['DateDiff1'].notnull()].to_csv('price_date_diff.csv', index = False, index_label = False)

# Output file for hypothesis test
propAssessment[['PROPERTYZIP','DateDiff1']].to_csv('hypothesis_test_data.csv', index = False, index_label = False)

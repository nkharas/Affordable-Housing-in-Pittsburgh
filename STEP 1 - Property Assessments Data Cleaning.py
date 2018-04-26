# Author: Max Haytovich
# Changes: Nick Kharas

# Import packages
import pandas as pd
import re

# Note: The URL downloads the data as of Mar 2018.
# For data as of other time periods, please refer to the WPRDC data set https://data.wprdc.org/dataset/property-assessments
# The download is broken into separate chunks and concatenated again, as:
# 1. Pandas allocates the data type of a field based on initial values. This will break if the initial values in an alphanumeric column are numbers
# 2. Another workaround for the problem is to import the file as a single chunk. However, I did not have enough memory for that brute force approach
propAssessment = pd.read_csv('https://data.wprdc.org/dataset/2b3df818-601e-4f06-b150-643557229491/resource/f2b8d575-e256-4718-94ad-1e12239ddb92/download/assessments.csv', low_memory=False, iterator = True, chunksize = 50000)
propAssessment = pd.DataFrame(pd.concat(propAssessment))

# Since we want to identify genuine housing problems, we only analyze residential properties with relevant sale codes
# This step is important to ultimately identify and segment houses that are in poor condition
# Definition of sale codes - https://data.wprdc.org/dataset/9e0ce87d-07b8-420c-a8aa-9de6104f61d6/resource/96474373-bcdb-42cf-af5d-3683e326e227/download/sales-validation-codes-dictionary.pdf
propAssessment = propAssessment[(propAssessment['SALECODE'].isin(['0','U','UR','14','16','33','34','36','99','BK'])  ) & (propAssessment['CLASSDESC'] == 'RESIDENTIAL') ]
non_decimal = re.compile(r'[^\d.]+')

# Handle missing values where obvious
# for the below three columns, only the "yes" values are filled. The rest are null. For example, HOMESTEADFLAG = "HOM" if true, NULL otherwise. This is fixed
propAssessment.loc[(propAssessment.HOMESTEADFLAG != 'HOM') | (pd.isnull(propAssessment['HOMESTEADFLAG'])),'HOMESTEADFLAG'] = 'None'
propAssessment.loc[(propAssessment.FARMSTEADFLAG != 'FRM') | (pd.isnull(propAssessment['FARMSTEADFLAG'])),'FARMSTEADFLAG'] = 'None'
propAssessment.loc[(propAssessment.CLEANGREEN != 'Y') | (pd.isnull(propAssessment['CLEANGREEN'])),'CLEANGREEN'] = 'N'

# Find the sale price appreciation / depreciation when the property was last sold
propAssessment['PriceDiff1'] = propAssessment.SALEPRICE - propAssessment.PREVSALEPRICE
propAssessment['PriceDiff1_per'] = (((propAssessment.SALEPRICE - propAssessment.PREVSALEPRICE) * 1.0) / (propAssessment.SALEPRICE * 1.0)) * 100.0
propAssessment['PriceDiff2'] = propAssessment.PREVSALEPRICE - propAssessment.PREVSALEPRICE2
propAssessment['PriceDiff2_per'] = (((propAssessment.PREVSALEPRICE - propAssessment.PREVSALEPRICE2) * 1.0) / (propAssessment.PREVSALEPRICE * 1.0)) * 100.0

# Find the ownership duraton before the property was last sold
propAssessment['SALEDATE'] = pd.to_datetime(propAssessment['SALEDATE'], errors='coerce')
propAssessment['PREVSALEDATE'] = pd.to_datetime(propAssessment['PREVSALEDATE'], errors='coerce')
propAssessment['PREVSALEDATE2'] = pd.to_datetime(propAssessment['PREVSALEDATE2'], errors='coerce')
propAssessment['DateDiff1'] = propAssessment['SALEDATE'] - propAssessment['PREVSALEDATE']
propAssessment['DateDiff2'] = propAssessment['PREVSALEDATE'] - propAssessment['PREVSALEDATE2']

# Use the logic given in the property assesments data dictionary by the WPRDC (link below) to highlight vacant properties
# https://data.wprdc.org/dataset/2b3df818-601e-4f06-b150-643557229491/resource/cc4bafd2-25b6-41d7-83aa-d16bc211b020/download/alleghenycountypropertyassessmentdatauserguide-4.pdf
propAssessment['isVacant'] = 0
propAssessment.loc[(propAssessment.FAIRMARKETBUILDING == 0) ,'isVacant'] = 1

# Output file for modeling with classification algorithms
propAssessment[['PARID','PROPERTYZIP','MUNIDESC','SCHOOLDESC','NEIGHDESC','TAXDESC','OWNERDESC','USEDESC','LOTAREA','HOMESTEADFLAG','FARMSTEADFLAG','CLEANGREEN','SALEPRICE','SALECODE','COUNTYBUILDING','COUNTYLAND','COUNTYEXEMPTBLDG','LOCALBUILDING','LOCALLAND','FAIRMARKETBUILDING','FAIRMARKETLAND','STYLEDESC','STORIES','YEARBLT','EXTFINISH_DESC','ROOFDESC','BASEMENTDESC','GRADE','CONDITIONDESC','CDU','TOTALROOMS','BEDROOMS','FULLBATHS','HALFBATHS','HEATINGCOOLINGDESC','FIREPLACES','BSMTGARAGE','FINISHEDLIVINGAREA','isVacant']].to_csv('property_assessments_clean.csv', index = False, index_label = False)

# Output file for clustering
price_date_diff = propAssessment[['PARID','PriceDiff1_per','DateDiff1','SALEPRICE']][propAssessment['PriceDiff1_per'].notnull()] # Get price and date differences where price is not null
price_date_diff[price_date_diff['DateDiff1'].notnull()].to_csv('price_date_diff.csv', index = False, index_label = False)

# Output file for hypothesis test
propAssessment[['PROPERTYZIP','DateDiff1']].to_csv('hypothesis_test_data.csv', index = False, index_label = False)

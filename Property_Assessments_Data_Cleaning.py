# Changes by Nick
# 1. No join between assessment and sales, only assessments used
# 2. Price differences not used in marking class label, instead both price differneces used as input variable

import pandas as pd
import re

# propAssessment = pd.read_csv('F:\\DataAnalyticsClub\\DACaseComp\\DatasetDist\\Datasets\\Property_assessment_allegheny.csv', low_memory=False) 
propAssessment = pd.read_csv('alleghenycountymasterfile11012016.csv', low_memory=False, iterator = True, chunksize = 50000)
propAssessment = pd.DataFrame(pd.concat(propAssessment))
propAssessment = propAssessment[(( (propAssessment['SALEDESC'] == 'VALID SALE') | (propAssessment['SALEDESC'] == 'OTHER VALID') ) & (propAssessment['CLASSDESC'] == 'RESIDENTIAL'))]
# propAssessment = propAssessment[(propAssessment['SALEDESC'] == 'VALID SALE') | (propAssessment['SALEDESC'] == 'OTHER VALID')]
# propAssessment = propAssessment[propAssessment['CLASSDESC'] == 'RESIDENTIAL']
#print('head(propAssessment)')
#print(propAssessment.head())
# propAssessment = pd.read_csv('http://URL')
# salesData = pd.read_csv('F:\\DataAnalyticsClub\\DACaseComp\\DatasetDist\\Datasets\\aa301110120160700.csv', low_memory=False)
# salesData = pd.read_csv('aa301110120160700.csv', low_memory=False)
non_decimal = re.compile(r'[^\d.]+')

# propAssessment[propAssessment.PROPERTYUNIT == 'UNIT 902'].PROPERTYUNIT
# propAssessment.PROPERTYUNIT = propAssessment.PROPERTYUNIT.str.replace(r'\D+', '')
# propAssessment.PROPERTYUNIT = propAssessment.PROPERTYUNIT.apply(pd.to_numeric)
# propAssessment.PROPERTYUNIT = propAssessment.PROPERTYUNIT.fillna(-1).astype(int)

# salesData.PROPERTYUNITNO = salesData.PROPERTYUNITNO.str.replace(r'\D+', '')
# salesData.PROPERTYUNITNO = salesData.PROPERTYUNITNO.apply(pd.to_numeric)
# salesData.PROPERTYUNITNO = salesData.PROPERTYUNITNO.fillna(-1).astype(int)
# salesData.rename(columns={'PROPERTYUNITNO': 'PROPERTYUNIT'}, inplace=True)
# properSalesData = pd.DataFrame.copy(salesData[['PARID','PROPERTYUNIT','SCHOOLDESC','MUNIDESC','SALEDATE','PRICE','SALEDESC','INSTRTYPDESC']])
# properAssesment = pd.DataFrame.copy(propAssessment[['PARID','PROPERTYUNIT','NEIGHDESC','TAXDESC','TAXSUBCODE_DESC','OWNERDESC','USEDESC','LOTAREA','CLEANGREEN','FARMSTEADFLAG','ABATEMENTFLAG','COUNTYTOTAL','COUNTYEXEMPTBLDG','LOCALTOTAL','FAIRMARKETTOTAL','STYLEDESC','STORIES','YEARBLT','EXTFINISH_DESC','ROOFDESC','BASEMENTDESC','GRADEDESC','CONDITIONDESC','CDUDESC','TOTALROOMS','BEDROOMS','FULLBATHS','HALFBATHS','HEATINGCOOLINGDESC','FIREPLACES','BSMTGARAGE','FINISHEDLIVINGAREA', 'PREVSALEDATE', 'PREVSALEDATE2', 'PREVSALEPRICE', 'PREVSALEPRICE2']][(propAssessment['CLASSDESC']=='RESIDENTIAL') & (propAssessment['HOMESTEADFLAG']!='HOM')])
# df_join = pd.merge(properSalesData, properAssesment, on = ['PROPERTYUNIT', 'PARID'])

# Added by Nick
# Only values to be flagged are marked, rest are null, so need null filling
propAssessment.loc[(propAssessment.HOMESTEADFLAG != 'HOM') | (pd.isnull(propAssessment['HOMESTEADFLAG'])),'HOMESTEADFLAG'] = 'None'
propAssessment.loc[(propAssessment.FARMSTEADFLAG != 'FRM') | (pd.isnull(propAssessment['FARMSTEADFLAG'])),'FARMSTEADFLAG'] = 'None'
propAssessment.loc[(propAssessment.CLEANGREEN != 'Y') | (pd.isnull(propAssessment['CLEANGREEN'])),'CLEANGREEN'] = 'N'


propAssessment['PriceDiff1'] = propAssessment.SALEPRICE - propAssessment.PREVSALEPRICE
propAssessment['PriceDiff1_per'] = (((propAssessment.SALEPRICE - propAssessment.PREVSALEPRICE) * 1.0) / (propAssessment.SALEPRICE * 1.0)) * 100.0
propAssessment['PriceDiff2'] = propAssessment.PREVSALEPRICE - propAssessment.PREVSALEPRICE2
propAssessment['PriceDiff2_per'] = (((propAssessment.PREVSALEPRICE - propAssessment.PREVSALEPRICE2) * 1.0) / (propAssessment.PREVSALEPRICE * 1.0)) * 100.0

propAssessment['SALEDATE'] = pd.to_datetime(propAssessment['SALEDATE'], errors='coerce')
propAssessment['PREVSALEDATE'] = pd.to_datetime(propAssessment['PREVSALEDATE'], errors='coerce')
propAssessment['PREVSALEDATE2'] = pd.to_datetime(propAssessment['PREVSALEDATE2'], errors='coerce')
propAssessment['DateDiff1'] = propAssessment['SALEDATE'] - propAssessment['PREVSALEDATE']
propAssessment['DateDiff2'] = propAssessment['PREVSALEDATE'] - propAssessment['PREVSALEDATE2']

# Added by Nick
# There is missing data on Previous Sale Date
# Some transactions have the previous sale date coming after the most recent sale date, these are considered as errors and ignored, 45 out of 180000 records
propAssessment = propAssessment[(propAssessment['DateDiff1'] >= pd.Timedelta('0 days')) | (pd.isnull(propAssessment['DateDiff1']))]

#flippers1 = df_join[(df_join.DateDiff1 < pd.Timedelta('366 days')) & (df_join.DateDiff1 > pd.Timedelta('1 days'))]
#flippers1 = flippers1[(flippers1.PriceDiff1 < 100000) & (flippers1.PriceDiff1 > 0)]
                
#flippers1.groupby('GRADEDESC').agg('count')     
#flippers1[flippers1.MUNIDESC == 15207].groupby('GRADEDESC').agg('count')    

#flippers2 = df_join[(df_join.DateDiff2 < pd.Timedelta('366 days')) & (df_join.DateDiff2 > pd.Timedelta('1 days'))]
#flippers2 = flippers2[(flippers2.PriceDiff2 < 100000) & (flippers2.PriceDiff2 > 0)]

propAssessment['isVacant'] = 0
propAssessment.loc[(propAssessment.FAIRMARKETBUILDING == 0) ,'isVacant'] = 1

propAssessment['ClassLabel'] = 0
#df_join.loc[(df_join.DateDiff2 < pd.Timedelta('366 days')) & (df_join.DateDiff2 > pd.Timedelta('1 days')) & (df_join.PriceDiff2 < 100000) & (df_join.PriceDiff2 > 0), 'ClassLabel'] = 1
#df_join.loc[(df_join.DateDiff1 < pd.Timedelta('366 days')) & (df_join.DateDiff1 > pd.Timedelta('1 days')) & (df_join.PriceDiff1 < 100000) & (df_join.PriceDiff1 > 0),'ClassLabel'] = 1
#propAssessment.loc[(propAssessment.DateDiff1 < pd.Timedelta('366 days')) & (propAssessment.DateDiff1 >= pd.Timedelta('0 days')) & (propAssessment.PriceDiff1 < 100000),'ClassLabel'] = 1
#propAssessment.loc[(propAssessment.DateDiff1 < pd.Timedelta('366 days')) & (propAssessment.DateDiff1 >= pd.Timedelta('0 days')),'ClassLabel'] = 1
#propAssessment.loc[(propAssessment.DateDiff2 < pd.Timedelta('366 days')) & (propAssessment.DateDiff2 >= pd.Timedelta('0 days')),'ClassLabel'] = 1
propAssessment.loc[(propAssessment.DateDiff1 < pd.Timedelta('1100 days')) & (propAssessment.DateDiff1 >= pd.Timedelta('0 days')) & ((propAssessment.PriceDiff1_per < 15) | (propAssessment.SALEPRICE < 90000)),'ClassLabel'] = 1
#propAssessment.loc[(propAssessment.DateDiff2 < pd.Timedelta('1100 days')) & (propAssessment.DateDiff2 >= pd.Timedelta('0 days')) & ((propAssessment.PriceDiff2_per < 15)| (propAssessment.PREVSALEPRICE < 90000)),'ClassLabel'] = 1
#df_join.to_csv('F:\\DataAnalyticsClub\\DACaseComp\\DatasetDist\\Datasets\\BestFile.csv')

# Added by Nick
# Note that adding the price difference leads to a lot of null values, only 40000 out of 180000 rows remain without nulls in any column
propAssessment[['PROPERTYZIP','MUNIDESC','SCHOOLDESC','NEIGHDESC','TAXDESC','OWNERDESC','USEDESC','LOTAREA','HOMESTEADFLAG','FARMSTEADFLAG','CLEANGREEN','SALEPRICE','COUNTYBUILDING','COUNTYLAND','COUNTYTOTAL','COUNTYEXEMPTBLDG','LOCALBUILDING','LOCALLAND','LOCALTOTAL','FAIRMARKETBUILDING','FAIRMARKETLAND','FAIRMARKETTOTAL','STYLEDESC','STORIES','YEARBLT','EXTFINISH_DESC','ROOFDESC','BASEMENTDESC','GRADE','CONDITIONDESC','CDU','TOTALROOMS','BEDROOMS','FULLBATHS','HALFBATHS','HEATINGCOOLINGDESC','FIREPLACES','BSMTGARAGE','FINISHEDLIVINGAREA','isVacant','ClassLabel']].to_csv('property_assessments_clean.csv', index = False, index_label = False)

# Output file for clustering
price_date_diff = propAssessment[['PriceDiff1_per','DateDiff1','SALEPRICE']][propAssessment['PriceDiff1_per'].notnull()] # Get price and date differences where price is not null
price_date_diff[price_date_diff['DateDiff1'].notnull()].to_csv('price_date_diff.csv', index = False, index_label = False)

import pandas as pd
import re
propAssesment = pd.read_csv('F:\\DataAnalyticsClub\\DACaseComp\\DatasetDist\\Datasets\\Property_assessment_allegheny.csv', low_memory=False)
salesData = pd.read_csv('F:\\DataAnalyticsClub\\DACaseComp\\DatasetDist\\Datasets\\aa301110120160700.csv', low_memory=False)
non_decimal = re.compile(r'[^\d.]+')

#propAssesment[propAssesment.PROPERTYUNIT == 'UNIT 902'].PROPERTYUNIT

propAssesment.PROPERTYUNIT = propAssesment.PROPERTYUNIT.str.replace(r'\D+', '')

propAssesment.PROPERTYUNIT = propAssesment.PROPERTYUNIT.apply(pd.to_numeric)

propAssesment.PROPERTYUNIT = propAssesment.PROPERTYUNIT.fillna(-1).astype(int)

salesData.PROPERTYUNITNO = salesData.PROPERTYUNITNO.str.replace(r'\D+', '')

salesData.PROPERTYUNITNO = salesData.PROPERTYUNITNO.apply(pd.to_numeric)

salesData.PROPERTYUNITNO = salesData.PROPERTYUNITNO.fillna(-1).astype(int)

salesData.rename(columns={'PROPERTYUNITNO': 'PROPERTYUNIT'}, inplace=True)

properSalesData = pd.DataFrame.copy(salesData[['PARID','PROPERTYUNIT','SCHOOLDESC','MUNIDESC','SALEDATE','PRICE','SALEDESC','INSTRTYPDESC']])

properAssesment = pd.DataFrame.copy(propAssesment[['PARID','PROPERTYUNIT','NEIGHDESC','TAXDESC','TAXSUBCODE_DESC','OWNERDESC','USEDESC','LOTAREA','CLEANGREEN','FARMSTEADFLAG','ABATEMENTFLAG','COUNTYTOTAL','COUNTYEXEMPTBLDG','LOCALTOTAL','FAIRMARKETTOTAL','STYLEDESC','STORIES','YEARBLT','EXTFINISH_DESC','ROOFDESC','BASEMENTDESC','GRADEDESC','CONDITIONDESC','CDUDESC','TOTALROOMS','BEDROOMS','FULLBATHS','HALFBATHS','HEATINGCOOLINGDESC','FIREPLACES','BSMTGARAGE','FINISHEDLIVINGAREA', 'PREVSALEDATE', 'PREVSALEDATE2', 'PREVSALEPRICE', 'PREVSALEPRICE2']][(propAssesment['CLASSDESC']=='RESIDENTIAL') & (propAssesment['HOMESTEADFLAG']!='HOM')])

df_join = pd.merge(properSalesData, properAssesment, on = ['PROPERTYUNIT', 'PARID'])

df_join['PriceDiff1'] = df_join.PRICE - df_join.PREVSALEPRICE

df_join['PriceDiff2'] = df_join.PREVSALEPRICE - df_join.PREVSALEPRICE2

df_join['SALEDATE'] = pd.to_datetime(df_join['SALEDATE'])
df_join['PREVSALEDATE'] = pd.to_datetime(df_join['PREVSALEDATE'], errors='coerce')
df_join['PREVSALEDATE2'] = pd.to_datetime(df_join['PREVSALEDATE2'], errors='coerce')
df_join['DateDiff1'] = df_join['SALEDATE'] - df_join['PREVSALEDATE']
df_join['DateDiff2'] = df_join['PREVSALEDATE'] - df_join['PREVSALEDATE2']

flippers1 = df_join[(df_join.DateDiff1 < pd.Timedelta('366 days')) & (df_join.DateDiff1 > pd.Timedelta('1 days'))]
flippers1 = flippers1[(flippers1.PriceDiff1 < 100000) & (flippers1.PriceDiff1 > 0)]
                
flippers1.groupby('GRADEDESC').agg('count')     
flippers1[flippers1.MUNIDESC == 15207].groupby('GRADEDESC').agg('count')    

flippers2 = df_join[(df_join.DateDiff2 < pd.Timedelta('366 days')) & (df_join.DateDiff2 > pd.Timedelta('1 days'))]
flippers2 = flippers2[(flippers2.PriceDiff2 < 100000) & (flippers2.PriceDiff2 > 0)]
                      
df_join['ClassLabel'] = 0
df_join.loc[(df_join.DateDiff2 < pd.Timedelta('366 days')) & (df_join.DateDiff2 > pd.Timedelta('1 days')) & (df_join.PriceDiff2 < 100000) & (df_join.PriceDiff2 > 0), 'ClassLabel'] = 1
df_join.loc[(df_join.DateDiff1 < pd.Timedelta('366 days')) & (df_join.DateDiff1 > pd.Timedelta('1 days')) & (df_join.PriceDiff1 < 100000) & (df_join.PriceDiff1 > 0),'ClassLabel'] = 1
df_join.to_csv('F:\\DataAnalyticsClub\\DACaseComp\\DatasetDist\\Datasets\\BestFile.csv')


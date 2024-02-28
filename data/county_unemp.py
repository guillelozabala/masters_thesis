
'''
returns unemployment.csv

'''

import pandas as pd

df = pd.read_csv(r'./ladata64County.txt',sep="\t")
df = df.drop('footnote_codes',axis=1)
df = df.rename(columns={df.columns.values.tolist()[0]: 'series_id', df.columns.values.tolist()[1]: 'year',
                        df.columns.values.tolist()[2]: 'period', df.columns.values.tolist()[3]: 'value',})

df['state_fip'] = df['series_id'].str.slice(5,7)
df['county_fip'] = df['series_id'].str.slice(7,10)
df["fips"] = df["state_fip"] + df["county_fip"]
df['series'] = df['series_id'].str.slice(18,20)
df['month'] = df['period'].str.slice(1,3)

'''
measure_code	measure_text

03	unemployment rate

04	unemployment

05	employment

06	labor force

07	employment-population ratio

08	labor force participation rate

09	civilian noninstitutional population

https://users.nber.org/~notom/for_tal/LAUS/readme.txt 

M=Monthly, M13=Annual

'''

df = df.drop(df[(df.series != '03') | (df.period == 'M13')].index)
df = df.drop('series_id',axis=1)
df = df.drop('series',axis=1)
df = df.drop('period',axis=1)
df = df.drop('state_fip',axis=1)
df = df.drop('county_fip',axis=1)

dfips = pd.read_csv(r'./county_fips_master.csv',encoding = "ISO-8859-1")
dfips = dfips[['fips','county_name','state_abbr','state_name']]
dfips['fips'] = dfips['fips'].astype(str).str.rjust(5,'0')

df_final = df.merge(dfips, left_on='fips', right_on='fips')
df_final.to_csv(r'./unemployment.csv',sep=',',index=False)
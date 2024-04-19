
'''
returns unemployment_rate.csv, unemployment.csv, employment.csv, labor_force.csv, 
employment_population_ratio.csv, labor_force_participation_rate.csv

'''

import pandas as pd
import os

abspath = os.path.realpath('county_unemp.py')
dname = os.path.dirname(abspath)
os.chdir(dname)

df = pd.read_csv(r'./data/labor_market_outcomes/ladata64County.txt',sep="\t")
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

07	employment-population ratio (empty)

08	labor force participation rate (empty)

09	civilian noninstitutional population

https://users.nber.org/~notom/for_tal/LAUS/readme.txt 

MXX=Monthly, M13=Annual

'''

dfips = pd.read_csv(r'./data/fips/county_fips_master.csv',encoding = "ISO-8859-1")
dfips = dfips[['fips','county_name','state_abbr','state_name']]
dfips['fips'] = dfips['fips'].astype(str).str.rjust(5,'0')


df_unemp_rate = df.drop(df[(df.series != '03') | (df.period == 'M13')].index)
df_unemp = df.drop(df[(df.series != '04') | (df.period == 'M13')].index)
df_emp = df.drop(df[(df.series != '05') | (df.period == 'M13')].index)
df_lab_force = df.drop(df[(df.series != '06') | (df.period == 'M13')].index)

df_dict = {'unemployment_rate':df_unemp_rate,
           'unemployment':df_unemp,
           'employment':df_emp,
           'labor_force':df_lab_force
           }

for k, i_df in df_dict.items():
    i_df = i_df.drop('series_id',axis=1)
    i_df = i_df.drop('series',axis=1)
    i_df = i_df.drop('period',axis=1)
    i_df = i_df.drop('state_fip',axis=1)
    i_df = i_df.drop('county_fip',axis=1)

    i_df_final= i_df.merge(dfips, left_on='fips', right_on='fips')
    i_df_final.to_csv(f'./data/labor_market_outcomes/{k}.csv',sep=',',index=False)

import pandas as pd

data_naics = dict()
naics_df = ['four_naics_02to05', 'four_naics_06to10', 'four_naics_11to16']
for naic in naics_df:
    naic_subset = pd.read_csv(f'./sector_composition/combined_data_{naic}.csv', header=0)
    naic_subset['fipstate'] = naic_subset['fipstate'].astype(str).str.rjust(2, '0')
    naic_subset['fipscty'] = naic_subset['fipscty'].astype(str).str.rjust(3, '0')
    naic_subset['naics'] = naic_subset['naics'].astype(str).str.ljust(6, '0')
    data_naics[naic] = naic_subset

for naic, df in data_naics.items():
    data_naics[naic]['fips'] = data_naics[naic]['fipstate'] + data_naics[naic]['fipscty']
    data_naics[naic] = data_naics[naic].drop(['fipstate', 'fipscty'], axis=1)
    data_naics[naic]['naics'] = data_naics[naic]['naics'].str[:2]
    data_naics[naic] = data_naics[naic].groupby(['naics', 'year', 'fips'])['emp'].sum().reset_index()

unique_naics = data_naics['four_naics_02to05']['naics'].unique()
for naic, df in data_naics.items():
    for codes in unique_naics:
        data_naics[naic]['emp_' + codes + '_ratio'] = (data_naics[naic]['emp'] / data_naics[naic].groupby(['year', 'fips'])['emp'].transform('sum')).round(4)

for naic, df in data_naics.items():
    data_naics[naic] = data_naics[naic][data_naics[naic]['naics'] == '11']  
    data_naics[naic] = data_naics[naic].drop(['naics'], axis=1)


data_naics_merged = pd.concat([data_naics['four_naics_02to05'], data_naics['four_naics_06to10'], data_naics['four_naics_11to16']])
data_naics_merged.to_csv(r'./sector_composition/naics_shares_merged.csv',sep=',',index=False)
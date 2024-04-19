import pandas as pd

data_naics = dict()
naics_df = ['four_naics_02to05', 'four_naics_06to10', 'four_naics_11to16']
for naic in naics_df:
    naic_subset = pd.read_csv(f'./sector_composition/combined_data_{naic}.csv', header=0)
    naic_subset['fipstate'] = naic_subset['fipstate'].astype(str).str.rjust(2, '0')
    naic_subset['fipscty'] = naic_subset['fipscty'].astype(str).str.rjust(3, '0')
    naic_subset['naics'] = naic_subset['naics'].astype(str).str.ljust(6, '0')
    data_naics[naic] = naic_subset

wage_percentiles_02to16 = pd.read_csv(r'./data/industry_weighted_percs_02to16.csv', header=0)
wage_percentiles_02to16['naics'] = wage_percentiles_02to16['naics'].astype(str)

for naic, df in data_naics.items():
    data_naics[naic] = df.merge(wage_percentiles_02to16, on=['naics','year'], how='left')



'''
four_naics_02to05 = pd.read_csv(r'./sector_composition/combined_data_four_naics_02to05.csv', header=0)
four_naics_06to10 = pd.read_csv(r'./sector_composition/combined_data_four_naics_06to10.csv', header=0)
four_naics_11to16 = pd.read_csv(r'./sector_composition/combined_data_four_naics_11to16.csv', header=0)
'''


four_naics_02to05['fipscty'].unique()
wage_percentiles_02to16['naics'].unique()

four_naics_02to05

four_naics_02to05['fipscty'].astype(str).str.rjust(3, '0')

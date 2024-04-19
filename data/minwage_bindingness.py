import pandas as pd

data_naics = dict()
naics_df = ['four_naics_02to05', 'four_naics_06to10', 'four_naics_11to16']
for naic in naics_df:
    naic_subset = pd.read_csv(f'./data/sector_composition/combined_data_{naic}.csv', header=0)
    naic_subset['fipstate'] = naic_subset['fipstate'].astype(str).str.rjust(2, '0')
    naic_subset['fipscty'] = naic_subset['fipscty'].astype(str).str.rjust(3, '0')
    naic_subset['naics'] = naic_subset['naics'].astype(str).str.ljust(6, '0')
    data_naics[naic] = naic_subset

wage_percentiles_02to16 = pd.read_csv(r'./data/sector_wages/industry_weighted_percs_02to16.csv', header=0)
wage_percentiles_02to16['naics'] = wage_percentiles_02to16['naics'].astype(str)

for naic, df in data_naics.items():
    data_naics[naic] = df.merge(wage_percentiles_02to16, on=['naics','year'], how='left')
    data_naics[naic]['fips'] = data_naics[naic]['fipstate'] + data_naics[naic]['fipscty']


county_percentiles = pd.DataFrame(index = range(len(data_naics['four_naics_02to05']['fips'].unique())*(2016-2001)+1), columns = ['year', 'fips','h_pct10', 'h_pct25', 'h_median', 'h_pct75', 'h_pct90'])

h = -1
for i, file in enumerate(data_naics):
    for j, year in enumerate(data_naics[file]['year'].unique()):
        for k, fip in enumerate(data_naics[file]['fips'].unique()):
            
            h += 1

            county = data_naics[file][(data_naics[file]['fips'] == fip) & (data_naics[file]['year'] == year)]

            county_percentiles.iloc[h,0] = year
            county_percentiles.iloc[h,1] = fip

            total_county_emp = county['emp'].sum()
            county.loc[:, 'industry_emp_shares'] = county['emp'].apply(lambda x: x/total_county_emp)

            county_percentiles.iloc[h,2] = (county['h_pct10']*county['industry_emp_shares']).sum()
            county_percentiles.iloc[h,3] = (county['h_pct25']*county['industry_emp_shares']).sum()
            county_percentiles.iloc[h,4] = (county['h_median']*county['industry_emp_shares']).sum()
            county_percentiles.iloc[h,5] = (county['h_pct75']*county['industry_emp_shares']).sum()   
            county_percentiles.iloc[h,6] = (county['h_pct90']*county['industry_emp_shares']).sum()

county_percentiles.to_csv(r'./data/sector_wages/county_percentiles.csv',sep=',',index=False)
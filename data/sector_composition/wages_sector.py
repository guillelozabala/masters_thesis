import pandas as pd
import glob
import re

'''
https://www.bls.gov/oes/tables.htm

'''

data_wages_1998 = pd.read_excel(r'./sector_composition/sector_wages/nat3d_sic_1998_dl.xls', skiprows=31, header=1)
data_wages_1999 = pd.read_excel(r'./sector_composition/sector_wages/nat3d_sic_1999_dl.xls', skiprows=35, header=1)
data_wages_2000 = pd.read_excel(r'./sector_composition/sector_wages/nat3d_sic_2000_dl.xls', skiprows=33, header=1)
data_wages_2001 = pd.read_excel(r'./sector_composition/sector_wages/nat3d_sic_2001_dl.xls', header=0)

columns_to_add = ['h_pct10', 'h_pct25', 'h_pct75', 'h_pct90']

for data_wages in [data_wages_1998, data_wages_1999, data_wages_2000]:
    for column in columns_to_add:
        data_wages[column] = float('nan')

data_wages_1998 = data_wages_1998[['year', 'sic', 'occ_code', 'tot_emp', 'h_median'] + columns_to_add]
data_wages_1999 = data_wages_1999[['year', 'sic', 'occ_code', 'tot_emp', 'h_median'] + columns_to_add]
data_wages_2000 = data_wages_2000[['year', 'sic', 'occ_code', 'tot_emp', 'h_median'] + columns_to_add]
data_wages_2001 = data_wages_2001[['year', 'sic', 'occ_code', 'tot_emp', 'h_median'] + columns_to_add]

data_wages_98to01 = pd.concat([data_wages_1998, data_wages_1999, data_wages_2000, data_wages_2001], axis=0)

# SIC to NAICS
xls_wages = [file.replace('sector_composition/sector_wages\\', '') for file in glob.glob(r'sector_composition/sector_wages/*.xls*')]

sic_files = ['nat3d_sic_1998_dl.xls', 'nat3d_sic_1999_dl.xls', 'nat3d_sic_2000_dl.xls', 'nat3d_sic_2001_dl.xls', 'field_descriptions.xls']

xls_wages = [x for x in xls_wages if x not in sic_files]

data_wages = dict()
years = []

for file in xls_wages:
    file_name = file.replace(r".xls*", "")
    data_wages[file_name] = pd.read_excel(f'./sector_composition/sector_wages/{file}', header=0)
    match = re.search(r'\d{4}', file)
    if match:
        years.append(int(match.group()))
    data_wages[file_name]['year'] = years[-1]
    data_wages[file_name].columns = map(str.lower, data_wages[file_name].columns)
    data_wages[file_name] = data_wages[file_name][['year', 'naics', 'occ_code', 'tot_emp', 'h_median'] + columns_to_add]


industry_weighted_percs = pd.DataFrame(index=range(sum(len(data_wages[key]) for key in data_wages.keys())), columns=['year', 'naics', 'h_median'] + columns_to_add)
k = -1
for i, file in enumerate(xls_wages):
    for j, naic in enumerate(data_wages[file]['naics'].unique()):
        k += 1

        industry = data_wages[file][data_wages[file]['naics'] == naic]
        # throw bad apples away
        industry = industry.apply(
            lambda x: pd.to_numeric(x, errors='coerce')
            )#.dropna(subset=['tot_emp','h_median'] + columns_to_add,how='any')

        industry_weighted_percs.iloc[i+k,0] = industry['year'].iloc[0]
        industry_weighted_percs.iloc[i+k,1] = naic

        total_industry_emp = industry['tot_emp'].sum()
        industry['industry_emp_shares'] = industry['tot_emp'].apply(lambda x: x/total_industry_emp)

        industry_weighted_percs.iloc[i+k,2] = (industry['h_median']*industry['industry_emp_shares']).sum()
        industry_weighted_percs.iloc[i+k,3] = (industry['h_pct10']*industry['industry_emp_shares']).sum()
        industry_weighted_percs.iloc[i+k,4] = (industry['h_pct25']*industry['industry_emp_shares']).sum()
        industry_weighted_percs.iloc[i+k,5] = (industry['h_pct75']*industry['industry_emp_shares']).sum()   
        industry_weighted_percs.iloc[i+k,6] = (industry['h_pct90']*industry['industry_emp_shares']).sum()



industry_weighted_percs = industry_weighted_percs.dropna(
                subset=['h_median'] + columns_to_add,
                how='any'
                )


industry_weighted_percs.to_csv(r'./data/industry_weighted_percs_02to16.csv',sep=',',index=False)






'''
data_wages_2002 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_2002_dl.xls', header=0)

data_wages_2003 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_may2003_dl.xls', header=0)
data_wages_2004 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_may2004_dl.xls', header=0)
data_wages_2005 = pd.read_excel(r'./sector_composition/sector_wages/nat4d__may2005_dl.xls', header=0)
data_wages_2006 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_may2006_dl.xls', header=0)
data_wages_2007 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_May2007_dl.xls', header=0)

data_wages_2008 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2008_dl.xls', header=0)
data_wages_2009 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2009_dl.xls', header=0)
data_wages_2010 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2010_dl.xls', header=0)
data_wages_2011 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2011_dl.xls', header=0)

data_wages_2012_1 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2012_dl_1_113300_517100.xls', header=0)
data_wages_2012_2 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2012_dl_2_517200_999300.xls', header=0)
data_wages_2013_1 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2013_dl_1_113300_517100.xls', header=0)
data_wages_2013_2 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2013_dl_2_517200_999300.xls', header=0)

data_wages_2014 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2014_dl.xlsx', header=0)
data_wages_2015 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2015_dl.xlsx', header=0)
data_wages_2016 = pd.read_excel(r'./sector_composition/sector_wages/nat4d_M2016_dl.xlsx', header=0)
'''



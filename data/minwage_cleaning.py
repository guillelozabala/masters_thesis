
'''
returns minwage_clean_states_ub.csv and minwage_clean_fed_ub.csv

'''

import pandas as pd

df = pd.read_csv(r'./minwage_raw.csv',sep=',')

df = df.drop(['1968 (a)','1970 (a)','1972','1976 (a)','1979','1980','1981','1988'],axis=1)
df = df[df['State or otherjurisdiction'] != 'Guam']
df = df[df['State or otherjurisdiction'] != 'Puerto Rico']
df = df[df['State or otherjurisdiction'] != 'U.S. Virgin Islands']
df = df.replace({'\$':''}, regex=True)

for i in range(len(df.iloc[:,0])):
    for j in range(len(df.columns)):
        if df.iloc[i,j] == '...':
            df.iloc[i,j] = df.iloc[0,j]

df = df.replace({'\[c\]':''}, regex=True).replace({'\(d\)':''}, regex=True).replace({'\(e\)':''}, regex=True)
df = df.replace({'\(g\)':''}, regex=True).replace({'\(h\)':''}, regex=True)

for i in range(len(df.iloc[:,0])):
    for j in range(len(df.columns)):
        if '-' in df.iloc[i,j]:
            df.iloc[i,j] = df.iloc[i,j].split('-')[1].lstrip().split(' ')[0]
        elif '/' in df.iloc[i,j]:
            df.iloc[i,j] = df.iloc[i,j].split('/')[1].lstrip().split(' ')[0]

df.iloc[:,1:] = df.iloc[:,1:].replace({' ':''}, regex=True)
df.iloc[:,1:-1] = df.iloc[:,1:-1].astype(float)
df = df.melt(id_vars="State or otherjurisdiction", 
        var_name="Year", 
        value_name="Value")

df.to_csv(r'./minwage_clean_states_ub.csv',sep=',',index=False)

new_df = pd.DataFrame()
for i in df['Year'].unique():
    year_data = df[df['Year'] == i]
    fed_value = year_data.loc[year_data['State or otherjurisdiction'] == 'Federal (FLSA)', 'Value'].values[0]
    year_data['Value'] = year_data['Value'].apply(lambda x: max(x, fed_value))
    new_df = pd.concat([new_df,year_data])
new_df = new_df.reset_index(drop=True)

new_df.to_csv(r'./minwage_clean_fed_ub.csv',sep=',',index=False)
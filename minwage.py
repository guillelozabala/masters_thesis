
import requests
from bs4 import BeautifulSoup
import pandas as pd

URL = "https://www.dol.gov/agencies/whd/state/minimum-wage/history"
page = requests.get(URL)
soup = BeautifulSoup(page.content, "html.parser")

tables = soup.find_all('table')
tables_dict = {}
for i in range(len(tables)): 
    tables_name = f"table{i}"
    tables_dict[tables_name] = tables[i]

titles = soup.find_all('th')
text_titles = [i.text for i in titles]

mwage_dict = {}
for i in range(len(tables)):
    col_data = tables_dict[f'table{i}'].find_all('tr') 
    single_row_data = {}
    k = 0
    for row in col_data:
        row_data = row.find_all('td')
        single_row_data[f'{k}'] = [data.text.strip() for data in row_data]
        k += 1
    single_row_data.pop('0')
    mwage_name = f"mwage{i}"
    mwage_dict[mwage_name] = pd.DataFrame.from_dict(single_row_data, orient='index')

df = pd.DataFrame()
for i in range(len(tables)):
    df = pd.concat([df,mwage_dict[f'mwage{i}']], axis=1)
df = df.set_axis(text_titles, axis=1)
states = df.iloc[:,0]
df = df.drop('State or otherjurisdiction', axis=1)
df = pd.concat([states,df],axis=1)
print(df)
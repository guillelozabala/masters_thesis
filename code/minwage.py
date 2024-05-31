import requests
from bs4 import BeautifulSoup
import pandas as pd
import os

'''
returns minwage_raw.csv
'''


# Get the absolute path of the current script
abspath = os.path.realpath('minwage.py')

# Get the directory name of the current script
dname = os.path.dirname(abspath)

# Change the current working directory to the script's directory
os.chdir(dname)

# URL of the webpage to scrape
URL = "https://www.dol.gov/agencies/whd/state/minimum-wage/history"

# Send a GET request to the URL and get the page content
page = requests.get(URL)

# Create a BeautifulSoup object to parse the HTML content
soup = BeautifulSoup(page.content, "html.parser")

# Find all the tables in the HTML content
tables = soup.find_all('table')

# Create a dictionary to store the tables
tables_dict = {}
for i in range(len(tables)):
    tables_name = f"table{i}"
    tables_dict[tables_name] = tables[i]

# Find all the table headers (titles) in the HTML content
titles = soup.find_all('th')

# Extract the text from the table headers
text_titles = [i.text for i in titles]

# Create a dictionary to store the data from each table
mwage_dict = {}
for i in range(len(tables)):
    # Find all the rows in the current table
    col_data = tables_dict[f'table{i}'].find_all('tr')

    # Create a dictionary to store the data for each row
    single_row_data = {}
    k = 0
    for row in col_data:
        # Find all the cells in the current row
        row_data = row.find_all('td')

        # Extract the text from each cell and store it in the dictionary
        single_row_data[f'{k}'] = [data.text.strip() for data in row_data]
        k += 1

    # Remove the first column (index 0) from the row data
    single_row_data.pop('0')

    # Create a unique name for the current table's data
    mwage_name = f"mwage{i}"

    # Create a DataFrame from the row data and store it in the dictionary
    mwage_dict[mwage_name] = pd.DataFrame.from_dict(single_row_data, orient='index')

# Create an empty DataFrame to store the final combined data
df = pd.DataFrame()

# Concatenate the DataFrames from each table into the final DataFrame
for i in range(len(tables)):
    df = pd.concat([df, mwage_dict[f'mwage{i}']], axis=1)

# Set the column names of the final DataFrame using the table headers
df = df.set_axis(text_titles, axis=1)

# Extract the 'State or other jurisdiction' column as a separate Series
states = df.iloc[:, 0]

# Drop the 'State or other jurisdiction' column from the final DataFrame
df = df.drop('State or otherjurisdiction', axis=1)

# Concatenate the 'states' Series and the final DataFrame horizontally
df = pd.concat([states, df], axis=1)

# Save the final DataFrame as a CSV file
df.to_csv(r'./data/source/minimum_wage/minwage_raw.csv', sep=',', index=False)

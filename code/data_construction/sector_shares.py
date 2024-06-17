import pandas as pd
import os

from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql.types import IntegerType

# Build session 
os.environ["JAVA_HOME"] = 'C:\Program Files\Java\jre-1.8'
spark = SparkSession.builder.appName('sectors_shares_merging').getOrCreate()

# Create an empty dictionary to store the data
data_naics = dict()

# List of naics dataframes
naics_df = ['four_naics_02to05', 'four_naics_06to10', 'four_naics_11to16']

# Loop through each naics dataframe
for naic in naics_df:
    # Read the files
    naic_subset = pd.read_csv(f'./data/intermediate/sector_composition/combined_data_{naic}.csv', header=0)

    # Preprocess the columns
    naic_subset['fipstate'] = naic_subset['fipstate'].astype(str).str.rjust(2, '0')
    naic_subset['fipscty'] = naic_subset['fipscty'].astype(str).str.rjust(3, '0')
    naic_subset['naics'] = naic_subset['naics'].astype(str).str.ljust(6, '0')

    # Store the naics dataframe in the dictionary
    data_naics[naic] = naic_subset

# Loop through each naics dataframe in the dictionary
for naic, df in data_naics.items():
    # Create a new column 'fips' by combining 'fipstate' and 'fipscty'
    data_naics[naic]['fips'] = data_naics[naic]['fipstate'] + data_naics[naic]['fipscty']
    
    # Drop unnecessary columns
    data_naics[naic] = data_naics[naic].drop(['fipstate', 'fipscty'], axis=1)
    
    # Extract the first two characters from 'naics' column
    data_naics[naic]['naics'] = data_naics[naic]['naics'].str[:2]
    
    # Group the data by 'naics', 'year', and 'fips' and calculate the sum of 'emp'
    data_naics[naic] = data_naics[naic].groupby(['naics', 'year', 'fips'])['emp'].sum().reset_index()

# Calculate the ratio of 'emp' column for each unique 'naics' code

unique_naics = data_naics['four_naics_02to05']['naics'].unique()
for naic, df in data_naics.items():
    for codes in unique_naics:
        data_naics[naic]['emp_ratio'] = (data_naics[naic]['emp'] / data_naics[naic].groupby(['year', 'fips'])['emp'].transform('sum')).round(4)

for naic, df in data_naics.items():
    data_naics[naic] = data_naics[naic].drop('emp',axis=1)
    data_naics[naic] = data_naics[naic].pivot_table(index=['year','fips'], columns='naics')
    data_naics[naic].columns = data_naics[naic].columns.droplevel().rename(None)
    data_naics[naic] = data_naics[naic].reset_index()
    data_naics[naic].columns = 'emp_' + data_naics[naic].columns + '_ratio'
    data_naics[naic] = data_naics[naic].rename({'emp_year_ratio' : 'year', 'emp_fips_ratio' : 'fips'}, axis=1)
    data_naics[naic]['state_code'] = data_naics[naic]['fips'].str[:2]
    data_naics[naic]['county_code'] = data_naics[naic]['fips'].str[2:]

data_naics['four_naics_02to05']

# Save the dataframes as CSV files
data_naics['four_naics_02to05'].to_csv(r'./data/intermediate/sector_shares/naics_shares_02to05.csv', sep=',', index=False)
data_naics['four_naics_06to10'].to_csv(r'./data/intermediate/sector_shares/naics_shares_06to10.csv', sep=',', index=False)
data_naics['four_naics_11to16'].to_csv(r'./data/intermediate/sector_shares/naics_shares_11to16.csv', sep=',', index=False)

# Create Spark DataFrames from the data
naics_shares_02to05 = spark.createDataFrame(data_naics['four_naics_02to05'])
naics_shares_06to10 = spark.createDataFrame(data_naics['four_naics_06to10'])
naics_shares_11to16 = spark.createDataFrame(data_naics['four_naics_11to16'])

# Union the Spark DataFrames
naics_shares = naics_shares_02to05.union(naics_shares_06to10).union(naics_shares_11to16)

# Save the merged DataFrame as a CSV file
naics_shares.toPandas().to_csv(r'./data/intermediate/sector_shares/naics_shares_merged.csv', sep=',', index=False)



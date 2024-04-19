
'''
returns combined_data_three_naics.csv

'''

import os, glob
import pyspark
import pandas as pd

from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql.types import IntegerType, StructType, StructField, StringType

# Build session 
os.environ["JAVA_HOME"] = 'C:\Program Files\Java\jre-1.8'
spark = SparkSession.builder.appName('sectors').getOrCreate()

# Get a list of all CSV files in the current folder except 'combined_data_three_naics.csv' and 'combined_data_four_naics.csv'
csv_files = [file.replace('sector_composition\\', '') for file in glob.glob(r'sector_composition/*.csv')]
csv_files.pop(csv_files.index('combined_data_three_naics.csv'))
csv_files.pop(csv_files.index('combined_data_four_naics_02to05.csv'))
csv_files.pop(csv_files.index('combined_data_four_naics_06to10.csv'))
csv_files.pop(csv_files.index('combined_data_four_naics_11to16.csv'))

# Define an empty schema
column_names = "fipstate|fipscty|naics|emp|year"
schema = StructType([StructField(c, StringType()) for c in column_names.split("|")])

# Create an empty DataFrame
combined_data = spark.createDataFrame(data=[], schema=schema)

# https://www.datalumos.org/datalumos/project/117464/version/V1/view?flag=follow&path=/datalumos/117464/fcr:versions/V1/Imputed-CBP-Files&type=folder&pageSize=100&sortOrder=(?title)&sortAsc=true

# Iterate over each CSV file and vertically concatenate the data
for file in csv_files:
    data = spark.read.csv(
        f'./sector_composition/{file}', 
        header=True
        )
    data = data.filter(
        F.col("naics").rlike(r'^\d{3}//')
        )
    data = data.withColumn(
        "naics", 
        F.regexp_replace(F.col("naics"), r"///", "")
        )
    data = data.withColumn(
        "year",
        F.lit(file[9:13])
        )
    data = data.withColumn(
        "emp",
        data.emp.cast(IntegerType())
        )
    data = data.withColumn(
        "year",
        data.year.cast(IntegerType())
        )
    data = data.withColumn(
        "fipstate", 
        F.lpad(F.col("fipstate"), 2, "0")
        )
    data = data.withColumn(
        "fipscty", 
        F.lpad(F.col("fipscty"), 3, "0")
        )
    combined_data = combined_data.union(
        data
        )

# Save the combined data to a new CSV file
combined_data.toPandas().to_csv(r'./combined_data_three_naics.csv',sep=',',index=False)

# Same with four digits
combined_data_4d = spark.createDataFrame(data=[], schema=schema)

for file in csv_files:
    data_4d = spark.read.csv(
        f'./sector_composition/{file}', 
        header=True
        )
    data_4d = data_4d.filter(
        F.col("naics").rlike(r'^\d{4}//')
        )
    data_4d = data_4d.withColumn(
        "naics", 
        F.regexp_replace(F.col("naics"), r"//", "")
        )
    data_4d = data_4d.withColumn(
        "year",
        F.lit(file[9:13])
        )
    data_4d = data_4d.withColumn(
        "emp",
        data_4d.emp.cast(IntegerType())
        )
    data_4d = data_4d.withColumn(
        "year",
        data_4d.year.cast(IntegerType())
        )
    data_4d = data_4d.withColumn(
        "fipstate", 
        F.lpad(F.col("fipstate"), 2, "0")
        )
    data_4d = data_4d.withColumn(
        "fipscty", 
        F.lpad(F.col("fipscty"), 3, "0")
        )
    combined_data_4d = combined_data_4d.union(
        data_4d
        )

# Save the combined data to a new CSV file
# java.lang.OutOfMemoryError: GC overhead limit exceeded
# combined_data_4d.toPandas().to_csv(r'./combined_data_four_naics.csv',sep=',',index=False) 

combined_data_4d.filter(
    (F.col('year') > 2001) & (F.col('year') < 2006)
    ).toPandas().to_csv(r'./sector_composition/combined_data_four_naics_02to05.csv',sep=',',index=False) 

combined_data_4d.filter(
    (F.col('year') > 2005) & (F.col('year') < 2011)
    ).toPandas().to_csv(r'./sector_composition/combined_data_four_naics_06to10.csv',sep=',',index=False) 

combined_data_4d.filter(
    F.col('year') > 2010
    ).toPandas().to_csv(r'./sector_composition/combined_data_four_naics_11to16.csv',sep=',',index=False) 
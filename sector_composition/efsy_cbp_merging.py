import os, glob
import pyspark
import pandas as pd

from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql.types import IntegerType, StructType, StructField, StringType

# Build session 
os.environ["JAVA_HOME"] = 'C:\Program Files\Java\jre-1.8'
spark = SparkSession.builder.appName('sectors').getOrCreate()

# Get a list of all CSV files in the current folder
#os.chdir(r"./sector_composition")
csv_files = glob.glob('*.csv')

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

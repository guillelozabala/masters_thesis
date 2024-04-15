
import os
import pyspark
import pandas as pd

from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql.types import IntegerType, StructType, StructField, StringType

# Build session 
os.environ["JAVA_HOME"] = 'C:\Program Files\Java\jre-1.8'
spark = SparkSession.builder.appName('emp_and_wage').getOrCreate()

'https://download.bls.gov/pub/time.series/oe/'

'https://download.bls.gov/pub/time.series/oe/oe.txt'

'''
Field #/Data Element		Length		Value(Example)		

1.  series_id		  	30		OEUM000040000000000000001

2.  seasonal		 	1		U

3.  areatype_code	  	1 		M

4.  industry_code 	  	6		000000

5.  occupation_code 	  	6		000000 
    
6.  datatype_code	  	2		01

7.  state_code	  		2		01

8.  area_code		 	7		0000400

9.  sector_code			6		11--12

10.  series_title		2KB		Employment for All Occupations in All Industries in Albany, GA

11.  footnote_codes		10		1

12.  begin_year		   	4		1980

13. begin_period	   	3		A01		
				
14.  end_year		   	4		2002		

15.  end_period		   	3		A01

'''

'''
datatype_code	datatype_name
01	Employment
02	Employment percent relative standard error
03	Hourly mean wage
04	Annual mean wage
05	Wage percent relative standard error
06	Hourly 10th percentile wage
07	Hourly 25th percentile wage
08	Hourly median wage
09	Hourly 75th percentile wage
10	Hourly 90th percentile wage
11	Annual 10th percentile wage
12	Annual 25th percentile wage
13	Annual median wage
14	Annual 75th percentile wage
15	Annual 90th percentile wage
16	Employment per 1,000 jobs
17	Location Quotient

'''

'''
seasonal_code	seasonal_text
S	Seasonally Adjusted
U	Not Seasonally Adjusted


'''

df_wage = spark.read.option("header", "true") \
    .option("delimiter", "\t") \
    .option("inferSchema", "true") \
    .csv(r'./sector_composition/oeseries.txt')

df_wage = df_wage.filter(
        df_wage.seasonal == 'U'
    ) # no S

df_wage = df_wage.filter(
        df_wage.areatype_code == 'S'
    )

df_wage = df_wage.filter(
        (df_wage.datatype_code == '06') | 
        (df_wage.datatype_code == '07') |
        (df_wage.datatype_code == '08') |
        (df_wage.datatype_code == '09') |
        (df_wage.datatype_code == '10')
    )

df_wage = df_wage.drop('series_id                     ')
df_wage = df_wage.drop('seasonal')
df_wage = df_wage.drop('areatype_code')
df_wage = df_wage.drop('area_code') # not needed, using states
df_wage = df_wage.drop('footnote_codes')

df_wage.show()


df_wage_1 = spark.read.option("header", "true") \
    .option("delimiter", "\t") \
    .option("inferSchema", "true") \
    .csv(r'./sector_composition/oedata1AllData.txt')

df_wage_1 = df_wage_1.withColumn(
    "surv_abbrev",
    F.substring(F.col("series_id                     "), 1, 2)
)

df_wage_1 = df_wage_1.withColumn(
    "seasonal",
    F.substring(F.col("series_id                     "), 3, 1)
)

df_wage_1 = df_wage_1.withColumn(
    "area_code",
    F.substring(F.col("series_id                     "), 5, 7)
)

df_wage_1 = df_wage_1.withColumn(
    "industry_code",
    F.substring(F.col("series_id                     "), 13, 6)
)

df_wage_1 = df_wage_1.withColumn(
    "occupation_code",
    F.substring(F.col("series_id                     "), 20, 6)
)

df_wage_1 = df_wage_1.withColumn(
    "datatype_code",
    F.substring(F.col("series_id                     "), 21, 2)
)


df_wage_1 = df_wage_1.filter(
        (df_wage_1.datatype_code == '06') | 
        (df_wage_1.datatype_code == '07') |
        (df_wage_1.datatype_code == '08') |
        (df_wage_1.datatype_code == '09') |
        (df_wage_1.datatype_code == '10')
    )

df_wage_1 = df_wage_1.drop('series_id                     ')
df_wage_1 = df_wage_1.drop('footnote_codes')

df_wage_1.show()




df_wage = spark.read.option("header", "true") \
    .option("delimiter", "\t") \
    .option("inferSchema", "true") \
    .csv(r'./sector_composition/oeseries.txt')

df_wage_1 = spark.read.option("header", "true") \
    .option("delimiter", "\t") \
    .option("inferSchema", "true") \
    .csv(r'./sector_composition/oedata1AllData.txt')

df_merged = df_wage.join(df_wage_1, on='series_id                     ', how='inner')

df_merged = df_merged.filter(
        (df_merged.datatype_code == '06') | 
        (df_merged.datatype_code == '07') |
        (df_merged.datatype_code == '08') |
        (df_merged.datatype_code == '09') |
        (df_merged.datatype_code == '10')
    )

df_merged = df_merged.filter(
        df_merged.areatype_code == 'S'
    )

df_merged = df_merged.filter(
        df_merged.seasonal == 'U'
    ) # no S

df_merged = df_merged.drop('series_id                     ')
df_merged = df_merged.drop('seasonal')
df_merged = df_merged.drop('areatype_code')
df_merged = df_merged.drop('area_code') # not needed, using states
df_merged = df_merged.drop('footnote_codes')

df_merged.show()


df_merged.select('year').distinct().show()

'''
returns county_demographics.csv

'''

import os
import pyspark
import pandas as pd

from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql.types import IntegerType

### build session 

os.environ["JAVA_HOME"] = 'C:\Program Files\Java\jre-1.8'
spark = SparkSession.builder.appName('demographics').getOrCreate()

### read the data

demo_data = spark.read.text(r'./data/demographics/us_1990_2020_19ages_adjusted.txt')

### obtain variables from txt file (see https://seer.cancer.gov/popdata/popdic.html for reference)

demo_data = demo_data.withColumn(
    "year",
    F.substring(F.col("value"), 1, 4)
)

demo_data = demo_data.withColumn(
    "state",
    F.substring(F.col("value"), 5, 2)
)

demo_data = demo_data.withColumn(
    "state_fip",
    F.substring(F.col("value"), 7, 2)
)

demo_data = demo_data.withColumn(
    "county_fip",
    F.substring(F.col("value"), 9, 3)
)

demo_data = demo_data.withColumn(
    "registry",
    F.substring(F.col("value"), 12, 2)
)

demo_data = demo_data.withColumn(
    "race",
    F.substring(F.col("value"), 14,1)
)

demo_data = demo_data.withColumn(
    "origin",
    F.substring(F.col("value"), 15, 1)
)

demo_data = demo_data.withColumn(
    "sex",
    F.substring(F.col("value"), 16, 1)
)

demo_data = demo_data.withColumn(
    "age",
    F.substring(F.col("value"), 17, 2)
)

demo_data = demo_data.withColumn(
    "population",
    F.substring(F.col("value"), 19, 8)
)

demo_data = demo_data.withColumn(
    "fips", 
    F.concat(F.col("state_fip"), F.col("county_fip"))
)

demo_data = demo_data.withColumn(
    "population",
    demo_data.population.cast(IntegerType())
)

demo_data = demo_data.withColumn(
    "year",
    demo_data.year.cast(IntegerType())
)

demo_data = demo_data.withColumn(
    "fips",
    demo_data.fips.cast(IntegerType())
) 

### drop redundant info, aggregate registries, rename

demo_data = demo_data.drop('value')
demo_data = demo_data.drop('state_fip')
demo_data = demo_data.drop('county_fip')
demo_data = demo_data.drop('state')

demo_data = demo_data.groupBy('year','race','origin','sex','age','fips').sum('population')

demo_data = demo_data.withColumnRenamed(
    "sum(population)", 
    "population"
)

### filter population data by race

dw_data = demo_data.filter(demo_data.race == 1)
db_data = demo_data.filter(demo_data.race == 2)
do_data = demo_data.filter(demo_data.race == 3)

dw_data = dw_data.groupBy('year','fips').sum('population')
db_data = db_data.groupBy('year','fips').sum('population')
do_data = do_data.groupBy('year','fips').sum('population')

dw_data = dw_data.withColumnRenamed(
    "sum(population)", 
    "w_population"
)

db_data = db_data.withColumnRenamed(
    "sum(population)", 
    "b_population"
)

do_data = do_data.withColumnRenamed(
    "sum(population)", 
    "o_population"
)

### filter population data by origin

dnh_data = demo_data.filter(demo_data.origin == 0)
dhi_data = demo_data.filter(demo_data.origin == 1)
dna_data = demo_data.filter(demo_data.origin == 9)

dnh_data = dnh_data.groupBy('year','fips').sum('population')
dhi_data = dhi_data.groupBy('year','fips').sum('population')
dna_data = dna_data.groupBy('year','fips').sum('population')

dnh_data = dnh_data.withColumnRenamed(
    "sum(population)", 
    "nh_population"
)

dhi_data = dhi_data.withColumnRenamed(
    "sum(population)", 
    "hi_population"
)

dna_data = dna_data.withColumnRenamed(
    "sum(population)", 
    "na_population"
)

### filter population data by sex

dm_data = demo_data.filter(demo_data.sex == 1)
df_data = demo_data.filter(demo_data.sex == 2)

dm_data = dm_data.groupBy('year','fips').sum('population')
df_data = df_data.groupBy('year','fips').sum('population')

dm_data = dm_data.withColumnRenamed(
    "sum(population)", 
    "male_population"
)

df_data = df_data.withColumnRenamed(
    "sum(population)", 
    "female_population"
)

### filter population data by age (19 groups)

age_dic = {}
for i in range(19):
    age_dic["d{0}_data".format(i)] = demo_data.filter(
        demo_data.age == i
    ).groupBy(
        'year',
        'fips'
    ).sum(
        'population'
    ).withColumnRenamed(
        "sum(population)", 
        "age{0}_population".format(i)
    )


### aggregate and outer join the whole thing
    
demo_data = demo_data.groupBy('year','fips').sum('population')

demo_data = demo_data.withColumnRenamed(
    "sum(population)", 
    "population"
)

demo_data = demo_data.join(dw_data, on=['year', 'fips'], how='full')
demo_data = demo_data.join(db_data, on=['year', 'fips'], how='full')
demo_data = demo_data.join(do_data, on=['year', 'fips'], how='full')

demo_data = demo_data.join(dnh_data, on=['year', 'fips'], how='full')
demo_data = demo_data.join(dhi_data, on=['year', 'fips'], how='full')
demo_data = demo_data.join(dna_data, on=['year', 'fips'], how='full')

demo_data = demo_data.join(dm_data, on=['year', 'fips'], how='full')
demo_data = demo_data.join(df_data, on=['year', 'fips'], how='full')

for i in range(19):
    demo_data = demo_data.join(age_dic["d{0}_data".format(i)], on=['year', 'fips'], how='full')


### correct for the nulls, obtain the ratios
    
demo_data = demo_data.na.fill(0)

demo_data = demo_data.withColumn(
    "w_population_ratio",
    F.round(F.col("w_population")/F.col("population"),5)
)

demo_data = demo_data.withColumn(
    "b_population_ratio",
    F.round(F.col("b_population")/F.col("population"),5)
)

demo_data = demo_data.withColumn(
    "o_population_ratio",
    F.round(F.col("o_population")/F.col("population"),5)
)
    
demo_data = demo_data.withColumn(
    "nh_population_ratio",
    F.round(F.col("nh_population")/F.col("population"),5)
)

demo_data = demo_data.withColumn(
    "hi_population_ratio",
    F.round(F.col("hi_population")/F.col("population"),5)
)

demo_data = demo_data.withColumn(
    "na_population_ratio",
    F.round(F.col("na_population")/F.col("population"),5)
)

demo_data = demo_data.withColumn(
    "male_population_ratio",
    F.round(F.col("male_population")/F.col("population"),5)
)

demo_data = demo_data.withColumn(
    "female_population_ratio",
    F.round(F.col("female_population")/F.col("population"),5)
)

for i in range(19):
    demo_data = demo_data.withColumn(
    "age{0}_population_ratio".format(i),
    F.round(F.col("age{0}_population".format(i))/F.col("population"),5)
    )

### save the dataset

demo_data.toPandas().to_csv(r'./data/demographics/county_demographics.csv',sep=',',index=False)

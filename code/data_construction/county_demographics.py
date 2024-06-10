from pyspark.sql import SparkSession
from pyspark.sql import functions as F
from pyspark.sql.types import IntegerType
import os

'''
returns county_demographics.csv

'''

# Build session 
os.environ["JAVA_HOME"] = 'C:\Program Files\Java\jre-1.8'
spark = SparkSession.builder.appName('demographics').getOrCreate()

# Read the data
demo_data = spark.read.text(r'./data/source/demographics/us_1990_2020_19ages_adjusted.txt')

# Extract columns from value column
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

# Obtain FIPS code (unique identifier for each county)
demo_data = demo_data.withColumn(
    "fips", 
    F.concat(F.col("state_fip"), F.col("county_fip"))
    )

# Convert data types
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

# Drop columns
demo_data = demo_data.drop('value')
demo_data = demo_data.drop('county_fip')
demo_data = demo_data.drop('state')

# Split data into county and state data
demo_county = demo_data.drop('state_fip')
demo_states = demo_data.drop('fips')

# Aggregate registries
demo_data_county = demo_county.groupBy('year','race','origin','sex','age','fips').sum('population')
demo_data_states = demo_states.groupBy('year','race','origin','sex','age','state_fip').sum('population')

# Rename columns
demo_county = demo_county.withColumnRenamed(
    "sum(population)", 
    "population"
    )

demo_states = demo_states.withColumnRenamed(
    "sum(population)", 
    "population"
    )

# Filter population data by race
race_columns = ['w_population', 'b_population', 'o_population']
race_values = [1, 2, 3]

race_data_counties = []
race_data_states = []
for column, value in zip(race_columns, race_values):

    race_data_counties.append(demo_county.filter(demo_county.race == value)
                              .groupBy('year', 'fips')
                              .sum('population')
                              .withColumnRenamed("sum(population)", column)
                              )
    
    race_data_states.append(demo_states.filter(demo_states.race == value)
                            .groupBy('year', 'state_fip')
                            .sum('population')
                            .withColumnRenamed("sum(population)", column)
                            )

# Filter population data by origin
origin_columns = ['nh_population', 'hi_population', 'na_population']
origin_values = [0, 1, 9]

origin_data_counties = []
origin_data_states = []
for column, value in zip(origin_columns, origin_values):

    origin_data_counties.append(demo_county.filter(demo_county.origin == value)
                                .groupBy('year', 'fips')
                                .sum('population')
                                .withColumnRenamed("sum(population)", column)
                                )
    
    origin_data_states.append(demo_states.filter(demo_states.origin == value)
                              .groupBy('year', 'state_fip')
                              .sum('population')
                              .withColumnRenamed("sum(population)", column)
                              )

# Filter population data by sex
sex_columns = ['male_population', 'female_population']
sex_values = [1, 2]

sex_data_counties = []
sex_data_states = []
for column, value in zip(sex_columns, sex_values):

    sex_data_counties.append(demo_county.filter(demo_county.sex == value)
                             .groupBy('year', 'fips')
                             .sum('population')
                             .withColumnRenamed("sum(population)", column)
                             )
    
    sex_data_states.append(demo_states.filter(demo_states.sex == value)
                           .groupBy('year', 'state_fip')
                           .sum('population')
                           .withColumnRenamed("sum(population)", column)
                           )

# Filter population data by age (19 groups)
age_data_counties = []
age_data_states = []
for i in range(19):

    age_data_counties.append(demo_county.filter(demo_county.age == i)
                            .groupBy('year', 'fips')
                            .sum('population')
                            .withColumnRenamed("sum(population)", f"age{i}_population")
                            )
    
    age_data_states.append(demo_states.filter(demo_states.age == i)
                           .groupBy('year', 'state_fip')
                           .sum('population')
                           .withColumnRenamed("sum(population)", f"age{i}_population")
                           )

# Aggregate everything at the year-fip and year-state level
demo_county = demo_county.groupBy('year', 'fips').sum('population').withColumnRenamed("sum(population)", "population")
demo_states = demo_states.groupBy('year', 'state_fip').sum('population').withColumnRenamed("sum(population)", "population")

# Outer join everything
for data in race_data_counties + origin_data_counties + sex_data_counties + age_data_counties:
    demo_county = demo_county.join(data, on=['year', 'fips'], how='full')

for data in race_data_states + origin_data_states + sex_data_states + age_data_states:
    demo_states = demo_states.join(data, on=['year', 'state_fip'], how='full')

# Correct the null values
demo_county = demo_county.na.fill(0)
demo_states = demo_states.na.fill(0)

# Obtain the ratios
ratio_columns = ['w_population_ratio', 'b_population_ratio', 'o_population_ratio',
                 'nh_population_ratio', 'hi_population_ratio', 'na_population_ratio',
                 'male_population_ratio', 'female_population_ratio']

for i in range(19):
    ratio_columns = ratio_columns + [f"age{i}_population_ratio"]

for column in ratio_columns:
    numerator = column.rsplit('_', 1)[0]
    denominator = 'population'
    demo_county = demo_county.withColumn(column, F.round(F.col(numerator) / F.col(denominator), 5))
    demo_states = demo_states.withColumn(column, F.round(F.col(numerator) / F.col(denominator), 5))

# Save the data
demo_states.toPandas().to_csv(r'./data/intermediate/demographics/county_demographics_states.csv',sep=',',index=False)
demo_county.toPandas().to_csv(r'./data/intermediate/demographics/county_demographics.csv',sep=',',index=False)
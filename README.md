# The Opioid Crisis: State Regulations and Labor Market Outcomes

### Master's Thesis by Guillermo Martínez

### Advisor: Tom Zohar

### CEMFI

### Year 23/24

**NOTES**: 

This replication package includes two *main* files.
Most of the data construction is done using Python, the 
statistical analysis is done using R.

First, you must run the *main.py* file. 

This script calls the files 
*labor_market_outcomes.py*, *minwage_cleaning.py*, *wages_sector.py*
and *minwage_bindingness.py*.

These are not the only Python scripts involved in data construction.
Nonetheless, the scripts *county_demographics.py*, *efsy_cbp_merging.py*
and *sector_shares.py* use PySpark to accelerate the process. This
environment requires to have Java 1.8 and Spark installed in your
machine to run, and therefore are not called from *main.py* -- although
you could after installing the software. *minwage.py* webscraps the raw data for the minimum wage and is not called either, since changes in the 
original webpage can break the code.

Then you must run *main.R*. Plots might not be displayed, but they'll be
saved in the folder 'slides', where the paper is stored. 
Each folder in ./data/source/ include a README.txt file with links to 
the sources. 

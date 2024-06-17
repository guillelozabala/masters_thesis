import subprocess
import sys
import os
import subprocess
import glob
import re

# Function to install a package using pip
def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# Install the 'pandas' package
install('pandas')
import pandas as pd

# Get the current working directory
folder_path = os.getcwd()

# Define file paths for different Python scripts
file_path_lmo = os.path.join(folder_path, 'code\\data_construction\\', 'labor_market_outcomes.py')
file_path_mwc = os.path.join(folder_path, 'code\\data_construction\\', 'minwage_cleaning.py')
file_path_wsec = os.path.join(folder_path, 'code\\data_construction\\', 'wages_sector.py')
file_path_mwb = os.path.join(folder_path, 'code\\data_construction\\', 'minwage_bindingness.py')

# Run the 'labor_market_outcomes.py' script using the 'python' command
subprocess.run(['python', file_path_lmo])

# Run the 'minwage_cleaning.py' script using the 'python' command
subprocess.run(['python', file_path_mwc])

# Run the 'wages_sector.py' script using the 'python' command
subprocess.run(['python', file_path_wsec])

# Run the 'wages_sector.py' script using the 'python' command (this line seems to be a duplicate)
subprocess.run(['python', file_path_wsec])
import glob

txt_files = glob.glob(r'./data/drugs/*.txt')
#txt_files.pop(txt_files.index('combined_data_three_naics.csv'))

for file in txt_files:
    with open(file, 'r') as f:
        # Do something with the file
        # For example, print its contents
        print(f.read())
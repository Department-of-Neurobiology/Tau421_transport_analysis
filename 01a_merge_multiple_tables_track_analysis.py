import pandas as pd
import glob
import os

tag_postfix = "condition"  # Comment out this string if you don't want a postfix

# Get the directory of the current file and set it as the working directory
# dir_path = os.path.dirname(os.path.realpath(__file__)) 
dir_path = r"windowspath\to\your\directory"

# Change the current working directory to the script directory
os.chdir(dir_path)

# Collect all filenames in the working directory that have an extension of .xls or .xlsx
all_filenames = [i for i in glob.glob('*.xls*')] 

# Read each excel file into a pandas DataFrame and store them in a list
# Skip the first row of each file assuming it's not part of the header
df_list = [pd.read_excel(f, skiprows=1) for f in all_filenames] 

# Start with the first DataFrame in the list
df = df_list[0]

# Define a list of regex patterns for column names to be dropped
columns_to_drop = ['Collection', 'ReferenceFrame', 'Unit']

# Drop the unwanted columns from the first dataframe
for pattern in columns_to_drop:
    df.drop(list(df.filter(regex = pattern)), axis = 1, inplace = True)

# Iterate over the rest of the DataFrames in the list
for df_ in df_list[1:]:
    # Drop the unwanted columns from the current DataFrame
    for pattern in columns_to_drop:
        df_.drop(list(df_.filter(regex = pattern)), axis = 1, inplace = True)

    # Merge the current DataFrame with the main DataFrame
    # This is done based on a common set of column names
    df = df.merge(df_, on=['Original ID','Category','ID','Surpass Object','Original Component ID','Original Component Name','Surpass Object Data ID','Original Image Name','Original Image ID'])

# Sort the columns of the DataFrame in alphabetical order
df = df.reindex(sorted(df.columns), axis=1)

# Sort the DataFrame based on the 'ID' column in ascending order
df=df.sort_values(by=["ID"], ascending=True)

# Attempt to use tag_postfix if it's defined
try:
    filename = "merged_table_" + tag_postfix + ".csv" if tag_postfix else "merged_table.csv"
except NameError:
    # If tag_postfix is not defined, use the default filename
    filename = "merged_table.csv"

# Export the DataFrame to a CSV file
df.to_csv(filename, sep=';', index=False, header=True)

import time
start = time.time()

import numpy as np
import pandas as pd
import glob
pd.set_option('display.expand_frame_repr', False)
import re
import os
dir_path = os.path.dirname(os.path.realpath(__file__))

extension = 'xls'
all_filenames = [i for i in glob.glob('*.{}'.format(extension))]
df_list = [pd.read_excel(f,sep=";", skiprows=1) for f in all_filenames]
df = df_list[0]
#https://stackoverflow.com/questions/23668427/pandas-three-way-joining-multiple-dataframes-on-columns
for df_ in df_list[1:]:
    df = df.merge(df_, on=['Original ID','Category','ID','Surpass Object','Original Component ID','Original Component Name','Surpass Object','Surpass Object Data ID','Original Image Name','Original Image ID'])

df.drop(list(df.filter(regex = 'Collection')), axis = 1, inplace = True)
df.drop(list(df.filter(regex = 'ReferenceFrame')), axis = 1, inplace = True)
df.drop(list(df.filter(regex = 'Unit')), axis = 1, inplace = True)

df = df.reindex(sorted(df.columns), axis=1)
df=df.sort_values(by=["ID"], ascending=True)
print(df)

#another way to choose columns of interest
#df=df[['ID','Original ID','Track Displacement Length Reference Frame']]

df.to_csv("merged_table.csv", sep=';', index=False, header=True)

end = time.time()
print(end - start)


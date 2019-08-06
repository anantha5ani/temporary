# -*- coding: utf-8 -*-
"""
Created on Tue Aug  6 11:10:13 2019

@author: Rajpurohit Anantha
"""
"""
This code is not tested

Code to get basic quality information regarding a dataframe
"""


import os
import pandas as pd
import xlwt 
from xlwt import Workbook
import numpy as np 

os.chdir(r'C:\Users\rajpurohit anantha\Documents\Cases\05. VIL\01. Problem Design\Sample Data\PREPOST_DATA_KPI_Vodafone')
data = pd.read_csv('ARPU_APR19_PRE_POST_VODA.csv', sep = '^')
data.name = 'arpu_data'

# Function to calculate missing values by column
def missing_values_table(df):
        # Total missing values
        mis_val = df.isnull().sum()
        
        # Percentage of missing values
        mis_val_percent = 100 * df.isnull().sum() / len(df)
        
        # Make a table with the results
        mis_val_table = pd.concat([mis_val, mis_val_percent], axis=1)
        
        # Rename the columns
        mis_val_table_ren_columns = mis_val_table.rename(
        columns = {0 : 'Missing Values', 1 : '% of Total Values'})
        
        # Sort the table by percentage of missing descending
        mis_val_table_ren_columns = mis_val_table_ren_columns[
            mis_val_table_ren_columns.iloc[:,1] != 0].sort_values(
        '% of Total Values', ascending=False).round(1)
        
        # Print some summary information
        print ("Your selected dataframe has " + str(df.shape[1]) + " columns.\n"      
            "There are " + str(mis_val_table_ren_columns.shape[0]) +
              " columns that have missing values.")
        
        # Return the dataframe with missing information
        return mis_val_table_ren_columns

def basic_stats(data):
    print(data.head())
    print(data.info())
    numeric_summary = data.describe(percentiles=np.linspace(0,1,11)).T
    categorical_summary = data.describe(include = "O").T
    missing_summary = missing_values_table(data)
    # Writing to an excel  
    # sheet using Python 
    writer = pd.ExcelWriter(data.name+'basic_stats.xlsx', engine='xlsxwriter')
    numeric_summary.to_excel(writer, sheet_name = 'numeric summary')
    categorical_summary.to_excel(writer, sheet_name = 'categorical summary')
    missing_summary.to_excel(writer, sheet_name = 'missing data summary')
    writer.save()

basic_stats(data)

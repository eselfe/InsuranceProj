
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os, sys
import re
import math
import csv
from scipy import stats
from array import *


data = pd.read_csv(r"C:\Users\Silas\Documents\ADEC7320\Data\insurance_training_data.csv")


# ----------------------------------------------- Numericizing data

money = data[['INCOME', 'HOME_VAL', 'BLUEBOOK', 'OLDCLAIM']].copy()

r = 1
c = 1
for c in range(len(money.columns)):
  col = money.iloc[:,c]  
  for r in range(len(col)):
      inspect = col[r]
      
      if isinstance(inspect, str):
        rem1 = inspect.replace("$", "")
        rem2 = rem1.replace(",", "")
        final = int(float(rem2))
        money.loc[r,c] = final
      else:
        pass


data["INCOME"] = money[0].values
data["HOME_VAL"] = money[1].values
data["BLUEBOOK"] = money[2].values
data["OLDCLAIM"] = money[3].values

# ---


# ----------------------------------------------- Categorical Categories
carType = pd.unique(data.CAR_TYPE)
location = pd.unique(data.URBANICITY)
jobType = pd.unique(data.JOB)
eduLevel = pd.unique(data.EDUCATION)

carType_Info = data.groupby("CAR_TYPE").mean()

print(carType)
print(location)
print(jobType)
print(eduLevel)

# 

# ----------------------------------------------- Fill NAs
data.isna().sum()


data.AGE.fillna(data.AGE[data.HOMEKIDS == 2].mean(), inplace = True)
data.AGE.fillna(data.AGE[data.HOMEKIDS == 3].mean(), inplace = True)
data.AGE.fillna(data.AGE[data.HOMEKIDS == 0].mean(), inplace = True)

data.CAR_AGE.fillna(data.CAR_AGE.mean(), inplace = True)
data.HOME_VAL.fillna(data.HOME_VAL.mean(), inplace = True)
data.YOJ.fillna(data.YOJ.mean(), inplace = True)
data.INCOME.fillna(data.INCOME.median(), inplace = True)
data.JOB.fillna("nan", inplace = True)

#


categorical = data[['PARENT1', 'MSTATUS', 'SEX', 'EDUCATION', 'JOB', 'CAR_USE', 'RED_CAR', 'REVOKED', 'URBANICITY']].copy()
# Convert Categories to Numbers
Num_data = data.copy()


Num_data.loc[Num_data['PARENT1'] == 'No', 'PARENT1'] = 0
Num_data.loc[Num_data['PARENT1'] == 'Yes', 'PARENT1'] = 1

Num_data.loc[Num_data['MSTATUS'] == 'z_No', 'MSTATUS'] = 0
Num_data.loc[Num_data['MSTATUS'] == 'Yes', 'MSTATUS'] = 1

Num_data.loc[Num_data['SEX'] == 'z_F', 'SEX'] = 0
Num_data.loc[Num_data['SEX'] == 'M', 'SEX'] = 1

Num_data.loc[Num_data['EDUCATION'] == '<High School', 'EDUCATION'] = 0
Num_data.loc[Num_data['EDUCATION'] == 'z_High School', 'EDUCATION'] = 1
Num_data.loc[Num_data['EDUCATION'] == 'Bachelors', 'EDUCATION'] = 2
Num_data.loc[Num_data['EDUCATION'] == 'Masters', 'EDUCATION'] = 3
Num_data.loc[Num_data['EDUCATION'] == 'PhD', 'EDUCATION'] = 4

Num_data.loc[Num_data['JOB'] == 'Student', 'JOB'] = 0
Num_data.loc[Num_data['JOB'] == 'Clerical', 'JOB'] = 1
Num_data.loc[Num_data['JOB'] == 'z_Blue Collar', 'JOB'] = 2
Num_data.loc[Num_data['JOB'] == 'Home Maker', 'JOB'] = 3
Num_data.loc[Num_data['JOB'] == 'Manager', 'JOB'] = 4
Num_data.loc[Num_data['JOB'] == 'Professional', 'JOB'] = 5
Num_data.loc[Num_data['JOB'] == 'Lawyer', 'JOB'] = 6
Num_data.loc[Num_data['JOB'] == 'Doctor', 'JOB'] = 7
Num_data.loc[Num_data['JOB'] == 'nan', 'JOB'] = 8

Num_data.loc[Num_data['CAR_USE'] == 'Private', 'CAR_USE'] = 0
Num_data.loc[Num_data['CAR_USE'] == 'Commercial', 'CAR_USE'] = 1

Num_data.loc[Num_data['CAR_TYPE'] == 'Minivan', 'CAR_TYPE'] = 0
Num_data.loc[Num_data['CAR_TYPE'] == 'z_SUV', 'CAR_TYPE'] = 1
Num_data.loc[Num_data['CAR_TYPE'] == 'Sports Car', 'CAR_TYPE'] = 2
Num_data.loc[Num_data['CAR_TYPE'] == 'Van', 'CAR_TYPE'] = 3
Num_data.loc[Num_data['CAR_TYPE'] == 'Panel Truck', 'CAR_TYPE'] = 4
Num_data.loc[Num_data['CAR_TYPE'] == 'Pickup', 'CAR_TYPE'] = 5

Num_data.loc[Num_data['RED_CAR'] == 'no', 'RED_CAR'] = 0
Num_data.loc[Num_data['RED_CAR'] == 'yes', 'RED_CAR'] = 1

Num_data.loc[Num_data['REVOKED'] == 'No', 'REVOKED'] = 0
Num_data.loc[Num_data['REVOKED'] == 'Yes', 'REVOKED'] = 1

Num_data.loc[Num_data['URBANICITY'] == 'Highly Urban/ Urban', 'URBANICITY'] = 0
Num_data.loc[Num_data['URBANICITY'] == 'z_Highly Rural/ Rural', 'URBANICITY'] = 1




# ----------------------------------------------- Categorizing by Age
ageInfo = data.groupby("AGE").mean()
ageInfo_det = ageInfo.index.values

# Loop counts observations by age
i = 0
for i in range(len(ageInfo)):
  tempAge = int(float(ageInfo_det[i]))
  amt = sum(data.AGE == tempAge)
  ageInfo.loc[tempAge,15] = amt

ageInfo.rename(columns={15:'n'}, inplace = True)

# 




# ----------------------------------------------- Categorizing by Ownership
privOwn = data[data.CAR_USE == "Private"]
commOwn = data[data.CAR_USE == "Commercial"]

prob_privOwn_1 = sum(privOwn.TARGET_FLAG == 1) / len(privOwn)
prob_commOwn_1 = sum(commOwn.TARGET_FLAG == 1) / len(commOwn)



# -------------------------------------
# -------------------------------------
# -------------------------------------
# -------------------------------------
# -------------------------------------


# Checking nas
data.isnull().values.any()
data.isnull().sum().sum()
data.KIDSDRIV.isnull().sum().sum()
data.AGE.isnull().sum().sum()
data.HOMEKIDS.isnull().sum().sum()
data.JOB.isnull().sum().sum()





# ---------------------------------------------- CRASH
data_1 = data[data.TARGET_FLAG == 1]
data_0 = data[data.TARGET_FLAG == 0]

data_1.mean()
data_0.mean()






exit()





























r = 1
c = 1
for c in range(len(categorical.columns)):
  col = categorical.iloc[:,0]  
  for r in range(len(col)):
      inspect = col[r]
      
      if isinstance(inspect, str):
        rem1 = inspect.replace("$", "")
        rem2 = rem1.replace(",", "")
        final = int(float(rem2))
        money.loc[r,c] = final
      else:
        pass







r = 1  # MOST PROMISING 
c = 1
for c in range(len(categorical.columns)):
  col = categorical.iloc[:,0]  
  first = col[0]
  for r in range(len(col)):
      inspect = col[r]
      options = len(pd.unique(col))

      if options == 2 & inspect != first:
        num = 1
      elif options == 5 & input != first:
        remem.apend(input)
        num = len(remem)
      else:
        num = 0


remem = []
if options == 5 & input != first:
  remem.apend(input)
  num = len(remem)
else:
  num = 0




      elif options == 6:
        
      elif options == 8:











sum(col)
len(col)
len(pd.unique(col))

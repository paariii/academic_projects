import numpy as np
import pandas as pd
import sklearn as sk
from sklearn.cluster import DBSCAN
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
import matplotlib.pyplot as plt
from sklearn.metrics import mean_absolute_error
from sklearn.linear_model import RidgeCV
from sklearn.linear_model import Ridge
from sklearn.linear_model import LassoCV
from sklearn.linear_model import Lasso
from sklearn.experimental import enable_iterative_imputer

########################################################import dataset##################################################
#import train data
df = pd.read_csv('E:/Desktop/term 8/data mining/Project/Train Data.csv')
#import test data
df1 = pd.read_csv('E:/Desktop/term 8/data mining/Project/Test Data.csv')

########################################################describe dataset################################################
head=df.head()
describe=df.describe()
shape=df.shape
columns=df.columns

##########################################################preprocessing#################################################
#gussing Y distribution
plt.hist(df['SalePrice'],bins=18,color='purple')
plt.xlabel('SalePrice')
plt.ylabel('frquency')
plt.title('distribution of SalePrice')
plt.show()
plt.hist(np.log(df['SalePrice']),bins=18,color='orange')
plt.xlabel('ln(SalePrice)')
plt.ylabel('frequency')
plt.title('distribution of ln(SalePrice)')
plt.show()

#replace empty cells which are not missing values
for i in ['Alley', 'GarageType', 'GarageYrBlt', 'GarageFinish', 'GarageQual', 'GarageCond', 'Fence', 'MiscFeature',
       'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'FireplaceQu', 'PoolQC']:
  df[i] = df[i].replace(np.nan, 0)
  df1[i] = df1[i].replace(np.nan, 0)

#replace ordered strings with numbers
replace_map = {
              'BsmtFinType1': {'Unf': 1, 'LwQ': 2, 'Rec': 3, 'BLQ': 4, 'ALQ': 5, 'GLQ': 6},
              'BsmtFinType2': {'Unf': 1, 'LwQ': 2, 'Rec': 3, 'BLQ': 4, 'ALQ': 5, 'GLQ': 6},
              'BsmtQual':     {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'BsmtCond':     {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'FireplaceQu':  {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'GarageCond':   {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'KitchenQual':  {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'ExterQual':    {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'ExterCond':    {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'HeatingQC':    {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'GarageQual':   {'Po': 1, 'Fa': 2, 'TA': 3, 'Gd': 4, 'Ex': 5},
              'Utilities':    {'ELO': 1, 'NoSeWa': 2, 'NoSewr': 3, 'AllPub': 4},
              'BsmtExposure': {'No': 1, 'Mn': 2, 'Av': 3, 'Gd': 4},
              'PoolQC':       {'Fa': 1,'TA': 2, 'Gd': 3, 'Ex': 4},
              'LandSlope':    {'Sev':1,'Mod':2,'Gtl':3},
              'PavedDrive':   {'N':1,'P':2,'Y':3},
              'CentralAir':   {'N':0,'Y':1}
              }

df.replace(replace_map, inplace=True)
df1.replace(replace_map, inplace=True)

#merging two data sets (because their columns are different)
total_data = df.drop(['SalePrice'], axis=1)
total_data = total_data.append(df1)
#feature engineering
total_data = pd.get_dummies(total_data)

#separate train and test data with same columns
size = df.shape[0]
col_index = df.shape[1]+1
Df_out = total_data.iloc[:size,:]
Df_out.insert(len(Df_out.columns),'SalePrice',df['SalePrice'],True)
test_df = total_data.iloc[size:,:]

#fill in missing values in train and test data
MeanImputer = sk.impute.SimpleImputer(missing_values=np.nan, strategy='mean')

DfMean = Df_out.copy(deep=True)
DfMean.iloc[:, :] = MeanImputer.fit_transform(DfMean)

DfMean2 = test_df.copy(deep=True)
DfMean2.iloc[:, :] = MeanImputer.fit_transform(DfMean2)

##########################################################outlier detection#############################################
#find best parameters for DBSCAN
"""
L = []
for i in np.arange(1040, 1055, 0.2) :
 for j in range(3,6) :
     clustering = DBSCAN(eps= i , min_samples= j).fit(Df_out)
     predict = clustering.labels_
     outliers_num=list(predict).count(-1)
     L.append([i,j,outliers_num])

L = pd.DataFrame(L).sort_values(by=2, ascending=True)
"""
#DBSCAN algorithm
clustering = DBSCAN(eps = 1045, min_samples=3).fit(DfMean)
predict = clustering.labels_
Df_out = DfMean[~np.where(predict == -1, True, False)]
#Final X and Y for making models
X_out = Df_out.drop(['SalePrice'], axis = 1)
Y_out = np.log(Df_out['SalePrice'])

#final test data
X_final_test = DfMean2

###########################################################Models : Ridge & Lasso#######################################
MAEridge=[]
MAElasso = []
R2_ridge=[]
R2_lasso = []

for i in range(0,20):
  X1_train, X1_test, Y1_train, Y1_test = train_test_split(X_out, Y_out, test_size=0.2)

  #using CV for finding best parameters
  ridgecv = RidgeCV(cv=10, normalize=False)
  ridgecv.fit(X1_train, Y1_train)

  lassocv = LassoCV(cv=10, normalize=False)
  lassocv.fit(X1_train, Y1_train)

  reg1 = Ridge(alpha=ridgecv.alpha_, normalize=False)
  reg1.fit(X1_train, Y1_train)
  Y1_predict = reg1.predict(X1_test)
  R2_1 = r2_score(Y1_test,Y1_predict)
  R2_ridge.append(R2_1)
  MAEridge.append(mean_absolute_error(np.exp(Y1_test), np.exp(Y1_predict)))

  reg2 = Lasso(alpha=lassocv.alpha_, normalize=False)
  reg2.fit(X1_train, Y1_train)
  Y2_predict = reg2.predict(X1_test)
  R2_2 = r2_score(Y1_test, Y2_predict)
  R2_lasso.append(R2_2)
  MAElasso.append(mean_absolute_error(np.exp(Y1_test), np.exp(Y2_predict)))

table = {
  'Lasso' : {
      'MAE': np.mean(MAElasso),
      'R2': np.mean(R2_lasso)
  },
  'Ridge' : {
      'MAE' : np.mean(MAEridge),
      'R2' : np.mean(R2_ridge)
  }
}
print(pd.DataFrame(table))

##################################################prediction by best model for test data################################
#prediction by ridge
Y_final_predict = reg1.predict(X_final_test)
Y_final = pd.DataFrame(np.exp(Y_final_predict))
#exporting to an excel file
final_excel_df = pd.read_csv('E:/Desktop/term 8/data mining/Project/Test Data.csv')
final_excel_df['Price_prediction'] = Y_final
final_excel_df.to_csv('E:/Desktop/term 8/data mining/Project/Test_Data.csv', index=False)

##########################################################Correlation###################################################
#finding the most important parameter for predicting sale price
correlation = Df_out.corr()['SalePrice']
correlation = pd.DataFrame(correlation)
correlation = correlation.sort_values(by='SalePrice',ascending=False)


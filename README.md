# Heart-Disease-Prediction

R | Logistic Model | CDC Data
-

Overview
-
  - Used R for the entire project
  - Cleaned a huge dataset and filtered out unnecessary null values and meaningless numbers
  - Visualized data by donut charts, pie charts, histograms and bar charts using ggplot2 and lessR
  - Identified key indicators of heart disease such as gender and age
  - Logistic Model was applied to predict heart disease and reached an accuracy of 88%

Packages used in R
-
  - foreign, dplyr, ggplot2, lessR, scales, tidyverse

Data source
-
  - CDC - 2020 BRFSS Survey Data and Documentation
  - https://www.cdc.gov/brfss/annual_data/annual_2020.html

Incentives
-
  - Find out key indicators of having heart disease
  - Heart disease prediction
  
Data cleaning
-
  - Select records from completed surveys
  - Keep 29 columns after careful selection according to CDC dataset codebook
  - Change column names for better readability
  - Remove columns containing NAs of more than a half
  - Replace “88” in Fall with 0 for more accurate calculation result (“88” represents “None”: didn’t fall in the past 12 years)
  - Delete “77”/”99” in SleepTime & Fall (“77”/”99” represent answers of “Not sure”/”Refused(to answer)”)
  - Remove NAs in HeartDisease & SleepTime & Marital & Fall & BMI
  - Convert certain variables to factors and add labels

Visualizations
- 
![image](https://user-images.githubusercontent.com/120230351/210030327-3056964e-ce02-47df-ad59-669a702eb94a.png)
![image](https://user-images.githubusercontent.com/120230351/210030558-d4c9216c-0fa5-421c-8563-99794a374581.png)
![image](https://user-images.githubusercontent.com/120230351/210030573-7004284b-7598-4692-902c-e9442f2369c0.png)
![image](https://user-images.githubusercontent.com/120230351/210030586-b86f66e4-9955-4e88-88b1-4f32507cee07.png)
<img width="650" alt="image" src="https://user-images.githubusercontent.com/120230351/210030596-f8a757b6-edcd-4e1e-8ba7-5d96c06ef47e.png">
![image](https://user-images.githubusercontent.com/120230351/210030600-42a47d5c-ccdf-4c7d-ba8d-7fa60a9c5c2c.png)
![image](https://user-images.githubusercontent.com/120230351/210030642-358ea586-5f20-4857-a7aa-7cbfec8c3b94.png)
![image](https://user-images.githubusercontent.com/120230351/210030650-5fcba279-f3eb-4504-846d-c5fb8b9b0919.png)
<img width="286" alt="image" src="https://user-images.githubusercontent.com/120230351/210031091-142fc2a2-f852-4bde-8403-8af8866dce27.png">

Key indicators
-
 - General health
 - Age
 - Gender
 - BMI
 - Exercising frequency
 - Having diabetes
 - Having stroke
 - Having difficulty in walking
 - Number of times of falling in the past 12 months
 - Number of teeth removed because of tooth decay or gum disease
<img width="146" alt="image" src="https://user-images.githubusercontent.com/120230351/210031542-7e4e2234-01c7-4b46-8c63-22b3b7078bef.png">

Prediction
-
Logistic Model
Performance: reached 88% accuracy



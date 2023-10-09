# Introduction
This project uses the **'Life_Expectancy_Data1.csv'** dataset, which includes various World Bank indicator variables. The goal of this project is to perform exploratory data analysis, data preprocessing, and build a linear regression model to analyse the factors affecting life expectancy.

# Dataset Description
The dataset consists of the following worldbank indicator variables:
- SP.DYN.LE00.IN: Life expectancy at birth, total (years)
- EG.ELC.ACCS.ZS: Access to electricity (% of population)
- NY.ADJ.NNTY.KD.ZG: Adjusted net national income (annual % growth)
- NY.ADJ.NNTY.PC.KD.ZG: Adjusted net national income per capita (annual % growth)
- SH.HIV.INCD.14: Children (ages 0-14) newly infected with HIV
- SE.PRM.UNER: Children out of school, primary
- SE.PRM.CUAT.ZS: Educational attainment, at least completed primary, population 25+ years, total (% cumulative)
- SE.TER.CUAT.BA.ZS: Educational attainment, at least Bachelor’s or equivalent, population 25+, total (% cumulative)
- SP.DYN.IMRT.IN: Mortality rate, infant (per 1,000 live births)
- SE.PRM.CMPT.ZS: Primary completion rate, total (% of relevant age group)
- SE.ADT.LITR.ZS: Literacy rate, adult total (% of people ages 15 and above)
- FR.INR.RINR: Real interest rate (%)
- SP.POP.GROW: Population growth (annual %)
- EN.POP.DNST: Population density (people per sq. km of land area)
- SP.POP.TOTL: Population, total
- SH.XPD.CHEX.PP.CD: Current health expenditure per capita, PPP (current international $)
- SH.XPD.CHEX.GD.ZS: Current health expenditure (% of GDP)
- SL.UEM.TOTL.NE.ZS: Unemployment, total (% of total labor force) (national estimate)
- NY.GDP.MKTP.KD.ZG: GDP growth (annual %)
- NY.GDP.PCAP.PP.CD: GDP per capita, PPP (current international $)
- SP.DYN.CBRT.IN: Birth rate, crude (per 1,000 people)
- EG.FEC.RNEW.ZS: Renewable energy consumption (% of total final energy consumption)
- SH.HIV.INCD: Adults (ages 15-49) newly infected with HIV
- SH.H2O.SMDW.ZS: People using safely managed drinking water services (% of population)
- SI.POV.LMIC: Poverty headcount ratio at $3.20 a day (2011 PPP) (% of population)
- SE.COM.DURS: Compulsory education duration (years)

# Project Workflow
## Exploratory Data Analysis: 
We will explore both graphical and numerical representations of the dataset to gain insights into the variables and their relationships.

## Data Preprocessing:
- Deal with Missing Values: We will begin by removing variables that have greater than or equal to 80% of missing values.
- Impute Missing Data: We will use the **Multiple Imputation by Chained Equations (MICE)** method to impute missing values.
- Check for Outliers and Influential Observations.
- Collinearity Check: We will investigate collinearity between predictor variables using the **Variance Inflation Factor (VIF)**.

## Model Building:
Finally, we will fit a **linear regression model** to analyze the relationship between the selected predictor variables and life expectancy.

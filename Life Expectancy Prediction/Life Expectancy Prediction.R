# Install and load the required packages
# install.packages("tidyverse")
# install.packages("funModeling")
# install.packages("ggcorrplot")
library(tidyverse)
library(funModeling)
library(ggcorrplot)
library(dplyr)

# Read the dataset
Data <- read.csv('Life_Expectancy_Data1.csv')

# Analyse data using graphical representations

# Distribution of 6 continents
freq(Data)

# Remove missing values in Life expectancy
b <- na.omit(Data$SP.DYN.LE00.IN)

# Histogram of Life Expectency
hist(b,main="Life expectancy - Histogram",xlab="Life expectancy at birth, total (years)")

# Distribution of data for each variables
plot_num(Data, bins=10)

# Use Correlation Matrix
cor_table <- cor(Data[,4:29],use="pairwise.complete.obs")

# Graphs for some pairs having negative correlation
par(mfrow=c(2,3))
plot(Data$SP.DYN.IMRT.IN[!is.na(Data$SP.DYN.IMRT.IN)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SP.DYN.IMRT.IN)], ylab="Life expectancy", xlab="Mortality rate")

plot(Data$SP.DYN.CBRT.IN[!is.na(Data$SP.DYN.CBRT.IN)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SP.DYN.CBRT.IN)], ylab="Life expectancy", xlab="Birth rate")

plot(Data$SP.POP.GROW[!is.na(Data$SP.POP.GROW)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SP.POP.GROW)], ylab="Life expectancy", xlab="Population growth")

plot(Data$SE.PRM.UNER[!is.na(Data$SE.PRM.UNER)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SE.PRM.UNER)], ylab="Life expectancy", xlab="Children out of school")

plot(Data$SH.HIV.INCD.14[!is.na(Data$SH.HIV.INCD.14)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SH.HIV.INCD.14)], ylab="Life expectancy", xlab="Children newly infected with HIV")

plot(Data$SL.UEM.TOTL.NE.ZS[!is.na(Data$SL.UEM.TOTL.NE.ZS)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SL.UEM.TOTL.NE.ZS)], ylab="Life expectancy", xlab="Unemployment rate")


# Graphs for some pairs having positive correlation
par(mfrow=c(2,3))
plot(Data$SH.H2O.SMDW.ZS[!is.na(Data$SH.H2O.SMDW.ZS)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SH.H2O.SMDW.ZS)], ylab="Life expectancy", xlab="People using safely managed drinking water services")

plot(Data$EG.ELC.ACCS.ZS[!is.na(Data$EG.ELC.ACCS.ZS)],
     Data$SP.DYN.LE00.IN[!is.na(Data$EG.ELC.ACCS.ZS)], ylab="Life expectancy", xlab="Access to electricity")

plot(Data$NY.GDP.PCAP.CD[!is.na(Data$NY.GDP.PCAP.CD)],
     Data$SP.DYN.LE00.IN[!is.na(Data$NY.GDP.PCAP.CD)], ylab="Life expectancy", xlab="GDP per capita")

plot(Data$SH.XPD.CHEX.PC.CD[!is.na(Data$SH.XPD.CHEX.PC.CD)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SH.XPD.CHEX.PC.CD)], ylab="Life expectancy", xlab="Current health expenditure per capita ($)")

plot(Data$SE.PRM.CMPT.ZS[!is.na(Data$SE.PRM.CMPT.ZS)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SE.PRM.CMPT.ZS)], ylab="Life expectancy", xlab="Primary completion rate")

plot(Data$SH.XPD.CHEX.GD.ZS[!is.na(Data$SH.XPD.CHEX.GD.ZS)],
     Data$SP.DYN.LE00.IN[!is.na(Data$SH.XPD.CHEX.GD.ZS)], ylab="Life expectancy", xlab="Current health expenditure (%)")

# Boxplot for Life Expectancy of six continents
# par(mfrow=c(1,1))
# boxplot(Data$SP.DYN.LE00.IN~Data$Continent,main='Comparing life expectancy from different continents',
#         xlab='Continent', col="light gray", ylab = "Life Expectancy (Years)")

# Analyse data using numerical representations

# Understand number of observations, variables, variable datatypes and sample values 
str(Data)

# Understand count of zeroes, NAs, Infinity and unique values for all the variables with their datatypes
df_status(Data)

# Understand Statistics like Mean, Std Dev, Coefficient of Variation, etc. for each variable
profiling_num(Data)
summary(Data)

# Top 5 highest life-expectancy countries
top5 <- Data[order(-Data$SP.DYN.LE00.IN),]
print(top5[1:5,1:4])

# Top 5 lowest life-expectancy countries
top5 <- Data[order(Data$SP.DYN.LE00.IN),]
print(top5[1:5,1:4])

# Check the method deletion missing values with "Complete case" analysis
data_complete_case <- Data[complete.cases(Data),]

# Load ("mice")
library(mice)

# List deleted columns (include 5 columns):
del_col <- colSums(is.na(Data))/217>0.8
colnames(Data[,del_col])

# Delete columns with high N/A more than 80% (>=0.8)
p_data <- Data[,!del_col]

# The variable "SP.POP.TOTL" (Population,total) is a huge number compared to the other variables
# Leadto Error Singularity Matrix.
# Use Log function to deal with it.
p_data["SP.POP.TOTL"] <- data.frame(log(p_data$SP.POP.TOTL))

# Remove temporary Life expectancy before imputation
life_exp <- p_data[,4]
p_data <- p_data[,-4]

# Look at the number of missing data and and their pattern
dim(p_data)
md.pattern(p_data)

# STAGE 1:
# Use imputation to deal with missing value 
# Graph
m_number=10
imp_method <- c("","","","pmm","pmm","pmm","pmm","pmm","pmm","pmm",
                "pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm")

imputations <- mice(p_data, method = imp_method, m=m_number, seed = 123456) 

stripplot(imputations, EG.ELC.ACCS.ZS +
            NY.ADJ.NNTY.KD.ZG +
            NY.ADJ.NNTY.PC.KD.ZG +
            SH.HIV.INCD.14 +
            SE.PRM.UNER +
            SP.DYN.IMRT.IN +
            SE.PRM.CMPT.ZS +
            FR.INR.RINR +
            SP.POP.GROW +
            EN.POP.DNST +
            SP.POP.TOTL +
            SH.XPD.CHEX.PC.CD +
            SH.XPD.CHEX.GD.ZS +
            SL.UEM.TOTL.NE.ZS +
            NY.GDP.MKTP.KD.ZG +
            NY.GDP.PCAP.CD +
            SP.DYN.CBRT.IN +
            SH.HIV.INCD +
            SH.H2O.SMDW.ZS +
            SE.COM.DURS  ~ .imp, pch = 20, cex = 1.2)

complete_data <- complete(imputations)
imputations$imp

complete_data$SP.DYN.LE00.IN <- Data$SP.DYN.LE00.IN

# STAGE 2: ANALYSIS
# Build a linear regression model which will be fit for each imputed dataset.
feature.selection <- expression(null.model1 <- lm(life_exp ~ 1),
                                model2 <- step(null.model1, scope = ~ EG.ELC.ACCS.ZS +
                                                 NY.ADJ.NNTY.KD.ZG +
                                                 NY.ADJ.NNTY.PC.KD.ZG +
                                                 SH.HIV.INCD.14 +
                                                 SE.PRM.UNER +
                                                 SP.DYN.IMRT.IN +
                                                 SE.PRM.CMPT.ZS +
                                                 FR.INR.RINR +
                                                 SP.POP.GROW +
                                                 EN.POP.DNST +
                                                 SP.POP.TOTL +
                                                 SH.XPD.CHEX.PC.CD +
                                                 SH.XPD.CHEX.GD.ZS +
                                                 SL.UEM.TOTL.NE.ZS +
                                                 NY.GDP.MKTP.KD.ZG +
                                                 NY.GDP.PCAP.CD +
                                                 SP.DYN.CBRT.IN +
                                                 SH.HIV.INCD +
                                                 SH.H2O.SMDW.ZS + 
                                                 SE.COM.DURS, action=na.omit
                                ))

step.fit <- with(imputations, feature.selection)

# Choose the variables which appear at least 5 times of the imputed models 
# And select them in the final model
step.fit.models <- lapply(step.fit$analyses, formula)

step.fit.features <- lapply(step.fit.models, terms)

feature.frequency <- unlist(lapply(step.fit.features, labels))

feature.frequency

sort(table(feature.frequency),decreasing=TRUE)

# STAGE 3: POOLING (with the selected variables from STAGE 2)
model.fit <- with(imputations, lm(life_exp ~  EG.ELC.ACCS.ZS +
                                    SH.HIV.INCD +
                                    SP.DYN.CBRT.IN +
                                    SP.DYN.IMRT.IN +
                                    SP.POP.GROW +
                                    SP.POP.TOTL +
                                    NY.GDP.PCAP.CD +
                                    EN.POP.DNST +
                                    SH.H2O.SMDW.ZS +
                                    SH.XPD.CHEX.GD.ZS
))

summary(pool(model.fit))

pool.r.squared(model.fit)

# Remove the rows with NA in SP.DYN.LE00.IN
new_data_comp<-complete_data %>%  na.omit()
summary(new_data_comp)

# Rename the variables
df<-rename(new_data_comp[,4:24], x1 = EG.ELC.ACCS.ZS, x2 = NY.ADJ.NNTY.KD.ZG,x3=NY.ADJ.NNTY.PC.KD.ZG,
           x4=SH.HIV.INCD.14, x5=SE.PRM.UNER, x6=SP.DYN.IMRT.IN,
           x7=SE.PRM.CMPT.ZS, x8= FR.INR.RINR, x9=SP.POP.GROW,
           x10= EN.POP.DNST, x11=SP.POP.TOTL, x12=SH.XPD.CHEX.PC.CD,
           x13=SH.XPD.CHEX.GD.ZS, x14=SL.UEM.TOTL.NE.ZS, x15=NY.GDP.MKTP.KD.ZG,
           x16=NY.GDP.PCAP.CD, x17=SP.DYN.CBRT.IN, x18=SH.HIV.INCD,
           x19=SH.H2O.SMDW.ZS,x20=SE.COM.DURS, y=SP.DYN.LE00.IN)

# Plot of the residuals to test the conditions for the full model
full_models<-lm(y~ .,data=df)
stdres_fullmodel<-rstandard(full_models)
plot(full_models$fitted.values,stdres_fullmodel,pch=16,
     ylab="Standatdized Residuals", xlab="fitted y",
     ylim=c(-3,3), main="full model")
abline(h=0)
abline(h=2,lty=2)
abline(h=-2,lty=2)

# Plots the QQ-plot for full model
qqnorm(stdres_fullmodel, ylab="Standardized Residuals",
       xlab="Normal Scores",main="QQ Plot for the full model")
qqline(stdres_fullmodel)

# The fitted linear regression line
plot(full_models)


# Order them from max to min
hat_matrix[order(-hat_matrix['hatvalues(full_models)']), ]

# test 1(without na rows) and just for the predictors:
install.packages("corrplot")
library(corrplot)
X<-df[,-21]
cor_data_comp.corr<-cor(X)
cor_data_comp.corr
corrplot.mixed(cor_data_comp.corr,lower.col="black",number.cex=.5)
install.packages("faraway")
library("faraway")
vif(X)

# Reduced model1 without X2 and x3:
reducedmodel1<-lm(y~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
                  +x18+x19+x20,data=df)
fullmodel<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
              +x18+x19+x20,data=df)
summary(reducedmodel1)
summary(fullmodel)
anova(reducedmodel1,fullmodel)

# Linear_model is the reduced model1
linear_model<-lm(y~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14
                 +x15+x16+x17+x18+x19+x20,data=df)
linear_model
summary(linear_model)

# Forward feature selection using the variables in the linear model(reducemodel1)
selection1<-lm(y~1,data=df)
step1<-step(selection1,scope=~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
            +x18+x19+x20,
            method='forward')
summary(step1)

# Backward feature selection using the variables in the linear model(reducemodel1)
selection2<-lm(y~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
               +x18+x19+x20,data=df)
step2<-step(selection2,method="backward")
summary(step2)

# Our new model after the selection
reducedmodel2<-lm(y~x1+x4+x6+x7+x9+x11+x14+x16+x17
                  +x18+x19,data=df)
summary(reducedmodel2)
AIC(reducedmodel1)
AIC(reducedmodel2)

# ANOVA test on reducedmodel2 vs model without x11
reducedmodel3<-lm(y~x1+x4+x6+x7+x9+x14+x16+x17
                  +x18+x19,data=df)
reducedmodel2<-lm(y~x1+x4+x6+x7+x9+x11+x14+x16+x17
                  +x18+x19,data=df)
anova(reducedmodel3,reducedmodel2)

summary(reducedmodel3)

# ANOVA test on reducedmodel3 vs model without x19
reducedmodel4<-lm(y~x1+x4+x6+x7+x9+x14+x16+x17
                  +x18,data=df)
reducedmodel3<-lm(y~x1+x4+x6+x7+x9+x14+x16+x17
                  +x18+x19,data=df)
anova(reducedmodel4,reducedmodel3)

summary(reducedmodel4)

model_select1<-lm(y~x1+x4+x6+x7+x9+x14+x16+x17
                  +x18,data=df)
summary(model_select1)

# Plot of the residuals to test the conditions:

stdres_modelselected<-rstandard(model_select1)
plot(model_select1$fitted.values,stdres_modelselected,pch=16,
     ylab="Standatdized Residuals", xlab="fitted y",
     ylim=c(-3,3), main="modelselected")
abline(h=0)
abline(h=2,lty=2)
abline(h=-2,lty=2)

# Plots the QQ-plot for full model
qqnorm(stdres_modelselected, ylab="Standardized Residuals",
       xlab="Normal Scores",main="QQ Plot for modelselected")
qqline(stdres_modelselected)

# The fitted linear regression line
plot(lm(y~x1+x4+x6+x7+x9+x14+x16+x17
        +x18,data=df))

plot(model_select1)


# Predict y without using outliers(deleting the rows with the outliers)
# Outliers for the full model (influencial observations)
hat_matrix <- as.data.frame(hatvalues(full_models))
hat_matrix

# Order them from max to min
hat_matrix[order(-hat_matrix['hatvalues(full_models)']), ]
hat_matrix>=(21/198)*2

# Delete the rows with those influencial observations (with the True result form previous code)
df2 <- df %>% slice(-c(1, 40, 46,52,92,115,117,118,124,
                       135,144,145,178,192,194,207,217))

# Calculate VIF again
X2<-df2[,-21]
cor_data_comp.corr2<-cor(X2)
cor_data_comp.corr2
corrplot.mixed(cor_data_comp.corr2,lower.col="black",number.cex=.5)
vif(X2)

# Reduced model1 without X2 and x3
reducedmodelout1<-lm(y~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
                     +x18+x19+x20,data=df)
fullmodelout<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
                 +x18+x19+x20,data=df)
anova(reducedmodelout1,fullmodelout)

# Regression model(without x1,x2)
linear_modelout<-lm(y~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14
                    +x15+x16+x17+x18+x19+x20,data=df2)

summary(linear_modelout)

# Forward feature selection using the variables in the linear model(reducemodel1)
selection_out1<-lm(y~1,data=df2)
step_out1<-step(selection_out1,scope=~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
                +x18+x19+x20,
                method='forward')
summary(step_out1)

# Backward feature selection using the variables in the linear model(reducemodel1)
selection_out2<-lm(y~x1+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17
                   +x18+x19+x20,data=df2)
step_out2<-step(selection_out2,method="backward")
summary(step_out2)

# Our new model after the selection
reducedmodel_out2<-lm(y~x1+x4+x6+x7+x9+x11+x14+x16+x17
                      +x18+x19,data=df2)
summary(reducedmodel_out2)
AIC(selection_out2)
AIC(reducedmodel_out2)
anova(reducedmodel_out2,selection_out2)

# ANOVA test on reducedmodel2 vs model without x11
reducedmodel_out3<-lm(y~x1+x4+x6+x7+x9+x14+x16+x17
                      +x18+x19,data=df2)
reducedmodel_out2<-lm(y~x1+x4+x6+x7+x9+x11+x14+x16+x17
                      +x18+x19,data=df2)
anova(reducedmodel_out3,reducedmodel_out2)
summary(reducedmodel_out3)

# ANOVA test on reducedmodel_out3 vs model without x19
reducedmodel_out4<-lm(y~x1+x4+x6+x7+x9+x14+x16+x17
                      +x18,data=df2)
reducedmodel_out3<-lm(y~x1+x4+x6+x7+x9+x14+x16+x17
                      +x18+x19,data=df2)
anova(reducedmodel_out4,reducedmodel_out3)

summary(reducedmodel_out4)

# Compare the model with the influential observation and without
summary(reducedmodel_out4)
summary(reducedmodel4)

# Read dataset
data_raw<-read.csv('Life_Expectancy_Data1.csv', header=T,row.names=1 )

# Remove rows with  NaN's for Life_expectancy
data <-  data_raw[(!is.na(data_raw$SP.DYN.LE00.IN) ), ]

# In this exercise we have to use experimental design to study difference between average life expectancies with respect to the continent
# Our response variable is "Life Expectancy" - "SP.DYN.LE00.IN"
# Our data is continuous so we can use either
# "1-way ANOVA","2-way ANOVA","ANCOVA","RCBD","Latin squares"


# Since we have 1 factor variable given in the exercise ("Continent"), we  will use 1-way ANOVA,
# Under one-way ANOVA, we want to investigate whether there are differences in the average weight loss
# between Life expectancies for different continents.

attach(data) # so that we can access objects directly

# Some summary stat before anova
group.means<-tapply(SP.DYN.LE00.IN,Continent,mean)
group.means

boxplot(SP.DYN.LE00.IN~Continent,main='Comparing the Life expectancy for Continents',
        xlab='Continent', col="light gray", ylab = "Life Expectancy",)



# To employ in R the method of one-way ANOVA, we use the R bulit in function aov()
anova1way<-aov(SP.DYN.LE00.IN~as.factor(Continent),data=data)
summary(anova1way)
#Since F-value is very small we reject H_0 at the 5% significance level.


# Now we need to carry out the post-hoc ANOVA tests, 
# These tests will allow us to see where are the differences in life expectancy with respect to the continent,

# We will use Bonferonni and Tukey's Honest Significant Differences (HSD) post-hoc tests
cat("Bonferroni post-hoc test","\n")
pairwise.t.test(SP.DYN.LE00.IN, Continent, p.adj = "bonferroni")

# We can see that there are significant differences in Life expectancy for 
# e.g Africa and Asia, or Australia and Europe
cat("\n","Tukey post-hoc test","\n")
tukey.Life<-TukeyHSD(anova1way)
plot(tukey.Life)

#Now we will check whether the one-way ANOVA  assumptions are satisfied
#1.Assumption: normality of residuals
data$residuals1<-anova1way$residuals
par(mfrow=c(1,2))
hist(data$residuals1, main="Standardised residuals-histogram",xlab="Standardised residuals")
qqnorm(data$residuals1,pch=19)
qqline(data$residuals1)

# From a plot we can see that data has  a normal distribution but just to be sure we can run Shapiro test
shapiro.test(data$residuals1)#Shapiro test
 
# Since p value is above 0.05 we accept the h_0=The data is normally distributed

#2.Assumption : equal variances between the different continents
# leveneTest
if (!require("car")) install.packages("car")
library("car")
leveneTest(SP.DYN.LE00.IN~factor(data$Continent))

# again p-value is small so we accept H_0= there are equal variances between different continent for life expectancies

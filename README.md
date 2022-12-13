# Concrete-data-analysis

Data analysis about concrete. We want to find the best linear model that could explained the most our data.

## About the data

The data contain 8 predictors :

- Cement (component 1)(kg in a m^3 mixture)

- Blast Furnace Slag (component 2)(kg in a m^3 mixture)

- Fly Ash (component 3)(kg in a m^3 mixture)

- Water  (component 4)(kg in a m^3 mixture)

- Superplasticizer (component 5)(kg in a m^3 mixture)

- Coarse Aggregate  (component 6)(kg in a m^3 mixture)

- Fine Aggregate (component 7)(kg in a m^3 mixture)

- Age (day)


And we have the response below that we want to explained and assess :
  
- Concrete compressive strength(MPa, megapascals)

We randomly split the data 70% / 30% for the train / test.


## Content of the R script

What you will find in the *concrete.R* script is :

- First some basics linear model analysis with the lm function, We use the t-test, f-test and deviance test to manage our analysis. We also look at the residuals.

- A stepwise forward and backgward based on AIC criterium.

- Some basic dimension reduction analysis : PCR and PLS.

- Shrinkage methods like ridge and lasso with cross validation.

- All the resulting models are evaluated on a test set with the Mean square error. 
  



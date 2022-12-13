# just few installation things ----------
# install.packages("languageserver")
# install.packages("httpgd")
# install.packages("systemfonts")
# install.packages("MASS")
# install.packages("glmnet")
# install.packages('RcppEigen')

# clean
rm(list = ls()) # clear environment
graphics.off() # clear all figures
cat("\f") # clear console


# packages ----------
library('readxl')
library('httpgd')
library('glmnet')
library('MASS')
library('pls')
?validationplot
# preprocess the data -------------
concrete_data <- read_excel("Concrete_Data.xlsx")


View(concrete_data)
names(concrete_data) <- c("cement", "bfs", "fa", "water", "sp", "ca", "finea", "age", "cs")
names(concrete_data)
View(concrete_data) # shows datas
nrow(concrete_data); ncol(concrete_data)

# some information about the subject -----------
# Concrete is the most important material in civil engineering.
# Several studies independently have shown that concrete strength development 
# is determined not only by the water-to-cement ratio, but that it also
# is influenced by the content of other concrete ingredients


# Analyse the data and find a good model to understand and predict concrete compressive strength.

# variables -------
# This dataset has 9 attributes including 8 quantitative predictors,
# and 1 quantitative response variable.
# - Cement (component 1)(kg in a m^3 mixture) "cement"
# - Blast Furnace Slag (component 2)(kg in a m^3 mixture) "bfs"
# - Fly Ash (component 3)(kg in a m^3 mixture) "fa"
# - Water  (component 4)(kg in a m^3 mixture) "water"
# - Superplasticizer (component 5)(kg in a m^3 mixture) "sp"
# - Coarse Aggregate  (component 6)(kg in a m^3 mixture) "ca"
# - Fine Aggregate (component 7)(kg in a m^3 mixture) "finea"
# - Age (day) "age"
# - Concrete compressive strength(MPa, megapascals)  "cs" <- response

# open hgd -----
hgd()
hgd_browse()


# spliting data
#make this example reproducible
set.seed(2)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(concrete_data), replace = TRUE, prob = c(0.7, 0.3))
train  <- concrete_data[sample, ]
test   <- concrete_data[!sample, ]
attach(train) # attach all renames variables

# display dimension
nrow(train)
nrow(test)

?plot
plot(train, main = "train data")
plot(test, main = "test data")

# alpha = 5 % for the test

#Test with all parameters
lm1 <- lm(cs ~ bfs + fa + cement + water + sp + ca + finea + age)
# t-test hypothesis H0 : beta = 0 vs H1 : beta != 0 
# f-test hypothsesis H0 : beta_0 = .. = beta_1 = 1 vs beta_0 != ... != beta_n 
summary(lm1)
# We notice a that the variable ca and sp are not really relevant for cement
# R² = 0.64

# let's only look at variables with H1 p-values
lm2 <- lm(cs ~ bfs + fa + cement + water + age + sp + ca)
summary(lm2)
plot(lm2)

# let's only look at variables with the best p-values
lm3 <- lm(cs ~ bfs + fa + cement + water + age + sp)
summary(lm3)
plot(lm3)


lm4 <- lm(cs ~ bfs + fa + cement + water + age)
# Let's now do the F test
anova(lm1) # which test is it F test but what is the first and the second hypothesis

# let check residuals of lm1 ------------
# hypothesis :
# - independent,
# - have a distribution which is approximately Normal,
# - mean of zero,
# - constant variance.
# residuals vs fitted

# QQplot seems to give a kind of normality.
# residual vs fitted quite bad variances are quitely not constant
# residual vs tim ?
# residuals seems to follow normal distribution

# residuals plot
plot(lm1)
# with multipanel plot
par(mfrow = c(2, 2))
plot(lm1)

# is the residuals normal ?
par(mfrow = c(1, 1))
res1 <- residuals(lm1)
hist(res1, nclass = 25)

# plot for each non interesting variables
plot(res1 ~ ca, ylab = "Residuals", main = "residuals vs ca")
abline(h = 0, col = "red")

plot(res1 ~ finea, ylab = "Residuals", main = "residuals vs finea")
abline(h = 0, col = "red")

plot(res1 ~ sp, ylab = "Residuals", main = "residuals vs sp")
abline(h = 0, col = "red")

# with multipanel plot
par(mfrow = c(2, 2))
plot(lm2)


#ca and finea are not very good: we take them of
glm1 <- glm(lm1)
glm2 <- glm(lm2)
glm3 <- glm(lm3)
summary(glm1); summary(glm2); summary(glm3)



deviance_test <- function(model1, model2) {
    d0 <- deviance(model2)
    df0 <- df.residual(model2)
    d1 <- deviance(glm1)
    df1 <- df.residual(model1)
    f <- ((d0 - d1) / (df0 - df1)) / (d1 / df1)
    c <- qf(0.95, df0 - df1, df1)
    p <- pf(f, df0 - df1, df1, lower.tail = FALSE)
    return(list("f-stat" = f, "quant" = c, "p-value" = p))
}

deviance_test(glm1, glm2)
deviance_test(glm1, glm3)
# deviance_test(glm2, glm3)

#F<c: cannot reject H0: we keep the smaller model

?glm

# compare models
anova(lm1, lm2)
anova(lm1, lm3)
anova(lm2, lm3)


anova(lm2)

lm2=lm(cs ~ bfs+fa+cement+water+sp+age)
summary(lm2)


par(mfrow = c(2, 2))
plot(lm2)
par(mfrow = c(1,1))
res1 = residuals(lm2)
hist(res1, nclass = 25)


baseline <- cs ~ 1

simple_lm <- lm(baseline, data = train) # no predictors
step_fw <- stepAIC(simple_lm, scope=~bfs + fa + cement + water + sp + ca + finea + age, direction = "forward", trace = TRUE, data = train) 
summary(step_fw)
# Backward
step_bw = stepAIC(lm1, direction = "backward", trace = TRUE, data = train) # start with the model with all predictors
summary(step_bw)

all(bfs == BFS)

# processing datas
y <- train$cs
X <- as.matrix(train[, c('bfs', 'fa', 'cement', 'water', 'sp', 'ca', 'finea', 'age')])
X2 <- as.matrix(train[, c('bfs', 'fa', 'cement', 'water', 'age', 'sp','ca')])
X3 <- as.matrix(train[, c('bfs', 'fa', 'cement', 'water', 'age','sp')])
X4 <- as.matrix(train[, c('bfs', 'fa', 'cement', 'water', 'age')])

# scale data
train_scale <- scale(train)
X <- as.matrix(train_scale[, c('bfs', 'fa', 'cement', 'water', 'sp', 'ca', 'finea', 'age')])
y <- train_scale[ ,ncol(train_scale)]

# create lambda
lambda_seq <- seq(0, 1, by = 0.001)

ridge_cv <- cv.glmnet(x = X, y = y, alpha = 0, type.measure = "mse", lambda = lambda_seq, nfolds = 20)
ridge_cv
plot(ridge_cv, main = "ridge with lmT")

lambda_hat_ridge <- ridge_cv$lambda.min;lambda_hat_ridge
best_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_hat_ridge)
coef(best_ridge);coef(lm1)

# create lambda
lambda_seq <- seq(0, 1, by = 0.001)



# Lasso - using cross validation glmnet (alpha = 1 -> Lasso)
lasso_cv <- cv.glmnet(X, y, alpha = 1, type.measure = "mse", lambda = lambda_seq, nfold = 5)
lambda_hat_lasso <- lasso_cv$lambda.min; lambda_hat_lasso

best_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_hat_lasso)


plot(lasso_cv, main = "lasso with lmT")
coef(best_lasso); coef(lm1)

# # test the model -----
mean((test$cs - predict(lm1, newdata = test[, -ncol(test)]))^2)
mean((test$cs - predict(lm2, newdata = test[, -ncol(test)]))^2)
mean((test$cs - predict(lm3, newdata = test[, -ncol(test)]))^2)

y_pred_lasso <- predict(best_lasso, newx = as.matrix(test[,-ncol(test)]))
y_pred_ridge <- predict(best_ridge, newx = as.matrix(test[,-ncol(test)]))

mean((scale(test$cs) - y_pred_ridge)^2)
mean((test$cs - y_pred_lasso)^2)


# # test on the variable selection -----
ridge_cv_lm2 <- cv.glmnet(X2, y, alpha = 0, type.measure = "mse", lambda = lambda_seq, nfolds = 5)
lambda_hat_ridge <- ridge_cv_lm2$lambda.min
best_ridge_lm2 <- glmnet(X, y, alpha = 0, lambda = lambda_hat_ridge)
# ?cv.glmnet
plot(ridge_cv_lm2, main = "ridge lm2")

lasso_cv_lm2 <- cv.glmnet(X2, y, alpha = 1, type.measure = "mse", lambda = lambda_seq, nfolds = 5)
lambda_hat_lasso <- lasso_cv_lm2$lambda.min
best_lasso_lm2 <- glmnet(X, y, alpha = 1, lambda = lambda_hat_lasso)

plot(lasso_cv_lm2, main = "lasso lm2")

# # test on the variable selection 2 -----
ridge_cv_lm3 <- cv.glmnet(X3, y, alpha = 0, type.measure = "mse", lambda = lambda_seq, nfolds = 5)
lambda_hat_ridge <- ridge_cv_lm3$lambda.min
best_ridge_lm3 <- glmnet(X, y, alpha = 0, lambda = lambda_hat_ridge)
# ?cv.glmnet
plot(ridge_cv_lm3, main = "ridge lm3")

lasso_cv_lm3 <- cv.glmnet(X3, y, alpha = 1, type.measure = "mse", lambda = lambda_seq, nfolds = 5)
lambda_hat_lasso <- lasso_cv_lm3$lambda.min
best_lasso_lm3 <- glmnet(X, y, alpha = 1, lambda = lambda_hat_lasso)

plot(lasso_cv_lm3, main = "lasso lm3")

y_pred_lasso <- predict(best_lasso_lm3, newx = as.matrix(test[, -ncol(test)]))


# mean((test$cs - as.vector(y_pred_lasso))^2)


# lm1 
mean((test$cs - predict(lm1, newdata = test[, -ncol(test)]))^2)

# best ridge (lambda = 0)
y_pred_ridge <- predict(best_ridge, newx = as.matrix(test[,-ncol(test)]))
y_pred_lasso <- predict(best_lasso, newx = as.matrix(test[, -ncol(test)]))
y_pred_ridge <- predict(ridge_cv, s = 0, newx = as.matrix(test[, -ncol(test)]))

# is it the same thing 
all(as.vector(y_pred_ridge), as.vector(y_pred_ridge1))

mean((test$cs - y_pred_ridge)^2)

mean((test$cs - y_pred_lasso)^2)



# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
    sse <- sum((predicted - true)^2)
    sst <- sum((true - mean(true))^2)
    rsquare <- 1 - sse / sst
    mse <- sse / nrow(df)
    # Model performance metrics
    return(data.frame('MSE' = mse, 'Rsquare' = rsquare)) 
}

eval_results(test$cs, y_pred_ridge, as.matrix(test))


mse_ridge <- function(predictor, lambda_seq, model_name) {
    X <- as.matrix(train[, predictor])

    ridge_cv <- cv.glmnet(x = X, y = y, alpha = 0, type.measure = "mse", lambda = lambda_seq, nfolds = 20)
    plot(ridge_cv, main = paste("ridge with",model_name))
    lambda_hat_ridge <- ridge_cv$lambda.min
    best_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_hat_ridge)
    y_pred_ridge <- predict(best_ridge, newx = as.matrix(test[,predictor]))
    mse <- mean((test$CSS - y_pred_ridge)^2)
    return(list('model'= model_name, 'mse'=mse, 'lambda'=lambda_hat_ridge, 'result' = eval_results(test$CSS, y_pred_ridge, as.matrix(test))))
}

mse_ridge(c('Cement', 'BFS', 'FA', 'Water', 'Sp', 'CA', 'FineA', 'Age'), lambda_seq, 'lmT')
mse_ridge(c('Cement', 'BFS', 'FA', 'Water', 'Sp', 'CA', 'Age'), lambda_seq, 'lm1')
mse_ridge(c('Cement', 'BFS', 'FA', 'Water', 'Sp', 'Age'), lambda_seq, 'lm2')
mse_ridge(c('Cement', 'BFS', 'FA', 'Water', 'Age'), lambda_seq, 'lm3')



mse_lasso <- function(predictor, lambda_seq, model_name) {
    X <- as.matrix(train[, predictor])

    lasso_cv <- cv.glmnet(x = X, y = y, alpha = 1, type.measure = "mse", lambda = lambda_seq, nfolds = 20)
    plot(lasso_cv, main = paste("lasso with",model_name))
    lambda_hat_lasso <- lasso_cv$lambda.min
    best_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_hat_lasso)
    y_pred_lasso <- predict(best_lasso, newx = as.matrix(test[,predictor]))
    mse <- mean((test$CSS - y_pred_lasso)^2)
    return(list('model'= model_name, 'mse'=mse, 'lambda'=lambda_hat_lasso, 'result' = eval_results(test$CSS, y_pred_lasso, as.matrix(test))))
}

mse_lasso(c('Cement', 'BFS', 'FA', 'Water', 'Sp', 'CA', 'FineA', 'Age'), lambda_seq, 'lmT')
mse_lasso(c('Cement', 'BFS', 'FA', 'Water', 'Sp', 'CA', 'Age'), lambda_seq, 'lm1')
mse_lasso(c('Cement', 'BFS', 'FA', 'Water', 'Sp', 'Age'), lambda_seq, 'lm2')
mse_lasso(c('Cement', 'BFS', 'FA', 'Water', 'Age'), lambda_seq, 'lm3')

# ?predplot
# close the windows
# dev.off()



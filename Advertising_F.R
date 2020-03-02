#' VIOLATIONS OF OLS ASSUMPTIONS
#' 
#' Data: Advertising.csv
#' Sales (in thousands of units) for a particular product in 200 sales regions, against 
#' advertising budgets(in thousands of dollars) for TV, online, and print media ads.

df <- read.csv("D:/USF MS BAIS CourseWork/Sem-2/StatisticalDataMining/Week1- Regression assumptions/Advertising.csv")
str(df)
View(df)                                      # Data is clean; no missing values


#' Descriptive analysis

hist(df$sales, col = 'red')
den <- density(df$sales)                        # Density function
plot(den, main="Kernel Density of Sales", col="red")
hist(df$sales, breaks=20, prob=T, main="Histogram of Sales")
lines(den, col="red")

plot(sales ~ tv, data=df)                     # Fanning and concave
plot(sales ~ online, data=df)                 # Fanning
plot(sales ~ print, data=df)                  # No pattern
pairs(df)                                     # Pair plots

round(cor(df),3)                              # Correlation matrix
# install.packages("Hmisc") 
library("Hmisc")
rcorr(as.matrix(df))                          # Correlation matrix with p-value


#' OLS Estimation

ols0 <- lm(sales ~ 1, data=df)                # Intercept only model (i.e., no model) - baseline
summary(ols0)                                 
plot(ols0) 



ols1 <- lm(sales ~ tv + online + print, data=df)
summary(ols1)                                 # Print has no effect
plot(ols1)                                    # Curvature on residual plot

ols2 <- lm(sales ~ tv*online*print, data=df)  # Drop print and test for interactions
summary(ols2)                                 # Significant interaction
plot(ols2)                                    # Still curvature on residual plot

#' Test for assumptions

plot(ols2$res ~ ols2$fit)                     # Residual plot
hist(ols2$fit)

qqnorm(ols2$res)                              # Q-Q plot
qqline(ols2$res, col="red")

shapiro.test(ols2$res)                        # Shapiro-Wilk's test of multivariate normality
norm <- rnorm(200)
ks.test(norm, ols1$res)                       # Kolmogorov-Smirnov test

# install.packages("car")
library("car")
bartlett.test(list(ols2$res, ols2$fit))       # Bartlett's test of homoskedasticity
leveneTest(ols2$res, ols2$fit, center=mean)   # Levene's test of homoskedasticity

# install.packages("lmtest")
library(lmtest)
dwtest(ols2)                                    # Durbin-Watson test of autocorrelation


#' Data transformation

plot(log(sales) ~ tv, data=df)                # Still curvature, but less fanning
plot(log(sales) ~ online, data=df)            # No curvature or fanning

ols3 <- lm(log(sales) ~ tv*online*print, data=df)   # Exponential model
summary(ols3)                                 # Significant interaction
plot(ols3)                                    # Residuals more homogeneous but still has curvature 


#' Quadratic (second-order) model
#' I() creates a new variable by combining specified variables

ols4 <- lm(log(sales) ~ tv + I(tv*tv) + online + tv*online + print, data=df)   
summary(ols4)                                 # Quadratic term is significant
plot(ols4)                                    # Residuals show low bias and appear homogeneous

ols5 <- lm(log(sales) ~ tv + I(tv*tv) + online + I(online*online) + tv*online + print, data=df)   
summary(ols5)                                 # Final model; but this is not better than ols3
plot(ols5) 


#' MLE Estimation
#' MLE is robust to non-normal and heteroskedastic populations (but not linearity)

mle <- glm(log(sales) ~ tv + I(tv*tv) + online + tv*online + print, data=df, family=gaussian)
summary(mle)
plot(mle)

poisson <- glm(sales ~ tv + online + tv*online + print, data=df, family=poisson(link="log"))
summary(poisson)
plot(poisson)


#' Summarizing results using stargazer

library(stargazer)
stargazer(ols2, ols4, mle, poisson, title="Analysis of Advertising Data", type="text")
outfile = "D:/USF MS BAIS CourseWork/Sem-2/StatisticalDataMining/Week1- Regression assumptions/Advertising.html"
stargazer(ols1, ols2, ols3, ols4, mle, poisson, title="OLS Analysis of Advertising Data", out=outfile)


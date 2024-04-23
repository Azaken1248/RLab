'
LAB 1

a <- 5
b <- 6
c = a + b
c

a = 4
b = 2
c = a/b
c

a = 0
b = 1
cat(a | b)


a = 7
b = 3

if(a > b){
  cat(a)
}else{
  cat(b)
}

nums = c(1,5,2,3,10)
Sum = sum(nums)
avg = Sum/5
cat(avg)

a = 3
if(a%%2 == 0){
  cat("even")
}else{
  cat("odd")
}

a = -4
if(a < 0){
  cat("-ve")
}else if(a > 0){
  cat("+ve")
}else{
  cat("0")
}

'





'
LAB-2
v = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
v

v = 1:20
v

v = seq(from = 1, to = 20, by = 1)
v

odd = seq(from = 1, to = 100, by = 2)
avg = sum(odd)/length(odd)
avg

even = seq(from = 0, to = 25, by = 2)
odd = seq(from = 1, to = 25, by = 2)
Sum = even+odd
Sum


M = matrix(1:25, ncol = 5)

V = diag(M)
cat(V)

N = M[1:3,(ncol(M)-2):ncol(M)]
N

A = t(N)
A

B = solve(N)
B

C = A %*% B
D = N %*% B
E = A %*% N
C
D
E

'


'
LAB-3

df = read.csv2("Experiment-3_dataset.csv", header = TRUE, sep = ",")
df


summary(df)

table(df$Age)

xtabs(~ df$Age + df$IncomeC, data = df)


df = read.csv("https://drive.google.com/uc?id=1zO8ekHWx9U7mrbx_0Hoxxu6od7uxJqWw&export=download",header = TRUE)


head(df)

summary(df)
'



'
LAB-4

df = read.csv2("Experiment-3_dataset.csv", header = TRUE, sep = ",")
df

plot(x = df$Age, y = df$Married, type="p", main = "Age vs Marital Status", col = "red")
plot(x = df$Age, y = df$Married, type="l", main = "Age vs Marital Status", col = "red")
boxplot(df$Age, col = "blue", border = "red", xlab = "Age", ylab = "Frequency", main = "Age Distribution")
barplot(height = df$Age, names.arg = df$Income, xlab = "Income", ylab = "Age", col = "green", border = "red")
hist(x = df$Age, col = "blue", border = "red", xlab = "Age", ylab = "Frequency", main = "Histogram")
pie(x = df$Age, labels = df$Income, clockwise = T, col = rainbow(length(df$Age)))
legend("topright", legend = df$Income, fill = rainbow(length(df$Age)), border = "red")

library(plotrix)

pie3D(x = df$Age, labels = df$Income, explode = 0.1)

'


'
LAB-5

df = read.csv2("Experiment-3_dataset.csv", header = TRUE, sep = ",")
df

corr = cor(df$Age,df$IncomeC)
corr

cor_matrix = cor(df)
cor_matrix

corr_spear = cor(df$Age,df$IncomeC,method = "spearman")
corr_spear

corr_test = cor.test(df$Age,df$IncomeC)
corr_test$p.value

corr_test = cor.test(df$Age,df$IncomeC, method = "spearman")
corr_test

corr_test = cor.test(df$Age,df$IncomeC, method = "pearson")
corr_test


library(ppcor)
partial_corr = pcor(df, method = "pearson")
partial_corr

library(corrplot)

corrplot(cor_matrix, method = "color")

'


'
LAB-6

# Create data vectors
x <- c(15, 16, 20, 14, 17, 18, 19, 21, 22)
y <- c(30, 39, 32, 31, 34, 35, 37, 39, 40)

# Fit the regression line
model <- lm(y ~ x)
summary(model)

# Print regression coefficients
coefficients(model)

# Find Y when X is 29
y_hat <- coefficients(model)[1] + coefficients(model)[2] * 29
print(y_hat)


# Fit the regression line
model2 <- lm(x ~ y)
summary(model2)

# Print regression coefficients
coefficients(model2)

# Find X when Y is 45
x_hat <- coefficients(model2)[1] + coefficients(model2)[2] * 45
print(x_hat)

# Scatter plot of Y on X and regression line
plot(x, y, xlab = "X", ylab = "Y", main = "Y on X")
abline(model, col = "red")

# Scatter plot of X on Y and regression line
plot(y, x, xlab = "Y", ylab = "X", main = "X on Y")
abline(model2, col = "blue")


# Mean of X
mean_x <- mean(x)
print(mean_x)

# Mean of Y
mean_y <- mean(y)
print(mean_y)

# Correlation coefficient
cor_coef <- cor(x, y)
print(cor_coef)


library(scatterplot3d)


# Load the mtcars dataset
data(mtcars)

# Fit the multiple linear regression model
model <- lm(mpg ~ cyl + disp, data = mtcars)
summary(model)

# Visualize the model
s3d = scatterplot3d(mtcars$mpg,mtcars$cyl,mtcars$disp, pch = 16, highlight.3d = T, angle = 60)
s3d$plane3d(model)
'


'
LAB - 7

# 1. (i) Getting at least 5 heads
n <- 10
p <- 0.5
prob_atleast5 <- sum(dbinom(5:n, n, p))
print(prob_atleast5)
x <- 0:n
pmf <- dbinom(x, n, p)
barplot(pmf, names.arg = x, xlab = "Number of Heads", ylab = "Probability",
        main = "Binomial Distribution (n = 10, p = 0.5)")
abline(v = 4.5, col = "red", lwd = 2)
legend("topright", legend = paste0("P(X >= 5) = ", round(prob_atleast5, 4)),
       bty = "n")

# 1. (ii) Getting exactly 5 heads
prob_exact5 <- dbinom(5, n, p)
print(prob_exact5)
barplot(pmf, names.arg = x, xlab = "Number of Heads", ylab = "Probability",
        main = "Binomial Distribution (n = 10, p = 0.5)")
abline(v = c(4.5, 5.5), col = "red", lwd = 2)
legend("topright", legend = paste0("P(X = 5) = ", round(prob_exact5, 4)),
       bty = "n")

# 1. (iii) Getting between 4 and 6 heads, inclusive
prob_between46 <- sum(dbinom(4:6, n, p))
print(prob_between46)
barplot(pmf, names.arg = x, xlab = "Number of Heads", ylab = "Probability",
        main = "Binomial Distribution (n = 10, p = 0.5)")
abline(v = c(3.5, 6.5), col = "red", lwd = 2)
legend("topright", legend = paste0("P(4 <= X <= 6) = ", round(prob_between46, 4)),
       bty = "n")

# 2. (i) There are at most two emergency calls in a 10-minute interval
lambda <- 4
prob_atmost2 <- sum(dpois(0:2, lambda))
print(prob_atmost2)
x <- 0:10
pmf <- dpois(x, lambda)
barplot(pmf, names.arg = x, xlab = "Number of Emergency Calls",
        ylab = "Probability", main = "Poisson Distribution (lambda = 4)")
abline(v = 2.5, col = "red", lwd = 2)
legend("topright", legend = paste0("P(X <= 2) = ", round(prob_atmost2, 4)),
       bty = "n")

# 2. (ii) There are exactly three emergency calls in a 10-minute interval
prob_exact3 <- dpois(3, lambda)
print(prob_exact3)
barplot(pmf, names.arg = x, xlab = "Number of Emergency Calls",
        ylab = "Probability", main = "Poisson Distribution (lambda = 4)")
abline(v = c(2.5, 3.5), col = "red", lwd = 2)
legend("topright", legend = paste0("P(X = 3) = ", round(prob_exact3, 4)),
       bty = "n")

# 3. (i) P(26 <= X < 40)
mu <- 30
sigma <- sqrt(25)
prob_26to40 <- pnorm(40, mu, sigma) - pnorm(26, mu, sigma)
print(prob_26to40)
x <- seq(10, 50, by = 0.1)
pdf <- dnorm(x, mu, sigma)
plot(x, pdf, type = "l", xlab = "X", ylab = "Density",
     main = "Normal Distribution (mu = 30, sigma = 5)")
abline(v = c(26, 40), col = "red", lwd = 2)
legend("topright", legend = paste0("P(26 <= X < 40) = ", round(prob_26to40, 4)),
       bty = "n")

# 3. (ii) P(X >= 45)
prob_greaterequal45 <- 1 - pnorm(45, mu, sigma)
print(prob_greaterequal45)
plot(x, pdf, type = "l", xlab = "X", ylab = "Density",
     main = "Normal Distribution (mu = 30, sigma = 5)")
abline(v = 45, col = "red", lwd = 2)
legend("topright", legend = paste0("P(X >= 45) = ", round(prob_greaterequal45, 4)),
       bty = "n")

# 3. (iii) P(|X - 30| > 5)
prob_abs_diff_greater5 <- 2 * (1 - pnorm(35, mu, sigma))
print(prob_abs_diff_greater5)
plot(x, pdf, type = "l", xlab = "X", ylab = "Density",
     main = "Normal Distribution (mu = 30, sigma = 5)")
abline(v = c(25, 35), col = "red", lwd = 2)
legend("topright", legend = paste0("P(|X - 30| > 5) = ", round(prob_abs_diff_greater5, 4)),
       bty = "n")
'



'
LAB-8

# 1. Testing the mean life of tyres
# H0: mu = 15150 (population mean is 15,150 kms)
# H1: mu != 15150 (population mean is not 15,150 kms)
x_bar <- 15200  # Sample mean
mu_0 <- 15150   # Hypothesized population mean
sigma <- 1200   # Population standard deviation
n <- 49         # Sample size
alpha <- 0.05   # Significance level

t_stat <- (x_bar - mu_0) / (sigma / sqrt(n))
p_value <- 2 * pt(-abs(t_stat), df = n - 1)

cat("Test statistic (t) =", t_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Reject the null hypothesis at the 0.05 significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.05 significance level.\n")
}

# 2. Testing the mean life span
# H0: mu = 70 (population mean is 70 years)
# H1: mu > 70 (population mean is greater than 70 years)
x_bar <- 71.8   # Sample mean
mu_0 <- 70      # Hypothesized population mean
sigma <- 8.9    # Population standard deviation
n <- 100        # Sample size
alpha <- 0.05   # Significance level

t_stat <- (x_bar - mu_0) / (sigma / sqrt(n))
p_value <- pt(t_stat, df = n - 1, lower.tail = FALSE)

cat("Test statistic (t) =", t_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Reject the null hypothesis at the 0.05 significance level.\n")
  cat("The mean life span today seems to be greater than 70 years.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.05 significance level.\n")
  cat("There is no evidence that the mean life span today is greater than 70 years.\n")
}

# 3. Testing the mean breaking strength of ribbon
# H0: mu = 180 (population mean is 180 pounds)
# H1: mu < 180 (population mean is less than 180 pounds)
x_bar <- 169.5  # Sample mean
mu_0 <- 180     # Hypothesized population mean
sigma <- 5.7    # Sample standard deviation
n <- 5          # Sample size
alpha <- 0.01   # Significance level

t_stat <- (x_bar - mu_0) / (sigma / sqrt(n))
p_value <- pt(t_stat, df = n - 1)

cat("Test statistic (t) =", t_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Reject the null hypothesis at the 0.01 significance level.\n")
  cat("The mean breaking strength seems to be less than 180 pounds.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.01 significance level.\n")
  cat("There is no evidence that the mean breaking strength is less than 180 pounds.\n")
}
'


'
LAB-9


# 1. Testing if girls perform better than boys in statistics
# H0: mu_boys = mu_girls (mean marks for boys and girls are equal)
# H1: mu_girls > mu_boys (mean marks for girls is greater than mean marks for boys)
x_bar <- 78     # Mean marks for boys
y_bar <- 80     # Mean marks for girls
n1 <- 32        # Sample size for boys
n2 <- 35        # Sample size for girls
s1 <- 9         # Standard deviation for boys
s2 <- 7         # Standard deviation for girls
alpha <- 0.01   # Significance level

s_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
t_stat <- (y_bar - x_bar) / (s_pooled * sqrt(1/n1 + 1/n2))
df <- ((s1^2/n1 + s2^2/n2)^2) / ((s1^2/n1)^2/(n1 - 1) + (s2^2/n2)^2/(n2 - 1))
p_value <- pt(t_stat, df = df, lower.tail = FALSE)

cat("Test statistic (t) =", t_stat, "\n")
cat("Degrees of freedom =", df, "\n")
cat("p-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Reject the null hypothesis at the 0.01 significance level.\n")
  cat("The girls perform better than boys in the statistics course.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.01 significance level.\n")
  cat("There is no evidence that the girls perform better than boys in the statistics course.\n")
}

# 2. Testing if the proportion of voters is below 70%
# H0: p = 0.7 (proportion of voters in the population is 70%)
# H1: p < 0.7 (proportion of voters in the population is less than 70%)
x <- 90         # Number of people who voted in the survey
n <- 150        # Total number of people in the survey
p0 <- 0.7       # Hypothesized proportion of voters
alpha <- 0.05   # Significance level

p_hat <- x / n
z_stat <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
p_value <- pnorm(z_stat, lower.tail = TRUE)

cat("Test statistic (z) =", z_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Reject the null hypothesis at the 0.05 significance level.\n")
  cat("The proportion of voters in the population is below 70% this year.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.05 significance level.\n")
  cat("There is no evidence that the proportion of voters in the population is below 70% this year.\n")
}

'


'
LAB-10

# 1. Testing the variance of shipment weights
# H0: sigma^2 = 20 (variance of shipment weights is 20 kg^2)
# H1: sigma^2 != 20 (variance of shipment weights is not 20 kg^2)
weights <- c(38, 40, 45, 53, 47, 43, 55, 48, 52, 49)
n <- length(weights)
var_sample <- var(weights)
chi_sq_stat <- (n - 1) * var_sample / 20
p_value <- pchisq(chi_sq_stat, df = n - 1, lower.tail = FALSE)

cat("Test statistic (chi-squared) =", chi_sq_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject the null hypothesis at the 0.05 significance level.\n")
  cat("The variance of shipment weights is not equal to 20 kg^2.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.05 significance level.\n")
  cat("There is no evidence that the variance of shipment weights is not equal to 20 kg^2.\n")
}

# 2. Testing if the variance of source A is significantly greater than source B
# H0: sigma_A^2 = sigma_B^2 (variances of sources A and B are equal)
# H1: sigma_A^2 > sigma_B^2 (variance of source A is greater than source B)
var_A <- 225
var_B <- 200
n_A <- 10
n_B <- 11
f_stat <- var_A / var_B
p_value <- pf(f_stat, df1 = n_A - 1, df2 = n_B - 1, lower.tail = FALSE)

cat("Test statistic (F) =", f_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < 0.01) {
  cat("Reject the null hypothesis at the 0.01 significance level.\n")
  cat("The variance of source A is significantly greater than the variance of source B.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.01 significance level.\n")
  cat("There is no evidence that the variance of source A is significantly greater than the variance of source B.\n")
}

# 3. Testing if accident conditions were the same during the 10-week period
# H0: The frequencies of accidents are the same across the 10-week period
# H1: The frequencies of accidents are not the same across the 10-week period
accidents <- c(12, 8, 20, 2, 14, 10, 15, 6, 9, 4)
chi_sq_stat <- sum((accidents - mean(accidents))^2) / mean(accidents)
p_value <- pchisq(chi_sq_stat, df = length(accidents) - 1, lower.tail = FALSE)

cat("Test statistic (chi-squared) =", chi_sq_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject the null hypothesis at the 0.05 significance level.\n")
  cat("The frequencies of accidents are not the same across the 10-week period.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.05 significance level.\n")
  cat("There is no evidence that the frequencies of accidents are not the same across the 10-week period.\n")
}

# 4. Testing if there is a significant difference in the effect of the drug and sugar pills
# H0: The effects of the drug and sugar pills are the same
# H1: The effects of the drug and sugar pills are different
drug <- c(150, 30, 70)
sugar <- c(130, 40, 80)
observed <- c(drug, sugar)
expected <- c(rep(250, 3), rep(250, 3))
chi_sq_stat <- sum((observed - expected)^2 / expected)
p_value <- pchisq(chi_sq_stat, df = 2, lower.tail = FALSE)

cat("Test statistic (chi-squared) =", chi_sq_stat, "\n")
cat("p-value =", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject the null hypothesis at the 0.05 significance level.\n")
  cat("There is a significant difference in the effect of the drug and sugar pills.\n")
} else {
  cat("Fail to reject the null hypothesis at the 0.05 significance level.\n")
  cat("There is no evidence of a significant difference in the effect of the drug and sugar pills.\n")
}

'

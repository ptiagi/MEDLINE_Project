# The data directory for this class has a file called Arrowsmith.xls which contains a 
# “gold standards” dataset used to model relevance in an approach to literature-based 
# discovery (http://dx.doi.org/10.1093/bioinformatics/btm161). Your goal is to replicate 
# the model described in Table S2 (in the supplemental data file) in that paper, write up 
# an account of your investigation.

# The steps include:
# 1. Read the paper and learn about literature-based discovery.
# 2. Convert the Excel file (.xls) to comma-separated-value plain text file (.csv).
# 3. Load the csv file into R and construct the following attributes

Arrowsmith = read.csv("Arrowsmith.csv", header = TRUE)
dim(Arrowsmith)
summary(Arrowsmith)
head(Arrowsmith)

# We would calculate the seven features called X1, X2, X3, X4, X5, X6 and X7 to filter out the data
# and get most of the values between 0 and 1. Also, the values that very high are curtailed using the 
# log function (example X5 and X7).

# X1 = 1 if (nA > 1 or A-lit size < 1000) and (nC > 1 or C-lit size < 1000), 0 otherwise
X1 <- ifelse((Arrowsmith$nA > 1 | Arrowsmith$A.lit.size < 1000) & (Arrowsmith$nC > 1 | Arrowsmith$C.lit.size < 1000),1,0)

# X2 = 1 if nof MeSH > 0 and < 99999, 0.5 if nof MeSH = 99999, 0 otherwise
X2 = ifelse(Arrowsmith$nof.MeSH.in.common > 0 & Arrowsmith$nof.MeSH.in.common < 99999,1,ifelse(Arrowsmith$nof.MeSH.in.common == 99999,0.5,0))


# X3 = 1 if nof semantic categories > 0, 0 otherwise
X3 = ifelse(Arrowsmith$nof.semantic.categories > 0,1,0)

# X4 = cohesion score if cohesion score < 0.3, 0.3 otherwise
X4 = ifelse(Arrowsmith$cohesion.score < 0.3,Arrowsmith$cohesion.score,0.3)

# X5 = -|log10(n in MEDLINE) – 3|
X5 = -abs(log10(Arrowsmith$n.in.MEDLINE)-3)

# X6 = max(min(1st year in MEDLINE,2005),1950)
X6 = mapply(max, mapply(min, Arrowsmith$X1st.year.in.MEDLINE,2005), 1950)

# X7 = min(8,-log10(pAC+0.000000001))
X7 = mapply(min, 8,-log10(Arrowsmith$pAC + 0.000000001))

# I1 = 1 if Arrowsmith search = ‘retinal detachment’, 0 otherwise 
I1 = ifelse(Arrowsmith$Arrowsmith.search == 'retinal detachment vs aortic aneurysm',1,0)

#I2 = 1 if Arrowsmith search = ‘NO and mitochondria vs PSD’ Similarly for I3 through I6.
I2 = ifelse(Arrowsmith$Arrowsmith.search == 'NO and mitochondria vs PSD',1,0)

#I3 = 1 if Arrowsmith search = ‘mGluR5 vs lewy bodies’
I3 = ifelse(Arrowsmith$Arrowsmith.search == 'mGluR5 vs lewy bodies',1,0)

#I4 = 1 if Arrowsmith search = ‘magnesium vs migraine’
I4 = ifelse(Arrowsmith$Arrowsmith.search == 'magnesium vs migraine',1,0)

#I5 = 1 if Arrowsmith search = ‘Calpain vs PSD’
I5 = ifelse(Arrowsmith$Arrowsmith.search == 'Calpain vs PSD',1,0)

#I6 = 1 if Arrowsmith search = ‘APP vs reelin’
I6 = ifelse(Arrowsmith$Arrowsmith.search == 'APP vs reelin',1,0)

# Y = 1 if target = 0 or 2, 0 otherwise
Y = ifelse(Arrowsmith$target == 0 | Arrowsmith$target == 2, 1 , 0)

# 3. Get to know the dataset: assess the summary statistics, histograms, 
# and pairwise scatter plots before and after your transformation. Are there 
# missing values or outliers? Include screen shots as figures in your write-up.

# For Feature X1 for B-terms.
summary(X1)
summary(Arrowsmith$nA)
summary(Arrowsmith$nC)
plot(Arrowsmith$nA, Arrowsmith$nC)
# The plot shows that literature A and C have some outliers. Like in nA outliers are the values that
# are beyond 1500 and for literature C the outlier is beyond 4000.

# Plot for visualising the original and transformed data.
before <- hist(Arrowsmith$nC, col = rgb(0,0,1,1/4))
after <- hist(X1, col = rgb(1,0,0,1/4))

# Limiting the values of 'nC' from 0-5 as the value is same from 0-100 and to show the 0 and 1 values 
# of X1 (transformed data) we should limit the value of 'nC'.

plot(before, col = rgb(0,0,1,1/4), xlim = c(0,5))
plot(after, col = rgb(1,0,0,1/4), xlim = c(0,4), add = T)

# Scatter Plot.
# The below plots show the value of transformed data for each value of original data.
plot(X1,Arrowsmith$nA)
plot(X1,Arrowsmith$nC)

# For Feature X2 for B-terms.

summary(X2)
summary(Arrowsmith$nof.MeSH.in.common)

# Plot for visualising the original and transformed data.

before <- hist(Arrowsmith$nof.MeSH.in.common, col = rgb(0,0,1,1/4))
after <- hist(X2, col = rgb(1,0,0,1/4))
plot(before, col = rgb(0,0,1,1/4), xlim = c(0,10))
plot(after, col = rgb(1,0,0,1/4), xlim = c(0,1), add = T)

# Scatter Plot.
# The below plots show the value of transformed data for each value of original data.
plot(X2,Arrowsmith$nof.MeSH.in.common)


# For Feature X3 for B-terms.
summary(X3)
summary(Arrowsmith$nof.semantic.categories)

# Plot for visualising the original and transformed data.

before <- hist(Arrowsmith$nof.semantic.categories, col = rgb(0,0,1,1/4))
after <- hist(X3, col = rgb(1,0,0,1/4))
plot(before, col = rgb(0,0,1,1/4), xlim = c(0,5))
plot(after, col = rgb(1,0,0,1/4), xlim = c(0,1), add = T)

# Scatter Plot.
# The below plots show the value of transformed data for each value of original data.
plot(X3,Arrowsmith$nof.semantic.categories)

# For Feature X4 for B-terms.
summary(X4)
summary(Arrowsmith$cohesion.score)

# Plot for visualising the original and transformed data.

before <- hist(Arrowsmith$cohesion.score, col = rgb(0,0,1,1/4))
after <- hist(X4, col = rgb(1,0,0,1/4))
plot(before, col = rgb(0,0,1,1/4), xlim = c(0,0.4))
plot(after, col = rgb(1,0,0,1/4), xlim = c(0,0.4), add = T)

# Scatter Plot.
# The below plots show the value of transformed data for each value of original data.
plot(X4,Arrowsmith$cohesion.score)

# For Feature X5 for B-terms.
summary(X5)
summary(Arrowsmith$n.in.MEDLINE)

# Plot for visualising the original and transformed data.

before <- hist(Arrowsmith$n.in.MEDLINE, col = rgb(0,0,1,1/4))
after <- hist(X5, col = rgb(1,0,0,1/4))
plot(before, col = rgb(0,0,1,1/4))
plot(after, col = rgb(1,0,0,1/4), add = T)

# Scatter Plot.
# The below plots show the value of transformed data for each value of original data.
plot(X5, Arrowsmith$n.in.MEDLINE)

# For Feature X6 for B-terms.
summary(X6)
summary(Arrowsmith$X1st.year.in.MEDLINE)

# Plot for visualising the original and transformed data.

before <- hist(Arrowsmith$X1st.year.in.MEDLINE, col = rgb(0,0,1,1/4))
after <- hist(X6, col = rgb(1,0,0,1/4))
plot(before, col = rgb(0,0,1,1/4), xlim = c(1500,2000))
plot(after, col = rgb(1,0,0,1/4), add = T)

# Scatter Plot.
# The below plots show the value of transformed data for each value of original data.
plot(X6,Arrowsmith$X1st.year.in.MEDLINE)


# For Feature X7 for B-terms.
summary(X7)
summary(Arrowsmith$pAC)

# Plot for visualising the original and transformed data.

before <- hist(Arrowsmith$pAC,col = rgb(0,0,1,1/4))
after <- hist(X7, col = rgb(1,0,0,1/4))
plot(after, col = rgb(1,0,0,1/4))
plot(before,col = rgb(0,0,1,1/4), add = T)

# Scatter Plot.
# The below plots show the value of transformed data for each value of original data.
plot(X7,Arrowsmith$pAC)


# 4. Fit a logistic regression model and assess the validity of its assumptions and statistical significance. 
# Interpret the parameters and your model. Are your parameter estimates different from the ones reported? If so, why?

# Applying logistic model for the generated features using the data of literature A and C.
model_fit <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + I1 + I2 + I3 + I4 + I5 + I6, family = "binomial", data = Arrowsmith)


# Assumptions for Logistic Regression:

# There are two key conditions for fitting a logistic regression model:
#  1. Each predictor xi is linearly related to logit(pi) if all other predictors are held constant.
#  2. Each outcome Yi is independent of the other outcomes.
#  The above mentioned conditions are met as Y is independent of the features (X1, X2 .. X7) and condition
#  first is also satisfied.

plot(model_fit)

after <- data.frame(X1,X2,X3,X4,X5,X6,X7)
plot(after[1:7], main='Pairwise scatterplot from X1-X7')

# 1). The Residual plot that has been generated shows a pattern and fits with the line which indicates that the 
# model is a good model. Though there are some outliers that can be seen in the plot.

# 2). The Normal Q-Q plot also kind of fits with the line but has a few outliers.

# 3). The Scale-Location plot indicates the relation between Std. Deviation Residual and Predicted values that meets with
# a line though there is a line that doesn't fit and indicates the presence of outliers.

# 4). The data also fits with the line in the fourth plot which again indicates that there is some releant data but we have 
# some outliers too.

summary(model_fit)
coef(model_fit)

# The coeffcient values shows that the values match exactly with the values given in the paper and there are no differences in the
# reported parameters and the calculated parameters.

confint(model_fit)

# The 97.5% confidence interval suggest that we are 97.5% sure that the actual values would lie
# between the interval generated above.

# CALCULATION of B-TERM SCORE:

B_term_score = (0.73*X1) + (0.99*X2) + (1.32*X3) + (13.8*X4) + (0.59*X5) + (0.040*X6) + (0.19*X7)

# Value of B-TERM SCORE:

B_term_score

hist(B_term_score)

# The histogram shows the distribution of B-term Score.
# It seems to be normally distributed.

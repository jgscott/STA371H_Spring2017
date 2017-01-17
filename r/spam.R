
# Spam filtering
spamfit = read.csv('../data/spamfit.csv', header=TRUE)
spamtest = read.csv('../data/spamtest.csv', header=TRUE)

# Fit the model versus all predictors (. is shorthand for this)
glm1 = glm(y ~ ., data=spamfit, family='binomial')
summary(glm1)

# Make tables to calculate error rates
yhat_train = predict(glm1, newdata=spamfit, type='response')
class_train = yhat_train>0.5
xtabs(~ class_train + spamfit$y)

# Calculate error rates
97/(97+1703) # FPR
321/(321+879)  # FNR
97/(97+879)  # FDR

# Predict on test data
yhat_test = predict(glm1, newdata=spamtest, type='response')
class_test = yhat_test>0.5
xtabs(~ class_test + spamtest$y)

# Calculate error rates on test set
27/(27+358) # FPR
51/(51+165)  # FNR
27/(27+165)  # FDR

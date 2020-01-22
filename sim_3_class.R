# Model parameters
mu0 <- c(-1, 0) # Mean parameter of feature 1
mu1 <- c(1, 0)  # Mean parameter of feature 2
mu2 <- c(3, 0)  # Mean parameter of feature 2
alpha <- -1     
beta1 <- 2    
beta2 <- 3

class   <- factor(sort(t(rmultinom(1000, size = 1, prob = rep(1,3)/3)) %*% 1:3))
feature <- rbind(rmvnorm(sum(class == 1), mu0),
                 rmvnorm(sum(class == 2), mu1),
                 rmvnorm(sum(class == 3), mu2))

outcome   <- rbinom(n, size = 1, 
                    p = exp(alpha + 
                              (beta1)*(class==2) + beta2*(class==3))/(1+exp(alpha + (beta)*(class==2) + beta2*(class==3))))


train <- data.frame(id = 1:n, class = class, feature = feature, 
                    outcome = outcome)

# Fit Gaussian mixture model with two components
fit <- Mclust(train[,c("feature.1", "feature.2")], G = 3)
table(train$class, predict(fit)$classification)

# Classfication of training data
pred <- predict(fit)
reord <- order(fit$parameters$mean[1,])

train$predicted <- factor(pred$classification)
levels_key <- reord
names(levels_key) <- levels(train$predicted)
train$predicted   <- recode_factor(train$predicted, !!!levels_key)
table(train$class, train$predicted)

# Extract cluster probabilities as a fuzzy clustering
probs <- fit$z[,reord]
colnames(probs) <- c("p1", "p2", "p3")
train <- cbind(train, probs)

# True
glm(outcome ~ class, data = train, family = binomial, x = T, y = T)

# Naive
glm(outcome ~ predicted, data = train, family = binomial, x = T, y = T)

# Fuzzy
glm(outcome ~ p2 + p3, data = train, family = binomial, x = T, y = T)

library(MASS)
library(caret)

N <- 100;

sigma <- 0.3;

# simulate data

set.seed(1334)
x1 <- runif(N);

x2 <- rnorm(N, sd = sigma);
fev.true <- 0;

#mu <- x1^3 + 2*x1^2 - 1;
#x2 <- rnorm(N, mean = mu, sd = sigma);
#fev.true <- 1 - sigma^2 / var(mu);

X <- matrix(c(x1, x2), ncol=2);

# fit KNN regression

K <- 30;

fit.x1 <- knnreg(X, x1, k = K);
fit.x2 <- knnreg(X, x2, k = K);

x1.hat <- predict(fit.x1, X);
x2.hat <- predict(fit.x2, X);

X.hat <- matrix(c(x1.hat, x2.hat), ncol=2);

plot(X[,1], X[,2])
points(X.hat[,1], X.hat[,2], pch=20)

# fit loess regression

fit <- loess(x2 ~ x1);
x2.hat <- predict(fit, x1);

plot(X[,1], X[,2])
idx <- order(x1);
lines(x1[idx], x2.hat[idx], pch=20)

ssr <- sum((x2 - x2.hat)^2);
sst <- sum((x2 - mean(x2))^2);

fev <- 1 - ssr / sst;

print(fev.true)
print(fev)


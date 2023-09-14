library(MASS)
library(scam)

# fraction of explained variance
fev <- function(x, x.hat) {
	ssr <- sum((x - x.hat)^2);
	sst <- sum((x - mean(x))^2);
	1 - ssr / sst
}

# simulate data

set.seed(1334)

N <- 100;
sigma <- 0.3;

x1 <- runif(N);

#x2 <- rnorm(N, sd = sigma);
#fev.true <- 0;

mu <- -3*(x1 - 1)^2 + 5;
x2 <- rnorm(N, mean = mu, sd = sigma);
fev.true <- 1 - sigma^2 / var(mu);

X <- matrix(c(x1, x2), ncol=2);

# fit linear regression

fit <- lm(x2 ~ x1);
d <- data.frame(x1=x1);
x2.hat <- predict(fit, d, se.fit=TRUE, type="response");

plot(X[,1], X[,2])
idx <- order(x1);
lines(x1[idx], x2.hat$fit[idx], pch=20)
# confidence band
z <- qnorm(1 - 0.05/2);
lines(x1[idx], x2.hat$fit[idx] - z*x2.hat$se.fit[idx], pch=20)
lines(x1[idx], x2.hat$fit[idx] + z*x2.hat$se.fit[idx], pch=20)

x2.ci <- predict(fit, d, interval="confidence");
lines(x1[idx], x2.ci[idx, 2], col="blue")
lines(x1[idx], x2.ci[idx, 3], col="blue")

x2.ci <- predict(fit, d, interval="prediction");
lines(x1[idx], x2.ci[idx, 2], col="grey30")
lines(x1[idx], x2.ci[idx, 3], col="grey30")


# fit loess regression

fit <- loess(x2 ~ x1);
x2.hat <- predict(fit, x1);

plot(X[,1], X[,2])
idx <- order(x1);
lines(x1[idx], x2.hat[idx], pch=20)

fev.hat <- fev(x2, x2.hat);

print(fev.true)
print(fev.hat)

# fit shape constrained generalized additive models

k <- 5;
fit <- scam(x2 ~ s(x1, k = k, bs = "mpi"));

# predict mean with prediction interval for new data
predict_interval_scam <- function(fit, newdata, level=0.95) {
	y.hat <- predict(fit, se.fit=TRUE, type="response", newdata);

	# sigma2.hat is based on old data (1..N)
	nu <- length(fit$residuals) - length(coef(fit));
	sigma2.hat <- 1 / nu * sum( fit$residuals^2 );

	# \sqrt( \hat{ var[w] } ) = 
	#   \sqrt( \hat{ var[y_{N+1}] } + \hat{ var[\hat{y_{N+1}}] }
	sqrt_hat_var_w <- sqrt(sigma2.hat + y.hat$se.fit^2);
	
	tq <- qt(1 - (1 - level)/2, df=nu);
	se <- tq*sqrt_hat_var_w;
	data.frame(
		fit = y.hat$fit,
		lwr = y.hat$fit - se,
		upr = y.hat$fit + se
	)
}

x2.hat <- predict_interval_scam(fit, d);

plot(X[,1], X[,2])
idx <- order(x1);
lines(x1[idx], x2.hat$fit[idx], pch=20)
# confidence band
lines(x1[idx], x2.hat$lwr[idx], pch=20)
lines(x1[idx], x2.hat$upr[idx], pch=20)

fev.hat <- fev(x2, x2.hat$fit);

print(fev.true)
print(fev.hat)


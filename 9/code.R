library(MASS)	#For kde2d and mvrnorm

#Question-1:
gen_cholesky <- function(mu, SIGMA, sample) {
	U <- matrix(runif((2 * sample), 0, 1), nrow = sample, ncol = 2);
#	u1 <- runif(sample, 0, 1);
#	u2 <- runif(sample, 0, 1);
	R <- -2 * log(U[,1]);
	V <- 2 * pi * U[,2];
#	R <- -2 * log(u1);
#	V <- 2 * pi * u2;
	Z <- matrix(c((sqrt(R) * cos(V)), (sqrt(R) * sin(V))), nrow = sample, ncol = 2);
#	Z1 <- sqrt(R) * cos(V);
#	Z2 <- sqrt(R) * sin(V);

##	Z <- mvrnorm(n = sample, c(0, 0), matrix(c(1,0,0,1), 2, 2));

	rho = (SIGMA[1,2] / sqrt(SIGMA[1,1] * SIGMA[2,2]));	#Correlation
	sigma = c(sqrt(SIGMA[1,1]), sqrt(SIGMA[2,2]));	#Standard Deviations
	X <- matrix(c((mu[1] + (sigma[1] * Z[,1])), (mu[2] + (rho * sigma[2] * Z[,1]) + (sqrt(1 - (rho^2)) * sigma[2] * Z[,2]))), nrow = sample, ncol = 2);

	cat("The sample means, variances, and covariance are :","\nmean(X1) =", mean(X[,1]), "\nmean(X2) =", mean(X[,2]), "\nvariance(X1) =", var(X[,1]), "\nvariance(X2) =", var(X[,2]), "\ncorrelation(X1, X2) =", cor(X[,1], X[,2]), "\n");
	cat("\nWhile, the actual means, variances, and covariance are :","\nmean(X1) =", mu[1], "\nmean(X2) =", mu[2], "\nvariance(X1) =", SIGMA[1,1], "\nvariance(X2) =", SIGMA[2,2], "\ncorrelation(X1, X2) =", rho, "\n");

	f <- kde2d(X[,1], X[,2], n = sample);	# Two dimensional kernel density approximation
	pdf(paste("1",100*rho,".pdf"));
	contour(f, xlab = "X1", ylab = "X2", main = "")
#	legend('topright', legend = paste("a =", rho), lty = 0, bty = 'n');

##Question-2 ::
	X_T <- mvrnorm(n = sample*100, mu, SIGMA);

	pdf(paste("2_X1",100*rho,".pdf"));
	plot(ecdf(sort(X[,1])), do.points = FALSE, main = "", col = "red", xlab = "", ylab = "")
	par(new = TRUE)
	plot(ecdf(sort(X_T[,1])), do.points = FALSE, main = "", col = "green", xlab = "", ylab = "", axes = FALSE)
	legend('topleft', legend = c('Experimental (Empirical)', 'Theoretical (Actual)'), lty = 1, col = c("red", "green"), bty = 'n')
#	title("Cumulative Distribution Function for X1");
	title(xlab = "x", ylab = "F(x)");

	pdf(paste("2_X2",100*rho,".pdf"));
	plot(ecdf(sort(X[,2])), do.points = FALSE, main = "", col = "red", xlab = "", ylab = "")
	par(new = TRUE)
	plot(ecdf(sort(X_T[,2])), do.points = FALSE, main = "", col = "green", xlab = "", ylab = "", axes = FALSE)
	legend('topleft', legend = c('Experimental (Empirical)', 'Theoretical (Actual)'), lty = 1, col = c("red", "green"), bty = 'n')
#	title("Cumulative Distribution Function for X2");
	title(xlab = "x", ylab = "F(x)");

}

#Question-3:
gen_conditional <- function(mu, SIGMA, sample) {
	U <- matrix(runif((2 * sample), 0, 1), nrow = sample, ncol = 2);
#	u1 <- runif(sample, 0, 1);
#	u2 <- runif(sample, 0, 1);
	R <- -2 * log(U[,1]);
	V <- 2 * pi * U[,2];
#	R <- -2 * log(u1);
#	V <- 2 * pi * u2;
	Z <- matrix(c((sqrt(R) * cos(V)), (sqrt(R) * sin(V))), nrow = sample, ncol = 2);
#	Z1 <- sqrt(R) * cos(V);
#	Z2 <- sqrt(R) * sin(V);

##	Z <- mvrnorm(n = sample, c(0, 0), matrix(c(1,0,0,1), 2, 2));

	rho = (SIGMA[1,2] / sqrt(SIGMA[1,1] * SIGMA[2,2]));	#Correlation
	sigma = c(sqrt(SIGMA[1,1]), sqrt(SIGMA[2,2]));	#Standard Deviations

	X <- matrix(0, nrow = sample, ncol = 2);
	X[,1] <- (mu[1] + (sigma[1] * Z[,1]));
	X[,2] <- ((mu[2] + (rho * (sigma[2] / sigma[1]) * (X[,1] - mu[1]))) + (sqrt(1 - (rho^2)) * sigma[2] * Z[,2]));

	cat("The sample means, variances, and covariance are :","\nmean(X1) =", mean(X[,1]), "\nmean(X2) =", mean(X[,2]), "\nvariance(X1) =", var(X[,1]), "\nvariance(X2) =", var(X[,2]), "\ncorrelation(X1, X2) =", cor(X[,1], X[,2]), "\n");
	cat("\nWhile, the actual means, variances, and covariance are :","\nmean(X1) =", mu[1], "\nmean(X2) =", mu[2], "\nvariance(X1) =", SIGMA[1,1], "\nvariance(X2) =", SIGMA[2,2], "\ncorrelation(X1, X2) =", rho, "\n");

}

### EXECUTION :::
set.seed(1);

a = c(-0.25, 0, 0.25);
mu <- c(5, 8);

cat("Generation by Cholesky’s decomposition :-")
for (i in 1:3) {
	cat("\n\nCase -", i, ":: a =", a[i], "::\n");
	SIGMA <- matrix(c(1, (2 * a[i]), (2 * a[i]), 4), nrow = 2, ncol = 2);
	gen_cholesky(mu, SIGMA, 1000);
}

cat("\n\n###   ###   ###   ###   ###   ###   ###\n\n\n");

cat("Generation from conditional distribution :-")
for (i in 1:3) {
	cat("\n\nCase -", i, ":: a =", a[i], "::\n");
	SIGMA <- matrix(c(1, (2 * a[i]), (2 * a[i]), 4), nrow = 2, ncol = 2);
	gen_conditional(mu, SIGMA, 1000);
}
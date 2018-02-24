genWeibull <- function(val, beta, theta) {
	return ((1/theta) * ((-log(1 - val))^(1/beta)));
}

genMix <- function(sample, beta_1, theta_1, beta_2, theta_2, p) {
	M <- vector(length = sample);
	for (i in 1:sample) {
		u <- runif(2,0,1);
		if (u[1] < p) {
			M[i] = genWeibull(u[2], beta_1, theta_1);
		}
		else {
			M[i] = genWeibull(u[2], beta_2, theta_2);
		}
	}
	return (M);
}

set.seed(1);

sample = 50;

M <- genMix(sample, 2, 1, 1.5, 1, 0.4);

cat("The sample mean and variance, for the", sample, "random numbers generated from mixture of the two given Weibull distributions, are calculated to be", mean(M), ",and", var(M),"respectively.\n");

pdf("3.pdf");
#hist(M, xlab = "x", breaks = 50, main = paste("Histogram of Mixed Distribution\nn =", sample), col = "red");
hist(M, xlab = "x", main = "", col = "red");
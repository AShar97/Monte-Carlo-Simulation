##Question-1:
f1 <- function(m) {
	set.seed(1);
	U <- runif(m,0,1);
	Y <- exp(sqrt(U));
	return (c(mean(Y),sqrt(var(Y))));
}

I1 <- vector(length = 4);
S1 <- vector(length = 4);
Radius1 <- vector(length = 4);

##Same for all questions.
percentage = 95;
alpha = 1 - percentage/100;
p = (1 - (alpha/2));
z = qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE);

for (i in 2:5) {
	Out <- f1(10^i);
	I1[i-1] = Out[1];	S1[i-1] = Out[2];
	Radius1[i-1] = (z * (S1[i-1] / sqrt(10^i)));
}

cat("Using Standard Monte Carlo simulation algorithm ::\n");

for (i in 2:5) {
	cat("The 95% confidence interval for", 10^i, "values is (", (I1[i-1] - Radius1[i-1]), ",", (I1[i-1] + Radius1[i-1]), ") .\n");
}

##Question-2:
f2 <- function(m) {
	set.seed(1);
	U <- runif(m,0,1);
	Y <- ((exp(sqrt(U)) + exp(sqrt(1 - U))) / 2);
	return (c(mean(Y),sqrt(var(Y))));
}

I2 <- vector(length = 4);
S2 <- vector(length = 4);
Radius2 <- vector(length = 4);
for (i in 2:5) {
	Out <- f2(10^i);
	I2[i-1] = Out[1];	S2[i-1] = Out[2];
	Radius2[i-1] = (z * (S2[i-1] / sqrt(10^i)));
}

cat("\nUsing antithetic variates ::\n");

for (i in 2:5) {
	cat("The 95% confidence interval for", 10^i, "values is (", (I2[i-1] - Radius2[i-1]), ",", (I2[i-1] + Radius2[i-1]), ") .\n");
}

for (i in 2:5) {
	cat("The percentage of variance rejection from standard method for", 10^i, "values is", (((S1[i-1]^2) - (S2[i-1]^2))/(S1[i-1]^2)) * 100, "% .\n");
}

##Question-3:
f3 <- function(m) {
	set.seed(1);
	U <- runif(m,0,1);
	Y <- sqrt(U);
	X <- exp(Y);
	c = -(cov(X,Y)/var(Y));
	mu_y = mean(Y);
	W <- (X + c * (Y - mu_y));
	return (c(mean(W),sqrt(var(W))));
}

I3 <- vector(length = 4);
S3 <- vector(length = 4);
Radius3 <- vector(length = 4);
for (i in 2:5) {
	Out <- f3(10^i);
	I3[i-1] = Out[1];	S3[i-1] = Out[2];
	Radius3[i-1] = (z * (S3[i-1] / sqrt(10^i)));
}

cat("\nUsing control variates ::\n");

for (i in 2:5) {
	cat("The 95% confidence interval for", 10^i, "values is (", (I3[i-1] - Radius3[i-1]), ",", (I3[i-1] + Radius3[i-1]), ") .\n");
}

for (i in 2:5) {
	cat("The percentage of variance rejection from standard method for", 10^i, "values is", (((S1[i-1]^2) - (S3[i-1]^2))/(S1[i-1]^2)) * 100, "% .\n");
}
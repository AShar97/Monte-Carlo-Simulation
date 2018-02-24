genNormal_Box_Muller<-function(sample) {
	N<-vector(length = sample);
	set.seed(1);
	for (i in seq(1, sample, 2)) {
		u<-runif(2, 0, 1);
		R = -2 * log(u[1]);
		V = 2 * pi * u[2];
		N[i] = sqrt(R) * cos(V);
		N[i + 1] = sqrt(R) * cos(V);
	}
	return(N);
}

genNormal_Marsaglia_Bray<-function(sample) {
	N<-vector(length = sample);
	set.seed(1);
	j = 0;
	for (i in seq(1, sample, 2)) {
		repeat {
			j = j + 2;
			u<-runif(2, 0, 1);
			u = (2 * u) - 1;
			X = (u[1]^2) + (u[2]^2);
			if (X < 1) {
				Y = sqrt((-2 * log(X))/X);
				N[i] = u[1] * Y;
				N[i + 1] = u[2] * Y;
				break;
			}
		}
	}
	return(c((1 - (sample / j)), N));
}

time_BM = proc.time()[3];
N_BM <- genNormal_Box_Muller(10000);
time_BM = proc.time()[3] - time_BM;

time_MB = proc.time()[3];
N_MB <- genNormal_Marsaglia_Bray(10000);
time_MB = proc.time()[3] - time_MB;

r = N_MB[1];
N_MB <- N_MB[2:10001];

cat("Using Box-Muller method, the sample mean, and the sample variance, for different values of sample size, are calculated to be:\n");
cat("Sample size = 100 :: Mean = ", mean(N_BM[1:100]), "\t;\tVariance = ", var(N_BM[1:100]), "\n");
cat("Sample size = 500 :: Mean = ", mean(N_BM[1:500]), "\t;\tVariance = ", var(N_BM[1:500]), "\n");
cat("Sample size = 10000 :: Mean = ", mean(N_BM), "\t;\tVariance = ", var(N_BM), ".\n");

cat("\nUsing Marsaglia-Bray method, the sample mean, and the sample variance, for different values of sample size, are calculated to be:\n");
cat("Sample size = 100 :: Mean = ", mean(N_MB[1:100]), "\t;\tVariance = ", var(N_MB[1:100]), "\n");
cat("Sample size = 500 :: Mean = ", mean(N_MB[1:500]), "\t;\tVariance = ", var(N_MB[1:500]), "\n");
cat("Sample size = 10000 :: Mean = ", mean(N_MB), "\t;\tVariance = ", var(N_MB), ".\n");

pdf("N_BM100.pdf");
hist(N_BM[1:100], breaks = 50, col = "light cyan", plot = TRUE, main = "Histogram of 100 Normal_Box_Muller");
pdf("N_BM500.pdf");
hist(N_BM[1:500], breaks = 50, col = "light cyan", plot = TRUE, main = "Histogram of 500 Normal_Box_Muller");
pdf("N_BM10000.pdf");
hist(N_BM[1:10000], breaks = 50, col = "light cyan", plot = TRUE, main = "Histogram of 10000 Normal_Box_Muller");

pdf("N_MB100.pdf");
hist(N_MB[1:100], breaks = 50, col = "light cyan", plot = TRUE, main = "Histogram of 100 Normal_Marsaglia_Bray");
pdf("N_MB500.pdf");
hist(N_MB[1:500], breaks = 50, col = "light cyan", plot = TRUE, main = "Histogram of 500 Normal_Marsaglia_Bray");
pdf("N_MB10000.pdf");
hist(N_MB[1:10000], breaks = 50, col = "light cyan", plot = TRUE, main = "Histogram of 10000 Normal_Marsaglia_Bray");

##For N(0,5)
sN_BM <- sqrt(5) * sort(N_BM[1:500]);
sN_MB <- sqrt(5) * sort(N_MB[1:500]);
sN_T <- sort(rnorm(500, mean = 0, sd = sqrt(5)));
pdf("N(0,5).pdf");
#plot.ecdf(sN_BM);
#plot.ecdf(sN_MB);
#plot.ecdf(sN_T);
plot(ecdf(sN_BM), do.points = FALSE, main = "", col = "red")
par(new = TRUE)
plot(ecdf(sN_MB), do.points = FALSE, main = "", axes = FALSE, col = "green")
#plot(ecdf(sN_T), do.points = FALSE, main = "")
lines(sN_T, pnorm(sN_T, mean = 0, sd = sqrt(5)), type='l', col = "blue")
legend('topleft', legend = c('Experimental (Box-Muller method)', 'Experimental (Marsaglia-Bray method)', 'Theoretical'), lty = 1, col = c("red", "green", "blue"), bty = 'n')
title("Cumulative Distribution Function for N(0,5)");

##For N(5,5)
sN_BM <- (sqrt(5) * sort(N_BM[1:500])) + 5;
sN_MB <- (sqrt(5) * sort(N_MB[1:500])) + 5;
sN_T <- sort(rnorm(500, mean = 5, sd = sqrt(5)));
pdf("N(5,5).pdf");
#plot.ecdf(sN_BM);
#plot.ecdf(sN_MB);
#plot.ecdf(sN_T);
plot(ecdf(sN_BM), do.points = FALSE, main = "", col = "red")
par(new = TRUE)
plot(ecdf(sN_MB), do.points = FALSE, main = "", axes = FALSE, col = "green")
#plot(ecdf(sN_T), do.points = FALSE, main = "")
lines(sN_T, pnorm(sN_T, mean = 5, sd = sqrt(5)), type='l', col = "blue")
legend('topleft', legend = c('Experimental (Box-Muller method)', 'Experimental (Marsaglia-Bray method)', 'Theoretical'), lty = 1, col = c("red", "green", "blue"), bty = 'n')
title("Cumulative Distribution Function for N(5,5)");

cat("\nComputional time (elapsed time) for Box-Muller method and Marsaglia-Bray method are ", time_BM, ", and ", time_MB, "respectively.\n");
if (time_BM < time_MB) {
	cat("Box-Muller method is faster than Marsaglia-Bray method.\n");
} else {
	cat("Marsaglia-Bray method is faster than Box-Muller method.\n");
}

cat("\nFor the Marsaglia-Bray method the proportion of values rejected (in generating 10000 sample values) is ", r, ".\n");
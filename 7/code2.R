genPoisson <- function(sample, l) {
	P <- vector(length = sample);
	for (j in 1:sample) {
		u = runif(1,0,1);
		i = 0;
		p = exp(-l);
		F = p; 
		repeat {
			if (u < F) {
				P[j] = i;
				break;
			}
			else {
				p = ((l * p) / (i + 1));
				F = F + p;
				i = i + 1;
			}
		}
	}
	return (P);	
}

set.seed(1);

sample = 50;
l = 2;

P <- genPoisson(sample, l);
s_P <- sort(P);

Density <- ((exp(-l) * (l^(P))) / factorial(P));

cat("The sample mean and variance, for the", sample, "Poisson random numbers generated, are calculated to be", mean(P), ",and", var(P), "respectively.\n");

pdf("2_pmf.pdf");
#plot(P, Density, xlab = "x", ylab = "p(x) = P(X = x)", main = paste("Probability mass function\nPoisson distribution (with mean 2), n =", sample), col = "red");
plot(P, Density, xlab = "x", ylab = "p(x) = P(X = x)", main = "", col = "red");

pdf("2_CDF.pdf");
#plot(ecdf(s_P), xlab = "x", ylab = "F(x) = P(X <= x)", do.points = FALSE, main = paste("Cumulative distribution function\nPoisson distribution (with mean 2), n =", sample), col = "red");
plot(ecdf(s_P), xlab = "x", ylab = "F(x) = P(X <= x)", do.points = FALSE, main = "", col = "red");
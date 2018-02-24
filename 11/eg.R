sink("eg output.txt");

set.seed(1);

size = 5000;
n = 1000;
range_time  = 5;
dt = range_time/size;
sdt = sqrt(dt);

mu <- c(0.05, -0.05);
sigma <- c(0.25, 0.3);

S0 = 1;

for (p in 1:2)
{
for (q in 1:2)
{
S <- vector(length = size+1);
S[1] = S0;
s5 <- vector(length = n);

pdf(paste("eg plot",p,q,".pdf"));

for (i in 1:n) {
	for (j in 2:(size + 1)) {
		S[j] = S[j - 1] * exp(((mu[p] - (sigma[q]^2)/2) * dt) + (sigma[q] * sdt * rnorm(1, mean = 0, sd = 1)));
	}
	s5[i] = S[(5/dt) + 1];
	plot(seq(0, 5, dt), S, type = 'l', xlim = c(0,5), ylim = c(-1,10), col = i, verticals = FALSE, do.points = FALSE, main = "", xlab = "", ylab = "")
	par(new = TRUE)
}
title(ylab = 'S', xlab = 'Time');
legend('topright', legend = c(paste("mu =", mu[p]), paste("sigma =", sigma[q])), lty = 0, col = "white", bty = 'n');

cat("\n\nTaking mu", mu[p], "and sigma", sigma[q],", and sample size", n, "::");
cat("\nSample expectation and variance of S(5) are estimated to be", mean(s5), ", and", var(s5), ", respectively.");
cat("\nWhile, theoretical expectation and variance of S(5) are", (S0 * exp(mu[p] * 5)), ", and", (S0 * exp(2 * mu[p] * 5) * (exp((sigma[q]^2) * 5) - 1)), ", respectively.");

}
}

sink();
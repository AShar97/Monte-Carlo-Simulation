set.seed(1);

size = 5000;
n = 10;
range_time  = 5;
dt = range_time/size;
sdt = sqrt(dt);

mu = 0.06;
sigma = 0.3;

#No matrix assigment, because size will be too large.
X <- vector(length = size+1);
X[1] = 5;
x2 = 0; x5 = 0;

pdf("2.pdf");
for (i in 1:n) {
	for (j in 2:(size + 1)) {
		X[j] = X[j-1] + (mu * dt) + (sigma * sdt * rnorm(1, mean = 0, sd = 1));
	}
	x2 = x2 + (X[(2/dt) + 1]/n);
	x5 = x5 + (X[(5/dt) + 1]/n);
	plot(seq(0, 5, dt), X, type = 'l', xlim = c(0,5), ylim = c(3,7), col = i, verticals = FALSE, do.points = FALSE, main = "", xlab = "", ylab = "")
	par(new = TRUE)
}
title(ylab = 'X', xlab = 'Time');

cat("\nE[X(2)] =", x2, "\nE[X(5)] =", x5, '\n');

#E[X(2)] = 5.101632
#E[X(5)] = 5.184239
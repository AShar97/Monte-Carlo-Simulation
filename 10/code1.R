set.seed(1);

size = 5000;
n = 10;
range_time  = 5;
dt = range_time/size;
sdt = sqrt(dt);

#No matrix assigment, because size will be too large.
W <- vector(length = size+1);
W[1] = 0;
w2 = 0; w5 = 0;

pdf("1.pdf");
for (i in 1:n) {
	for (j in 2:(size + 1)) {
		W[j] = W[j-1] + (sdt * rnorm(1, mean = 0, sd = 1));
	}
	w2 = w2 + (W[(2/dt) + 1]/n);
	w5 = w5 + (W[(5/dt) + 1]/n);
	plot(seq(0, 5, dt), W, type = 'l', xlim = c(0,5), ylim = c(-5,5), col = i, verticals = FALSE, do.points = FALSE, main = "", xlab = "", ylab = "")
	par(new = TRUE)
}
title(ylab = 'W', xlab = 'Time');

cat("\nE[W(2)] =", w2, "\nE[W(5)] =", w5, '\n');

#E[W(2)] = -0.06122638 
#E[W(5)] = -0.3858699
mu <- function(j){
	return (0.0325 - (0.05*t));
}
sigma <- function(j) { 
	return (0.012 + (0.0138*t) + (0.00125*(t^2)));
}

set.seed(1);

size = 5000;
n = 10;
range_time  = 5;
dt = range_time/size;
sdt = sqrt(dt);

#No matrix assigment, because size will be too large.
Y <- vector(length = size+1);
Y[1] = 5;
y2 = 0; y5 = 0;

pdf("3.pdf");
for (i in 1:n) {
	for (j in 2:(size + 1)) {
		t = ((j - 1) * dt);
		Y[j] = Y[j-1] + (mu(t) * dt) + (sigma(t) * sdt * rnorm(1, mean = 0, sd = 1));
	}
	y2 = y2 + (Y[(2/dt) + 1]/n);
	y5 = y5 + (Y[(5/dt) + 1]/n);
	plot(seq(0, 5, dt), Y, type = 'l', xlim = c(0,5), ylim = c(4,6), col = i, verticals = FALSE, do.points = FALSE, main = "", xlab = "", ylab = "")
	par(new = TRUE)
}
title(ylab = 'Y', xlab = 'Time');

cat("\nE[Y(2)] =", y2, "\nE[Y(5)] =", y5, '\n');

#E[X(2)] = 4.96492
#E[X(5)] = 4.499891
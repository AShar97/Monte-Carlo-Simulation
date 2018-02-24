set.seed(1);

size = 5000;
n = 10;
range_time  = 5;
dt = range_time/size;
sdt = sqrt(dt);

W <- matrix(0, nrow = (size + 1), ncol = 10);

for (i in 1:n) {
	for (j in 2:(size + 1)) {
		W[j][i] = W[j - 1][i] + (sdt * rnorm(1, mean = 0, sd = 1));
	}
}

pdf("1c.pdf");
for (i in 1:n) {
	plot(seq(0, 5, dt), W[,i], type = 'l', xlim = c(0,5), ylim = c(-5,5), col = i, verticals = FALSE, do.points = FALSE, main = "", xlab = "", ylab = "")
	par(new = TRUE)
}
title(ylab = 'W', xlab = 'Time');
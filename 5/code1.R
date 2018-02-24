f<-function(x) {
	return (exp(-(1 / 2) * ((abs(x) - 1) ^ 2)));
}

genNormal<-function(sample) {
	N<-vector(length = sample);

	set.seed(1);

	j = 0;
	for (i in 1:sample) {
		repeat {
			j = j + 1;
			u<-runif(2, 0, 1);
			if (u[1] < (1/2)) {
				x = log(2 * u[1]);
			}
			else {
				x = -log(2 * (1 - u[1]));
			}
			if (u[2] <= f(x)) {
				N[i] = x;
				break;
			}
		}
	}

	png("1.png");
	hist(N, breaks = 50, col = "light cyan", plot = TRUE);

	return(c(mean(N), var(N), (sample / j)));
}

output<-genNormal(1000);

cat("The sample mean, sample variance, and the simulated acceptance probability are calculated to be", output[1], ",", output[2], ", and", output[3], "respectively.\n");

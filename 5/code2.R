f<-function(x) {
	return (exp(-(1 / 2) * ((x - 1) ^ 2)));
}

genHalf_Normal<-function(sample) {
	H_N<-vector(length = sample);

	set.seed(1);

	j = 0;
	for (i in 1:sample) {
		repeat {
			j = j + 1;
			u<-runif(2, 0, 1);
			x = - log(1 - u[1]);
			if (u[2] <= f(x)) {
				H_N[i] = x;
				break;
			}
		}
	}

	png("2.png");
	hist(H_N, breaks = 50, col = "light cyan", plot = TRUE);

	return(c(mean(H_N), var(H_N), (sample / j)));
}

output<-genHalf_Normal(1000);

cat("The sample mean, sample variance, and the simulated acceptance probability are calculated to be", output[1], ",", output[2], ", and", output[3], "respectively.\n");

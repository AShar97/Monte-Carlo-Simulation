P_inv<-function(x) {
	if (x < 0.05) {return(1);}
	else if (x < 0.3) {return(2);}
	else if (x < 0.75) {return(3);}
	else if (x < 0.9) {return(4);}
	else {return(5);}
}

genDiscrete<-function(sample) {
	D<-vector(length = sample);

	set.seed(1);

	for (i in 1:sample) {
		D[i] = P_inv(runif(1, 0, 1));
	}

	png("3_1.png");
	hist(D, breaks = 5, col = "light cyan", plot = TRUE);

	return(c(mean(D), var(D)));
}

output<-genDiscrete(10);

cat("The sample mean, and variance are calculated to be", output[1], ", and", output[2], "respectively.\n");

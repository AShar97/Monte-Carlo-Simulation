p<-function(x) {
	if (x == 1) {return(0.05);}
	else if (x == 2) {return(0.25);}
	else if (x == 3) {return(0.45);}
	else if (x == 4) {return(0.15);}
	else {return(0.1);}
}

genDiscrete<-function(sample) {
	D<-vector(length = sample);

	set.seed(1);

	for (i in 1:sample) {
		repeat {
			u<-runif(2, 0, 1);
			x = (floor(5 * u[1]) + 1);
			if (u[2] <= (p(x)/0.45)) {
				D[i] = x;
				break;
			}
		}
	}

	png("3_2.png");
	hist(D, breaks = 5, col = "light cyan", plot = TRUE);

	return(c(mean(D), var(D)));
}

output<-genDiscrete(10);

cat("The sample mean, and variance are calculated to be", output[1], ", and", output[2], "respectively.\n");

genExp<-function(sample, mean) {
  E<-vector(length = sample);

  set.seed(1);
  u<-runif(sample, 0, 1);

  lambda = 1 / mean
  E = ((-1 / lambda) * log(1 - u));

  png("1.png");
  hist(E, breaks = 50, col = "light cyan", plot = TRUE);

  return(c(mean(E), max(E), min(E)));
}

output<-genExp(5000, 5);

cat("The mean, maximum, and minimum are calculated to be", output[1], ",", output[2], ", and", output[3], "respectively.\n");

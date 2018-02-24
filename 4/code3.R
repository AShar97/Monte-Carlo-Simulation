f<-function(x) {
  return (20 * x * ((1 - x)^3));
}

genF<-function(sample) {

  F<-vector(length = sample);

  c = (135 / 64);

  set.seed(1);

  for (i in 1:sample) {
    repeat {
      u<-runif(2, 0, 1);
      if(u[2] <= (f(u[1])/c)){
        F[i] = u[1];
        break;
      }
    }
  }

  png("3.png");
  hist(F, breaks = 50, col = "light cyan", plot = TRUE);

  return(c(mean(F), var(F), max(F), min(F)));
}

output<-genF(5000);

cat("The mean, variance, maximum, and minimum are calculated to be", output[1], ",", output[2], ",", output[3], ", and", output[4], "respectively.\n");

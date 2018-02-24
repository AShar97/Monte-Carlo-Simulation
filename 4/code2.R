genGamma<-function(sample, n, lambda) {
  G<-vector(length=sample);

  set.seed(1);

  for (i in 1:sample) {
    u<-runif(n, 0, 1);
    G[i] = prod(u);
  }

  G = (-1 / lambda) * log(G);

  png("2.png");
  hist(G, breaks = 50, col = "light cyan", plot = TRUE);

  return(c(mean(G), max(G), min(G)));
}

output<-genGamma(5000, 5, 5);

cat("The mean, maximum, and minimum are calculated to be", output[1], ",", output[2], ", and", output[3], "respectively.\n");

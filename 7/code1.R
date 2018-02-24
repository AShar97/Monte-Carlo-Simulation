genGeometric <- function(sample, p) {
	u <- runif(sample,0,1);
	G <- ceiling(log(u)/log(1-p));
	return (G);
}

set.seed(1);
p = runif(1,0,1);	#Taking value of 'p', i.e. probability for success in a trial .

sample = 50;

G <- genGeometric(sample, p);	#Generating Geometric Random Numbers.
Density <- p * ((1 - p)^(G - 1));

cat("The value of p taken is",p,".\n");
cat("The sample mean and variance, for the", sample, "random numbers generated from Geometric distribution, with parameter p =", p, ", are calculated to be", mean(G),",and",var(G),"respectively.\n");

pdf("1.pdf");
#plot(G, Density, xlab = "x", ylab = "p(x)", main = paste("Probability mass function\nGeometric distribution, n =", sample), col = "red");
plot(G, Density, xlab = "x", ylab = "p(x) = P(X = x)", main = "", col = "red");
legend("topright", legend = paste("Parameter", p), lty = 0, col = "red", bty = 'n');
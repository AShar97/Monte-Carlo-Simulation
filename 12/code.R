lcg <- function(a, b, m, seed, size) {
	x <- vector(length = size);
	x[1] = seed;
	for (i in 2:size) {
		x[i] = (((a * x[i-1]) + b) %% m);
	}
	return(x/m);
}

reverse_base <- function(number, base) {
	n = number;
	result <- vector();
	while (n != 0) {
		result = c(result, n %% base);
		n = n %/% base;
	}
	return(result);
}

gen_Van_der_Corput <- function(number, base) {
	n <- reverse_base(number, base);
	result = 0;
	for (i in 1:length(n)) {
		result = result + (n[i] / (base^i));
	}
	return(result);
}

## Problem - 1 ##

###
v25 <- vector(length = 25);
for (i in 1:25) {
	v25[i] = gen_Van_der_Corput(i, 2);
}

cat(v25, "\n");

rm(v25);

###
v1000 <- vector(length = 1000);
for (i in 1:1000) {
	v1000[i] = gen_Van_der_Corput(i, 2);
}

pdf("pair_vdc.pdf");
plot(v1000[1:999], v1000[2:1000], type = "p", xlab = "x(i)", ylab = "x(i+1)", main = "", pch = '.');

rm(v1000);

###
v100 <- vector(length = 100);
for (i in 1:100) {
	v100[i] = gen_Van_der_Corput(i, 2);
}

v100000 <- vector(length = 100000);
for (i in 1:100000) {
	v100000[i] = gen_Van_der_Corput(i, 2);
}

l100 <- lcg(16807, 0, 2^31 - 1, 1, 100);

l100000 <- lcg(16807, 0, 2^31 - 1, 1, 100000);

pdf("v100.pdf");	hist(v100, breaks = 100, xlab = "X", main = "");
pdf("v100000.pdf");	hist(v100000, breaks = 100, xlab = "X", main = "");
pdf("l100.pdf");	hist(l100, breaks = 100, xlab = "X", main = "");
pdf("l100000.pdf");	hist(l100000, breaks = 100, xlab = "X", main = "");

rm(v100, v100000, l100, l100000, lcg);

## Problem - 2 ##

x100 <- matrix(nrow = 100, ncol = 2);
for (i in 1:100) {
	x100[i,] <- c(gen_Van_der_Corput(i, 2), gen_Van_der_Corput(i, 3));
}

x100000 <- matrix(nrow = 100000, ncol = 2);
for (i in 1:100000) {
	x100000[i,] <- c(gen_Van_der_Corput(i, 2), gen_Van_der_Corput(i, 3));
}

pdf("x100.pdf");	plot(x100, xlab = "phi_2(i)", ylab = "phi_3(i)", main = "", pch = '.');
pdf("x100000.pdf");	plot(x100000, xlab = "phi_2(i)", ylab = "phi_3(i)", main = "", pch = '.');

rm(list = ls());

#0.5 ; 0.25 ; 0.75 ; 0.125 ; 0.625 ; 0.375 ; 0.875 ; 0.0625 ; 0.5625 ; 0.3125 ; 0.8125 ; 0.1875 ; 0.6875 ; 0.4375 ; 0.9375 ; 0.03125 ; 0.53125 ; 0.28125 ; 0.78125 ; 0.15625 ; 0.65625 ; 0.40625 ; 0.90625 ; 0.09375 ; 0.59375
#This was an adaptation of a Python version of a program originally written in Matlab for data analysis during my PhD thesis.
#It is used for hypothesis testing using small data sets, specifically the hypothesis that the average of the second set is
#greater than that of the first set, with the median used as the metric.  The test is completely numerical and makes no assumptions
#about the underlying distribution of the data.  If the null hypothesis, that the two data samples come from the same population with
#the same median, is true, the assignment of data points to each set is effectively random, and any difference in the median
#is random noise.  THe basic principle of the function is to created simulated data replicates under the assumption of the null
#hypothesis by randomly reassigning each data point to the first and second sets, keeping the set sizes constant, and measuring
#the simulated difference in medians for each replicate.  By comparing the observed difference in medians to the distribution
#of simulated differences in medians, the p value for rejection of the null hypothesis can be read out directly.

shuffletest <- function(x,y){ #assume inputs to be vectors up front
	trials <- 500000 #minimum detectable p value is 1/trials
	Nx <- length(x)
	Ny <- length(y)
	z <- c(x,y)
	Nz <- Nx + Ny
	m <- median(y) - median (x)
	M <- vector(mode="numeric", length=trials)
	for (i in 1:trials) {
	sim <- sample(z) #sample picks a random sample without replacement, and default output length is input length, so effectively random permutation
	M[i] <- (median(sim[(Nx+1):Nz]) - median (sim[1:Nx]))
	}
	p <- sum(M>=m)/trials
	hist(M,breaks=50,col="blue")
	points(c(m),c(0),pch=19,col="red")
	return(p) 
}

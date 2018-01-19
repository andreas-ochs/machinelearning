# Indicative examples used in the class 
# Data Analytics crash course, University of Edinburgh, January 2018

# (c) A. Tsanas, January 2018

# (c) A. Tsanas, September 2016

#=====================================================================================================================
#for any comments please email: atsanas@ed.ac.uk OR tsanasthanasis@gmail.com
#=====================================================================================================================
#
rm(list=ls()) # remove all variables from workspace, *** remember to clear screen: Ctrl+L ***

# Example 1: See for yourself the statistical power in terms of sample size
# Draw random data from the normal distribution and compute statistics
x <- rnorm(100, mean=0, sd=1) # N(0,1), randomly drawing 100 samples
summary(x)
plot(density(x)); grid(nx=NULL, ny=NULL, col='red')

x <- rnorm(1000, mean=0, sd=1) # N(0,1), randomly drawing 1000 samples
summary(x)
plot(density(x)); grid(nx=NULL, ny=NULL, col='red')

x <- rnorm(1000000, mean=0, sd=1) # N(0,1), randomly drawing 1000000 samples
summary(x)
plot(density(x)); grid(nx=NULL, ny=NULL, col='red')

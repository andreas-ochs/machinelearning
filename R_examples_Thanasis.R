# Indicative program to see the effects of various functions using examples
# You will see next to each command the comments about its effect

# In the first few parts below, I have introduced some of the main concepts in R, to introduce you to the commands.
# Subsequently, I have organised the sections in the order I would go about trying to understand and solve a problem, following the Lab notes

# (c) A. Tsanas, September 2016

#=====================================================================================================================
#                         *** Introduction and starting creating a program ***

rm(list=ls()) # remove all variables from workspace, *** remember to clear screen: Ctrl+L ***

install.packages("matrixStats") # reminder for how to download a package from the command line; you can also do it by doing "Packages -> Install" in the bottom right hand-side window

# A fairly generic list of packages to download and install is the following:
#install.packages(packageList, dependencies=TRUE)
 

# library(XYZ) #reminder holder to load a specific library XYZ for your program if you're using some commands from those libraries

                #########
                # DAY 1 #
                #########

#=====================================================================================================================
#  *** Create a vector x with 1000 samples, mean = 0 and standard deviation = 1., and compute statistical properties ***
# in practice you would have some data to work on, but just to get you started let's go on with artificial data
x <-rnorm(1000,0,1) # the function 'rnorm' will generate samples from a Gaussian (normal) distribution
#N(0,1)

summary(x)

plot(density(x))

#increase sample size
x.larger <- rnorm(10000,0,1)
summary(x.larger)
plot(density(x.larger))

x.larger2 <- rnorm(1000000,0,1)
summary(x.larger2)
plot(density(x.larger2))

# start the exploration
View(x) # put the vector on the left hand-side panel to visualize its contents
plot(x) # visualize the data in a graph
hist(x) # see the histogram of the data
plot(density(x)) # see the density of the data
density(x) # see characteristics of the data
mean(x) # find the mean
sd(x) # find the standard deviation
summary(x) # summary statistics for the vector x

#now, there is no function to compute the mode... no problem, let's build that function!
getmode <- function(v) # note the characteristic method of constructing a function, 'v' here is the input variable
  {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
# the value returned by the function is the result of the last expression that was evaluated within the function;
# alternatively, you could use "return()" to indicate which variables would be returned from the function evaluation

#now that the 'getmode' function is defined, let's compute the mode
getmode(x) # note of course that this is only really useful when you have discrete data

# if you had defined the function externally, ensure this is in R's seach path: source("getmode.R")

#incidentally the internal function 'mode' in R gives you the type of the variable that you have, e.g. try "mode(x)"

#=====================================================================================================================
#                               *** Let's see how things work with vectors *** 

x <- c(1:10); y <- c(2:11); z <- x+y; z # same length vectors, verify the result
x <- c(1:10); y <- c(1:6); z <- x+y; z # different length vectors... what is happening? Be careful about this!
# Remember: recycling rule! This is different compared to other programming languages you may have been used to
x <- c(1:10); y <- c(1:3); z <- x+y; z # one more example, but this time you will see a warning appearing

#=====================================================================================================================
#                                       *** Create different objects *** 
rm(list=ls()) # remove all variables from workspace
x <- c(1:10) # a vector with components 1,2,...10
x <- c(1, 4, -5, 10, 11, -100) #a vector with some random values I assigned
my_list <- list(id=1234, name='Thanasis', gender='male') # create a list, various components can be of different types

#now verify what the difference is between my_list[1] and my_list[[1]]
my_list[1] # extracts the first component
my_list[[1]] # notice the double "[[]]"! extracts the value of the first component
my_list$id # alternative way to extract the value of the component with name 'id'
my_list[["id"]] # yet another way to extract the value of the component with name 'id', notice the familiar double "[[]]"

# Now, let's add some additional components to the list, say my GPA
my_list # verify what's happening and which components are part of the 'my_list' variable (the 'GPA' variable was added)

# Another very useful type is the 'Data frame', that's how the data from loading an Excel file are stored in R, as we will see later.
# let's create a data frame with some random data
myData <- data.frame(std.normal=rnorm(1000, m=0, sd=1),
            wide.normal=rnorm(1000, m=0, sd=2),
            exponent=rexp(1000, rate=1),
            uniform=runif(1000, min=-3, max=3)
          )


                #########
                # DAY 2 #
                #########

#=====================================================================================================================
#                           *** Work with a matrix and see how things go *** 

# first load the data programmatically (or via the 'files' or 'Import Dataset' as discussed in the Lab Notes)
library(readxl) # method to load a library (we will use some functions belonging to the library)
# ElectricityConsumption <- read_excel("C:/TEACHING/Statistical Research Methods/Excel/ElectricityConsumption.XLS") # of course, you'll need to adapt this to be your filename
ElectricityConsumption <- read_excel("~/Google Drive/THANASIS_sync_files/TEACHING/Oxford/Statistical Research Methods (SBS, 2016)/Excel/ElectricityConsumption.xls")
X <-data.matrix(ElectricityConsumption) # converting the data frame data into a matrix format
X2<-X[-(104:120), 1:7] # let's get rid of the NA values which complicate the task

dim(X2) # find the size of the matrix, i.e. the dimensions
nrow(X2) # number of rows of the matrix
ncol(X2) # number of columns of the matrix

Xnames <- colnames(X2) # access the column names (these are the variable names in the matrix)


                #########
                # DAY 3 #
                #########

#=====================================================================================================================
#                                   NOW GET TO THE MAIN PART OF ANALYSING YOUR DATA!
#=====================================================================================================================
#                                 *** Visualizing data: boxplots and scatterplots *** 

# Let's plot the second variable of the matrix X2 as a function of the fourth variable of the matrix X2
plot(X2[,4], X2[,2]) # plot dependent variable X2[,2] as a function of the independent variable X2[,1]
title(main = 'Scatter plot', xlab = Xnames[4], ylab = Xnames[2]) # Insert main title and axis labels... but do you see the problem with the labels?
plot(X2[,4], X2[,2], xlab = Xnames[4], ylab = Xnames[2], main = 'Scatter plot') # Creating the labels directly when creating the plot

boxplot(X2[,2]) # univariate boxplot of the second variable in X2

library(sfsmisc) # load required library for boxplot.matrix
boxplot.matrix(X2[,3:7]) # create the boxplots for the 3rd - 7th features in X2

# now work with a bivariate boxplot
library(aplpack) # load required library for bagplot
bagplot(X2[,4], X2[,2], xlab = Xnames[4], ylab = Xnames[2], main = "Bagplot of ELEC = f(C76)")

#=====================================================================================================================
#                                   *** Correlation analysis ***

colMeans(X2, na.rm = TRUE) # get the means of each column of a matrix
library(matrixStats) # load the library in memory for the following command
colSds(X2) # compute the standard deviation of each column in X2

for (i in 1:7){
  A[i] = mean(X2[,i])
  B[i] = sd(X2[,i])  
  
}
  
cor(X2, use="complete.obs", method="pearson") # compute the linear (Pearson) correlation coefficient for all pairwise combinations in the matrix X2
cor(X2, use="pairwise.complete.obs", method="pearson") # the same as above, but would remove missing value (NA) if you had them still in your matrix

cor(X2, X2[,2]) # find the correlation coefficient for each of the variables in X2 with a variable (which here I selected to be the second variable in X2; in practice this would be your response variable y)

cov(X2, use="complete.obs") # Compute the covariance matrix

# Now let's say you also want to get the statistical significance of the computed correlations.
# The default R does not have that, but there is a package that can be used to help us do that.
# You will need to install the package 'HMisc'. Check the Lab notes for a reminder on how to install a package, install it and then run the following
# Correlations with significance levels
library(Hmisc) # to include the library and use its functions (here the function 'rcorr')
rcorr(X2, type="pearson") # you can now see both the correlations and also the statistical significance (p-values)

#if you have a data frame and you want to work directly with the command, do: rcorr(as.matrix(X2))

#=====================================================================================================================
#                                   *** Statistical hypothesis testing *** 

# There are various statistical hypothesis tests, as discussed in the Lecture notes. The applicability of each test
# depends on the properties of the data, e.g. some tests require the distributions are normal.
# A simple generic test is the Wilcoxon ranksum test, which does not make such rigid assumptions (but requires the density to be continuous):
wilcox.test(X2[,3], X2[,4]) # RR!! how do you interpret the results appearing on the screen?
ks.test(X2[,3], X2[,4]) # Two sample Kolmogorov-Smirnov test

# you may also want to visualize the two densities in the same plot to verify the result
d1 <- density(X2[,3]) # get the density of the third variable in X2
d2 <- density(X2[,4]) # get the density of the fourth variable in X2
plot(range(d1$x, d2$x), range(d1$y, d2$y), type = "n", xlab = "x",ylab = "Density") # prepare the plot with the range of the values for both variables
lines(d1, col = "red"); lines(d2, col = "blue") # including the actual densities on the plot
# there are some alternatives to visualize the densities of two variables in the same plot,
# but you have to be careful because often they require that the two vectors have equal length which is not necessarily always the case!



          #########
          # DAY 4 #
          #########


#=====================================================================================================================
#                     *** Feature selection and Feature transformation placeholder *** 
#                           NOT Covered in the programming part of this course!




            #########
            # DAY 5 #
            #########




#=====================================================================================================================
#                       *** Statistical mapping: Regression and Classification *** 

# Let's start with the simple Ordinary Least Squares (OLS) Linear Regression

y <- X2[,2] # let's set up the ELEC variable to be the response
y <- matrix(X2[,2]) # if you want your response to appear as a 'matrix' so that you can see it in the left handside top panel
X <-X2[,-1:-2] # setting the variables in X2 except the first and the second, to comprise the design matrix Xregression

fitted_model_simple <- lsfit(X[,2], y, yname = Xnames[2]) # Fit an OLS model f(X) = y, using ONLY the second variable in X2, i.e. C76
fitted_model_simple$coefficients # look at the model coefficients, what can you say about your model?

fitted_model <- lsfit(X, y, yname = Xnames[2]) # Fit an OLS model f(X) = y
fitted_model # look at what the fitted model looks like
fitted_model$coefficients # observe the coefficients associated with each feature (explanatory variable)
fitted_model_no_intercept <- lsfit(X, y, intercept = FALSE, yname = Xnames[2]) # Same, but without using an intercept

# Alternative to get the linear regression working
D <- data.frame(X, y) # put things conveniently in a data frame
new_model <- lm(y~X, data = data.frame(X))
new_model

new_model2 <- lm(y~C66+C76+H55+DINC+AIRC, data = D) # verify also that this alternative way is the same as above in terms of the coefficients!
summary(new_model2)
# Verify that the results you obtained in terms of the OLS coefficients are identical from "lm" and "lsfit"!
plot(new_model2) # nice way to see some outcomes of the model
anova(new_model2) # apply Analysis of variance (ANOVA)
final_model <- step(new_model2)
summary(final_model)


# Introduce training and testing loops

for(i in 1:100){
  training_samples = sample(1:103,90)
  testing_samples = setdiff(1:103, training_samples)
  
  Xtrain = X[training_samples,]
  ytrain = y[training_samples]
  
  Xtest = X[testing_samples,]
  yest = y[testing_samples]
  
  fitted_model_iteration <- lsfit(Xtrain, ytrain, yname = Xnames[2]) # Fit an OLS model f(X) = y
  fitted_model_iteration$coefficients # observe the coefficients
}




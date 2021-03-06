##
## ## Ch3_SimulateBeta_RCode.R
##
##

## Set model and simulation parameters
Obs       = 1000		# Number of observations in each simulation
Reps      = 150		# Number of times we run the simulation
TrueBeta0	= 12000	# "True" beta0 for the simulated
TrueBeta1	= 1000	# "True" beta1 for the simulated
SD 		    = 10000	# The standard deviation of the error. The bigger this is, the larger the average value of epsilon
Ed = 16 * runif(Obs)# Simulate years of education as being between 0 and 16
  # "runif" is a uniform random variable between 0 and 1, with all values having equal probability
CoefMatrix	= matrix(NA, Reps, 2)	# Matrix to store our results.  
  # 1st argument is NA, meaning we store "not available" as initial values in the matrix
  # 2nd argument is Reps, meaning the number of rows is equal to number of times we run the simulations
  # 3rd argument is 2 meaning we have 2 columns, one for storing the beta0 estimate and one for storing the beta1 estimate

# Loop: repeat the commands between the brackets multiple times
for (ii in 1:Reps) { 
  Salary 	= TrueBeta0+ TrueBeta1* Ed + SD*rnorm(Obs) 	
  # Generate Salary = beta0 + beta1*Ed + epsilon
  # beta0 is the constant
  # beta1 is the number multiplied by the X variable
  # Epsilon has 2 parts: SD is the standard deviation; the bigger it is, the more epsilon varies.
  # "runif" is a uniform random variable between 0 and 1, with all values having equal probability
  OLS.result = lm(Salary ~ Ed) # Run a regression using simulated values of Y
  CoefMatrix[ii, ]	= coefficients(OLS.result)	 # Put OLS.result coefficients in row ii of CoefMatrix
  ## For fun: plot results for each survey
  ## plot(Ed, Salary, pch = 19, col= "darkgreen")		
  ## abline(OLS.result, lwd = 3, col= "darkgreen")
  ## Sys.sleep(0.075)		## Include to slow down calculations so we can see each plot (briefly); not necessary
}							 # This closes the "loop"

c(mean(CoefMatrix[,1]), min(CoefMatrix[,1]), max(CoefMatrix[,1]))  
# Average, min and max of beta_0 estimates

c(mean(CoefMatrix[,2]), min(CoefMatrix[,2]), max(CoefMatrix[,2]))  
# Average, min and max of beta_1 estimates

#
# For use in Chapter 3, #2 part (g):
#
# Kernel Density Plot
plot(density(CoefMatrix[,2]), main = 'Kernel Density Estimate')
abline(v = apply(CoefMatrix, 2, mean)[2]) #       # Solid line is average of estimated coefficients
abline(v = TrueBeta1, lty = 2)      # Dashed line is true value


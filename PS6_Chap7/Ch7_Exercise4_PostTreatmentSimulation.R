##
## Post treatment variable simulations
## 

##
## Code for parts (a) and (b)
##

## Directions: to change the value of Gamma1, Alpha or Gamma2, change the value on lines below and re-run the code.

SimCount	= 50			## Number of simulations
N 		= 1000				## Sample size
X1 		= rnorm(N)		## Independent variable of interest (which we create as a normally distributed random variable)
Gamma1	= 1.0				## Direct effect of X1 on Y
Alpha		= 1.0				## Effect of X1 on X2
Gamma2	= 1.0				## Effect of X2 on Y
X2 		  = Alpha*X1 + rnorm(N)		            ## Post-treatment variable
Y 		  = Gamma1*X1 + Gamma2*X2 + rnorm(N)	## Dependent variable a function of X and post-treatment variable

##	(a) Show results when X1 is only independent variable
  OLS.A = lm(Y ~ X1)					## Regression model with X1 only
  summary(OLS.A)

##	(b) Show results when X1 and X2 (post-treatment) are independent variables 
  OLS.B = lm(Y ~ X1 + X2) 				## Regression model with X1 and X2
  summary(OLS.B)

##
## Code for parts (d) and (e)
##

N 		= 1000			## Sample size
X1 	  = rnorm(N)	## Independent variable of interest
Alpha	= 1		      ## Effect of X1 on X2
Rho1	= 1		      ## Effect of U on X2
Rho2	= 1		      ## Effect of U on Y
U 	  = rnorm(N)	## Unobserved potential confounder variable U
X2 	  = Alpha*X1 + Rho1*U + rnorm(N)  ## X2 is a function of X1 and U and an error term
Y 	  = Rho2*U + rnorm(N)             ## Y is only a function of U and an additional error term

##	(d) Show results when X1 is only independent variable 
	## Note that X1 has no effect on Y in Figure 7.8 (and the simulation above)
  OLS.X1only = lm(Y ~ X1) 			
	summary(OLS.X1only)

##	(e) Show results when X1 and X2 (post-treatment) are independent variables 
	OLS.X1X2 = lm(Y ~ X1 + X2) 			
	summary(OLS.X1X2)


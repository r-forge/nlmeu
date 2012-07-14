Nonlinear Regression Models 

The nls() function (from stats) as well as the package minpack.lm allow the solution of nonlinear least squares problems. 

Correlated and/or unequal variances can be modeled using 
the gnls() function of the nlme package and the 
nlreg. 



#### Extract results from nlsFit
nlsFit <- nlsFit3c  # nlsFit1, nlsFit2
summary(nlsFit)
coef(nlsFit) 
confint(nlsFit)

fitted(nlsFit)
resid(nlsFit)
vcv <- vcov(nlsFit)
cov2cor(vcv)
plot(nlsFit) # Standardized residuals vs fitted values ??

###--- Predicted values and plot based on nlsFit3a, b, c (manually)
tm     <-  seq(0,150)             # Needs to start at 0
pars   <-  coef(nlsFit3a)         # Estimated model parameters extracted from the fit
V      <-  as.numeric(pars["V"])   
state  <-  c(Y = Dose0/V)    # State variable Y at t=0
odeY   <- ode(y=state,times=tm, func=pk1eq, parms=pars)

##---  Methods for odeY an object of deSolve class
plot(odeY, type="l") # Simple plot for predicted values
# print(odeY)
hist(odeY) # not too much value in this plot
#image(deSolve)

## More advanced plot
prd    <- odeY[,"Y"]              # Including time=0

require(lattice)

myPanel <- function(x,y,...){
    panel.xyplot(x,y, type="l",...)
    x. <- pk1$x
    y. <- pk1$y1
    #print(x.)
    #print(y.)
    panel.xyplot(x.,y.,...)
}

xyplot(prd~tm, panel=myPanel)


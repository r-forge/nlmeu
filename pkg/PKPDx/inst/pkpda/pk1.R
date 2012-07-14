###############################################
### PK1 - One-compartment iv bolus dosing
##  Analysis of data  for one subject:  y1. 
##  For remaining subjects replace y1 with y2,y3,y4 in the statement below.
###############################################

##  x is time ranging from 10 to 150 minutes
##  y1, y2, y3, y4 contain drug concentrations for four subjects
names(pk1dt)
head(pk1dt)


## List conatins 3 components. Variable x is split into two parts.
pk1List <- with(pk1dt, list(x1 = x[1], x.1 = x[-1], y = y1))         # See nls documentation


## ===>  Approach 1. "Clearance model" for a selected subject. Cl, V parameterization.
## -- A1a: With gradient. 

Dose0 <- 10000

## Obtain symbolic/analytic derivatives

pk1M1 <-                             # Function returns value and gradient for model function
   deriv(~ (Dose0/V)*exp(-(Cl/V)*x), # Model function definition
         c("Cl","V"),                # Derivatives wrt Cl and V
         function(x, Cl, V){})       # Model function has three arguments: x, Cl, and V


## print(pk1M1) 

pk1F1 <- function(resp, x1, x.1, Cl, V){  # Returns observed - predicted (and gradient)
     x     <- c(x1, x.1)                  # x recreated
     pred  <- pk1M1(x, Cl, V) 
    .grad  <- attr(pred, "gradient")
     difx  <- resp - pred 
     attr(difx, "gradient") <- -.grad
     difx
}


## --- With gradient:
nlsFit1 <- 
   nls( ~ pk1F1(y, x1, x.1, Cl, V), 
       data  = pk1List, 
       start = list(Cl = 1, V = 10),
       trace = TRUE
) 
summary(nlsFit1)

## -- A1b: Without gradient (to compare):

nlsFit1x <- 
   nls( y1 ~ (Dose0/V) *exp(-(Cl/V)*x), 
       data = pk1dt, 
       start = list(Cl = 1, V = 10), 
       trace = TRUE) 
 
summary(nlsFit1x)

## ====>  Approach 2.  "Library model #1" for a selected subject. V, K parametrization
## --- A2a:  With gradient.  

## - Preparatory step. Analytical derivatives.
pk1M2 <- deriv(~(Dose0/V)*exp(-K*x), 
          c("V","K"),
           function(x, V, K){})

pk1F2 <- function(resp, x1, x.1, V, K){     # Returns observed - predicted (and gradient)
     x     <- c(x1, x.1)                    # x recreated
     pred  <- pk1M2(x, V, K) 
    .grad  <- attr(pred,"gradient")
     difx  <- resp - pred 
     attr(difx, "gradient") <- -.grad
     difx
}

## --- With gradient using pk1F2: Number of iterations= 2
nlsFit2 <-
   nls( ~ pk1F2(y, x1, x.1, V, K), 
       data = pk1List, 
       start = list(V = 10, K = 0.01)) 
summary(nlsFit2)

## --- A2b:  Without gradient: Number of iterations = 2
nlsFit2x <- nls( y1 ~ (Dose0/V) *exp(-K*x), 
        data = pk1dt, 
        start = list(V = 10, K = 0.01)) 
summary(nlsFit2x)
#====>  Approach 3. "Differential equation model for a selected subject"

library(deSolve)


##--  Function pk1eq defines ODE model equations
pk1eq <- function(t, state, parameters, ...) { # state and parameters are named vectors 
 with(
  as.list(c(state, parameters)),  
{
 K <- Cl/V
 dY <- -K*Y 
 list(c(dY))
}) 
} # end pk1eq

pk1eq(0, state = c(Y = 1000), parameters = c(Cl = 0.1, V = 10))  # dY/dt at t = 0 

## ---  A3a: Numerical derivatives only

pk1Ma <- function(tm, Cl, V){  # Returns predicted values
  tmx    <- c(0, tm)           # time = 0 added
  pars   <- c(Cl = Cl, V = V)  # Named vector: Cl and V are parameters.
  state  <- c(Y  = Dose0/V)    # State at time = 0. Y is used as a label.
  ode(y = state, times = tmx, func = pk1eq, parms = pars)  # ODE integration
}


## -- Plotting solved equation and observed values
xi  <- seq(0, 150)
prd1 <- pk1Ma(xi, Cl = 0.1, V = 10)  

## traditional graphics
prd1dt <- as.data.frame(prd1)
names(prd1dt)
plot(prd1dt, type = "l")
abline (v = seq(0, 150, by = 50), h = seq(200, 1000, by= 200)) 
points (pk1dt$x, pk1dt$y1)

## trellis graphics
library(lattice)

myPanel <- function(x, y, ...){
  panel.grid(h = -1, v = -1)
  x1 <- pk1dt$x
  y1 <- pk1dt$y1
  panel.xyplot(x1, y1)
  panel.xyplot(x, y, type = "l")
}
xyplot(Y ~ time, panel = myPanel, data = prd1dt)


pk1Fa <- function(yvar, x, Cl, V){ # Returns: Observed minus predicted
  odeY   <- pk1Ma(x, Cl, V)   # 
  prd    <- odeY[,"Y"]        # Including time=0
  res    <- yvar - prd[-1]    # Excluding time=0
  res
}

## nls fit: 7 iterations
nlsFit3a <- nls(
          ~pk1Fa(y1, x, Cl, V), 
          data = pk1dt,
          start = list(V = 10, Cl = 1)
) 

summary(nlsFit3a)

## ---  A3b: User defined Jacobian for ode.  7 iterations 
##  pk1eq defined earlier

pk1eqjac <- function(t, state, parameters){
 # print(parameters)
 with(
  as.list(c(state, parameters)),
{
 print(0)
 K   <- Cl/V
 jac <- matrix(-K, 1, 1)                       #  Verify whether K or -K. 1 by 1 matrix
 return(jac)
})
}

pk1eqjac(0, state = c(Y = 1000), parameters = c(Cl = 0.1, V = 10))  #  t = 0 


pk1Mb    <- function(tm, Cl, V){  # Returns predicted values
  tmx    <- c(0, tm)              # time=0 added
  pars   <- c(Cl = Cl, V = V)
  state  <- c(Y  = Dose0/V)       # State at time=0
  ode(y = state, 
      times = tmx, 
      func = pk1eq, 
      parms = pars, 
      jacfunc = pk1eqjac,        # User defined jacobian
      jactype = "fullusr") 
}

pk1Fb <- function(yvar, x, Cl, V){ # Returns: Observed minus predicted
  odeY   <- pk1Mb(x,Cl,V)
  prd    <- odeY[,"Y"]        # Including time=0
  res    <- yvar - prd[-1]    # Excluding time=0
  res
}

nlsFit3b <- nls(
           ~pk1Fb(y1,x,Cl,V), 
           data = pk1dt,
           start= list(V = 10, Cl = 1)
) 

## ---  A3c: Sensitivity eqs --------

pk1seq <- function(t, state, parameters) { 
 with(
  as.list(c(state, parameters)),
{
 
 dY   <- - Cl*Y/V
 dY1  <- -(Y + Cl*Y1)/V           #   vs Cl
 dY2  <- Cl*(Y/V -Y2)/V           #   vs V
 list(c(dY, dY1, dY2))
 }) 
} # end pk1eq


pk1Mc <- function(tm, Cl, V){  # Returns predicted values and gradient
  tmx     <- c(0,tm)            # time=0 added
  pars    <- c(Cl = Cl, V = V)
  state   <- c(Y  = Dose0/V, Y1 = 0, Y2 = -Dose0/(V*V))    # State at time=0
  sol     <- ode(y=state, times=tmx, func=pk1seq, parms=pars)  # ODE integration
  .value  <- sol[-1, "Y"]            # predicted values for Y, t=0 omitted 
  .grad   <- sol[-1, c("Y1","Y2")]
  colnames(.grad) <- c("Cl","V")
  attr(.value, "gradient") <- .grad
  .value
}

# Test Pk1M1 by comparing with pk1Mc  
x    <- seq(5, 25, by=10)   # No zero in the sequence
Cl   <- 0.1
V    <- 8
sol1 <- pk1M1(x, Cl, V)  # Analytical model, with gradient (to compare)
sol3c <- pk1Mc(x, Cl,V)
    
print(sol1)
print(sol3c)


pk1Fc <- function(yobs, x, Cl, V){ # Returns: Observed minus predicted
  prd      <- pk1Mc(x,Cl,V)     
  res      <- yobs - prd   
  .grad    <- attr(prd,"gradient")
  attr(res,"gradient") <- -.grad 
  res
}

# nls fit: 2 iterations
nlsFit3c <- nls(
          ~pk1Fc(y1,x,Cl,V), 
          data=pk1dt,
          start=list(Cl=0.1, V=10),
          trace=TRUE
) 



pkgname <- "irtProb"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('irtProb')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("4pl")
### * 4pl

flush(stderr()); flush(stdout())

### Name: 4pl
### Title: One, Two, Three and Four Parameters Logistic Distributions
### Aliases: p4pl q4pl d4pl r4pl
### Keywords: distribution

### ** Examples

## ....................................................................
# probability of a correct response
 p4pl(theta = 3, b = 0)
 
# Verification of the approximation of N(0,1) by a logistic (D=1.702)
 a <- 1; b <- 0; c <- 0; d <- 1; theta <- seq(-4, 4, length = 100)
 
# D constant 1.702 gives an approximation of a N(0,1) by a logistic
 prob.irt  <- p4pl(theta, a*1.702, b, c, d)
 prob.norm <- pnorm(theta, 0, 1)
 plot(theta, prob.irt)
 lines(theta, prob.norm, col = "red")
 
# Maximal difference between the two functions: less than 0.01
 max(prob.irt - prob.norm)
 
# Recovery of the value of the probability of a correct response p4pl()
# from the quantile value q4pl()
 p4pl(theta = q4pl(p = 0.20))

# Recovery of the quantile value from the probability of a correct
# response
 q4pl(p=p4pl(theta=3))

# Density Functions [derivative of p4pl()]
 d4pl(theta = 3, a = 1.702)
 theta   <- seq(-4, 4, length = 100)
 a       <- 3.702; b <- 0; c <- 0; d <- 1
 density <- d4pl(theta = theta, a = a, b = b, c = c, d = d)
 label   <- expression("Density - First Derivative")
 plot(theta, density, ylab = label, col = 1, type = "l")
 lines(theta, dnorm(x = theta, sd = 1.702/a), col = "red", type = "l")

## Generation of proficiency levels from r4pl() according to a N(0,1)
 data <- r4pl(N = 10000, a = 1.702, b = 0, c = 0, d = 1)
 c(mean = mean(data), sd = sd(data))
## ....................................................................



cleanEx()
nameEx("PersonParametersEstimate")
### * PersonParametersEstimate

flush(stderr()); flush(stdout())

### Name: PersonParametersEstimate
### Title: Estimation of the Personal Parameters from the mpl4 Logistic
###   Model
### Aliases: m4plEstimate m4plEstimateMore m4plPersonParameters
### Keywords: distribution

### ** Examples

## GENERATION OF VECTORS OF RESPONSE
 # NOTE THE USUAL PARAMETRIZATION OF THE ITEM DISCRIMINATION,
 # THE VALUE OF THE PERSONNAL FLUCTUATION FIXED AT 0,
 # AND THE VALUE OF THE PERSONNAL PSEUDO-GUESSING FIXED AT 0.30.
 # IT COULD BE TYPICAL OF PLAGIARISM BEHAVIOR.
 nItems <- 40
 a      <- rep(1.702,nItems); b <- seq(-5,5,length=nItems)
 c      <- rep(0,nItems); d <- rep(1,nItems)
 nSubjects <- 1; rep <- 2
 theta     <- seq(-1,-1,length=nSubjects)
 S         <- runif(n=nSubjects,min=0.0,max=0.0)
 C         <- runif(n=nSubjects,min=0.3,max=0.3)
 D         <- runif(n=nSubjects,min=0,max=0)
 set.seed(seed = 100)
 X         <- ggrm4pl(n=nItems, rep=rep,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)
                     
## Genreric function m4plPersonParameters to use
## prefered to the more specific one: m4plEstimate and m4plEstimateMore
 # ....................................................................
 model <- "C"
 test1 <- m4plPersonParameters(x=X, b=b, s=1/a, c=c, d=d, m=0, model=model,
                               prior="uniform", more=FALSE)
 test2 <- m4plPersonParameters(x=X, b=b, s=1/a, c=c, d=d, m=0, model=model,
                               prior="uniform", more=TRUE)
 # ....................................................................

## ESTIMATION OF THE PERSONNAL PARAMETERS BY ALL MODELS.
 # THE CHOOSEN PRIOR IS UNIFORM WITH m=0.
 # ....................................................................
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "T",    prior="uniform")
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "S",    prior="uniform")
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "C",    prior="uniform")
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "D",    prior="uniform")
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "SC",   prior="uniform")
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "SD",   prior="uniform")
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "CD",   prior="uniform")
 m4plEstimate(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "SCD",  prior="uniform")
 # ....................................................................

## THE SAME ESTIMATION, BUT WITH INFORMATION ABOUT
 # THE STANDARD ERROR, THE CORRELATION AND THE LOG LIKELIKOOD
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "T",    prior="uniform")
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "S",    prior="uniform")
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "C",    prior="uniform")
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "D",    prior="uniform")
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "SC",   prior="uniform")
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "SD",   prior="uniform")
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "CD",   prior="uniform")
 m4plEstimateMore(x=X, b=b, s=1/a, c=c, d=d, m=0, model= "SCD",  prior="uniform")
 # ....................................................................

## Same simulation, but with replications
 # ....................................................................
 rep <- 100
 set.seed(seed = 100)
 X         <- ggrm4pl(n=nItems, rep=rep,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)
                     
## Function used to extract each parameters of the list return by m4plEstimateMore
 extract <- function(x,i) x[[i]]

## Usual IRT model integrating only the parameters theta
 apply(         X, 1, m4plEstimate,     b=b, s=1/a, c=c, d=d, m=0, model= "T",
                prior="uniform")
 tests <- apply(X, 1, m4plEstimateMore, b=b, s=1/a, c=c, d=d, m=0, model= "T",
                prior="uniform")
 personParameters <- matrix(unlist(lapply(tests,extract,1)),nrow=rep)
 personSe         <- matrix(unlist(lapply(tests,extract,2)),nrow=rep)
 personCor        <- matrix(unlist(lapply(tests,extract,3)),nrow=rep)
 personLL         <- matrix(unlist(lapply(tests,extract,4)),nrow=rep)
 results          <- data.frame(Parameter=personParameters, Se=personSe,
                                LL=personLL)
 round(c(mean=sapply(results, mean, na.rm=TRUE)),2); round(c(se=sapply(results, sd, na.rm=TRUE)),2)

## Model integrating the parameters theta and C
 # Same response patterns for comparison of estimates
 t(       apply(X, 1, m4plEstimate,     b=b, s=1/a, c=c, d=d, m=0, model= "C",
                prior="uniform"))
 tests <- apply(X, 1, m4plEstimateMore, b=b, s=1/a, c=c, d=d, m=0, model= "C",
                prior="uniform")
 personParameters <- t(matrix(unlist(lapply(tests,extract,1)),ncol=rep))
 personSe         <- t(matrix(unlist(lapply(tests,extract,2)),ncol=rep))
 personLL         <- t(matrix(unlist(lapply(tests,extract,4)),ncol=rep))
 results          <- data.frame(Parameter=personParameters, Se=personSe,
                                LL=personLL)
 round(c(mean=sapply(results, mean, na.rm=TRUE)),2); round(c(se=sapply(results, sd, na.rm=TRUE)),2)
 ## Average correlation between the person parameters
 nParameters <- length(personParameters[1,])
 personCor   <- unlist(lapply(tests,extract,3))
 personCor   <- matrix( sapply(data.frame(t(matrix(personCor,ncol=rep))), mean, na.rm=TRUE),
                ncol=nParameters)
 personCor
 
## Model integrating the parameters theta and C
 # Same response patterns for comparison of estimates
 t(apply(       X,1, m4plEstimate,      b=b, s=1/a, c=c, d=d, m=0, model= "SCD",
                prior="uniform"))
 tests <- apply(X, 1, m4plEstimateMore, b=b, s=1/a, c=c, d=d, m=0, model= "SCD",
                prior="uniform")
 personParameters <- t(matrix(unlist(lapply(tests,extract,1)),ncol=rep))
 personSe         <- t(matrix(unlist(lapply(tests,extract,2)),ncol=rep))
 personLL         <- t(matrix(unlist(lapply(tests,extract,4)),ncol=rep))
 results          <- data.frame(Parameter=personParameters, Se=personSe,
                                LL=personLL)
 round(c(mean=sapply(results, mean, na.rm=TRUE)),2); round(c(se=sapply(results, sd, na.rm=TRUE)),2)
 ## Average correlation between the person parameters
 nParameters <- length(personParameters[1,])
 personCor   <- unlist(lapply(tests,extract,3))
 personCor   <- matrix( sapply(data.frame(t(matrix(personCor,ncol=rep))), mean, na.rm=TRUE),
                ncol=nParameters)
 personCor
 # ....................................................................
 


cleanEx()
nameEx("checkAdequation")
### * checkAdequation

flush(stderr()); flush(stdout())

### Name: checkAdequation
### Title: Function to Check the Adequation of the Second Derivatives
### Aliases: checkAdequation
### Keywords: distribution

### ** Examples

## Complete adequation of the matrix
## ..........................................................
 x <- matrix(c(4.867054, 16.66902, 16.669023, 107.36390), ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 eigen(x)$values
 det(x)
 diag(x)
## ..........................................................

## Not positiveDefinite matrix
## ..........................................................
 x <- matrix(1:4, ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 eigen(x)$values
 det(x)
 diag(x)
## ..........................................................

## More problems
## ..........................................................
 x <- matrix(c("Inf",2,5,10), ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 det(x)
 diag(x)
# eigen(x)$values
 x <- matrix(c("NaN",2,5,10), ncol=2)
 checkAdequation(x)
 all(checkAdequation(x))
 det(x)
 diag(x)
# eigen(x)$values
## ..........................................................



cleanEx()
nameEx("conversion")
### * conversion

flush(stderr()); flush(stdout())

### Name: conversion
### Title: Functions to Convert from and to CTT and IRT Models
### Aliases: ctt2irt irt2ctt
### Keywords: distribution

### ** Examples

## ....................................................................
# Values of p and rbis according to de a, b, c and d values
#  MODEL means that item parameters are from a NORMAL or LOGISTIC model
#  type
 irt2ctt()
 nItems <- 5
 b      <-  seq(-3, 3, length=nItems)
 a      <-  rep(1, nItems)
 c      <-  rep(0, nItems)
 d      <-  rep(1, nItems)
 
# Difference between classical item parameters and IRT ones
 irt2ctt(b=b,a=a,c=c,d=d,model="LOGISTIC")
 irt2ctt(b=b,a=a,c=c,d=d,model="NORMAL")

# Values of a and b according p and rpbis
 ctt2irt()
 
# Verification of the recovery of original ctt item parameters
 nItems <- 5
 p      <- seq(0.10, 0.90, length=nItems)
 rpbis  <- seq(0.50, 0.95, length=nItems)
 irt    <- ctt2irt(dif=p,rpbis=rpbis)
 clas   <- irt2ctt(b=irt$normal[2],a=irt$normal[1],model="NORMAL")
 round(c(NORMAL=irt$normal, IRT=irt$irt, CTT=clas$normal), 3)
## ....................................................................



cleanEx()
nameEx("derivatives")
### * derivatives

flush(stderr()); flush(stdout())

### Name: derivatives
### Title: First and Second Derivatives
### Aliases: fprime fsecond
### Keywords: math

### ** Examples

## .....................................
 test  <- function(x) 2*x+5
  test(   x=0)
  fprime( x=0, FUN=test)
  fsecond(x=0, FUN=test)
## .....................................
 test2 <- function(x) 2*(x[1]*x[2])+5
  test2(  x=c(0,0))
  fprime( x=c(0,0), FUN=test2)
  fsecond(x=c(0,0), FUN=test2)
 


cleanEx()
nameEx("graphics")
### * graphics

flush(stderr()); flush(stdout())

### Name: graphics
### Title: Graphic Functions to Illustrate Response Curves and Parameter
###   Estimation
### Aliases: PCC
### Keywords: graphs

### ** Examples

## PCC curves grouped on a single figure
 res1 <- PCC(theta=c(-2,-2,-2),S=0, C=c(0.0, 0.1, 0.6), D=0.2,
             b=seq(-5,5,length=3000), ID=NULL, groups=TRUE,
             type=c("g","a"))
 res1
 
## PCC curves shingled on a single figure for each subject
 res2 <- PCC(theta=c(-2,-1,0),S=c(4.0,0.0, 1.0), C=c(0.0, 0.1, 0.6), D=0.2,
             b=seq(-5,5,length=3000), ID=NULL, groups=FALSE,
             type=c("g","a"))
 res2
 


cleanEx()
nameEx("likelihoodCurve")
### * likelihoodCurve

flush(stderr()); flush(stdout())

### Name: likelihoodCurve
### Title: Functions to Graph m4pl Likelihood Curves
### Aliases: likelihoodCurve groupLikelihoodCurves
### Keywords: multivariate

### ** Examples

 ## SIMULATION OF A RESPONSE PATTERN WITH 60 ITEMS
 nItems <- 60
 a      <- rep(1.702,nItems); b <- seq(-4,4,length=nItems)
 c      <- rep(0,nItems);     d <- rep(1,nItems)
 nSubjects <-  1
 theta     <-  -1
 S         <-  0.0
 C         <-  0.5
 D         <-  0.0
 
 set.seed(seed = 100)
 x         <- ggrm4pl(n=nItems, rep=1,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)

 ## Likelihood curves, person parameters estimates
  # and log likelihood of models graphed
 test <- likelihoodCurve(x=x, s=1/a, b=b, c=c, d=d, color=TRUE,
                         main="Likelihood Curve",
                         xlab=expression(theta), ylab=NULL, zlab="P(X)",
                         type="wireframe" , grain=50, limitD=c(0,1),
                         logLikelihood=FALSE, annotate=TRUE )

 # Contentd of the object test
 test$plotT
 test$plotC
 test$plotS
 test$plotD
 test$par
 round(test$logLikelihood,2)

 ## Graph of all the likelihood function curves
 groupLikelihoodCurves(test$plotT, test$plotS, test$plotC, test$plotD,
                       main=NULL, cex=0.7)
 


cleanEx()
nameEx("m4pl")
### * m4pl

flush(stderr()); flush(stdout())

### Name: m4pl
### Title: Multidimensional One, Two, Three and Four Person Parameters
###   Logistic Distributions
### Aliases: pm4pl dm4pl qm4pl rm4pl
### Keywords: distribution

### ** Examples

## ....................................................................
# Approximation of p4pl() by pm4pl()
 theta  <- 0
 S      <- 0; C <- 0; D <- 0
 a      <- 1.702; s <- sqrt(1/a^2); b <- 0; c <- 0; d <- 1
 p.4pl  <-  p4pl(theta,a=a,b,c,d);
 p.m4pl <- pm4pl(theta,S,C,D,s,b,c,d)
 print(c(p4pl=p.4pl, m4pl=p.m4pl))

# Comparison of p4pl() and pm4pl() according to diverse values of theta
#  while person parameters vary
 theta  <- seq(-4,4,length=100); N <- length(theta)
 S      <- rep(0.9,N); C <- rep(0,N); D <- rep(0.9,N)
 a      <- 1.702; s <- 1/1.702
 p.4pl  <- p4pl(theta=theta, a=a, b=b, c=c, d=d)
 p.m4pl <- pm4pl(theta=theta, S=S, C=C, D=D, s=s, b=b, c=c, d=d)
 print(c(difference.maximale = max(p.4pl - p.m4pl)))
 round(rbind(theta=theta, p4pl=p.4pl, m4pl=p.m4pl, dif=p.4pl-p.m4pl),3)
 plot(theta,p.4pl,type="l", col="black", ylab="Probability")
 lines(theta,p.m4pl,col="red"); max(p.4pl-p.m4pl)
 
# Recovery of probability by quantile
 pm4pl(theta=3,b=0); pm4pl(theta=qm4pl(p=0.20))

# Density function dm4pl()
# Comparison 01 between d4pl() and dm4pl()
 theta  <- seq(-4,4, length=100)
 # theta <- 0 # We can also experiment with an unique value of theta
 N      <- length(theta)
 S      <- rep(0,N); C <- rep(0,N); D <- rep(0,N)
 a      <- 1.702
 d.4pl  <- d4pl(theta=theta,a=a)
 d.m4pl <- dm4pl(theta=theta,S=0,s=1/a)
 stats  <- round(cbind(theta=theta, d4pl=d.4pl, m4pl=d.m4pl, dif=d.4pl-d.m4pl),3)
 print(stats)
 print(max(d.4pl - d.m4pl))
 plot(theta,d.4pl,type="l", col="black", ylab="Density")
 lines(theta,d.m4pl,col="red")

# Comparison 02 between d4pl() and dm4pl()
 theta  <- seq(-4,4, length=10)
 N      <- length(theta)
 S      <- rep(.3,N); C <- rep(0,N); D <- rep(0,N)
 a      <- 1.702
 d.4pl  <- d4pl(theta=theta,a=a)
 d.m4pl <- dm4pl(theta=theta, S=S, C=C, D=D, s=1/a)
 stats  <- round(cbind(theta=theta, d4pl=d.4pl, m4pl=d.m4pl, dif=d.4pl-d.m4pl),3)
 print(stats)
 print(max(d.4pl - d.m4pl))
 plot(theta,d.4pl,type="l", col="black", ylab="Density")
 lines(theta,d.m4pl,col="red")

# Comparison of q4pl and qm4pl
#  followed by recovery of quantiles
 pm4pl(theta=1, ); p4pl(theta=1, a=1.702)
 qm4pl(p=0.99,); q4pl(p=0.99, a=1.702)
 qm4pl(pm4pl(theta=1)); q4pl(p4pl(theta=1, a=1.702), a=1.702)
 qm4pl(p=pm4pl(theta=3))
 qm4pl(p=seq(0.01,0.99, length=10))

# Generation of theta values by rm4pl()
# ... Exemple 01 - A 4pl() equivalent distribution must be recovered when a=1.702
 res   <- rm4pl(N=1000)
 stats <- c(mean=sapply(res, mean), sd=sapply(res, sd), skewness=sapply(res, skewness),
            kurtosis=3-sapply(res, kurtosis))
 print(stats)
 # pdf of this distribution
 theta   <- seq(-4,4,length=100); C=rep(0,N); D=rep(0,N)
 density <- dm4pl(theta,C=C,D=D)
 plot(theta, density, type="l")

# ... Exemple 02 - Distribution with D != 0
 require(moments)
 S     <- 1/1.702
 B     <- 0
 C     <- 0.00
 D     <- 0.90
 res   <- rm4pl(N=1000, S=S, C=C, D=D)
 stats <- c(mean=sapply(res, mean), sd=sapply(res, sd), skewness=sapply(res, skewness),
            kurtosis=3-sapply(res, kurtosis))
 print(stats)
 # pdf of this distribution
 theta   <- seq(-4,4,length=100)
 density <- dm4pl(theta, S=S, C=C, D=D)
 print(c(max=max(density)))
 plot(theta, density, type="l")
## ....................................................................
 


cleanEx()
nameEx("m4plModelShow")
### * m4plModelShow

flush(stderr()); flush(stdout())

### Name: m4plModelShow
### Title: Results For Each Subject To Each Model
### Aliases: m4plModelShow
### Keywords: distribution

### ** Examples

## GENERATION OF VECTORS OF RESPONSES
 # NOTE THE USUAL PARAMETRIZATION OF THE ITEM DISCRIMINATION,
 # THE VALUE OF THE PERSONNAL FLUCTUATION FIXED AT 0,
 # AND THE VALUE OF THE PERSONNAL PSEUDO-GUESSING FIXED AT 0.30.
 # IT COULD BE TYPICAL OF PLAGIARISM BEHAVIOR.
 nItems <- 40
 a      <- rep(1.702,nItems); b <- seq(-5,5,length=nItems)
 c      <- rep(0,nItems); d <- rep(1,nItems)
 nSubjects <- 1; rep <- 100
 theta     <- seq(-1,-1,length=nSubjects)
 S         <- runif(n=nSubjects,min=0.0,max=0.0)
 C         <- runif(n=nSubjects,min=0.3,max=0.3)
 D         <- runif(n=nSubjects,min=0.0,max=0.0)
 set.seed(seed = 100)
 X         <- ggrm4pl(n=nItems, rep=rep,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)

## Results for each subjects for each models
 essai <- m4plModelShow(X, b=b, s=1/a, c=c, d=d, m=0, prior="uniform")
 
## Mean results for some speficic models
 median(essai[which(essai$MODEL == "TSCD") ,]$SeT, na.rm=TRUE)
 mean(  essai[which(essai$MODEL == "TSCD") ,]$SeT, na.rm=TRUE)
 mean(  essai[which(essai$MODEL ==   "TD") ,]$SeT, na.rm=TRUE)
 sd(    essai[which(essai$MODEL ==   "TD") ,]$T, na.rm=TRUE)
 
## Result for each models for the first subject
 essai[which(essai$ID == 1) ,]
 max(essai[which(essai$ID == 1) ,]$LL)

## Difference between the estimated values with the T and TSCD models for the
## first subject
 essai[which(essai$ID == 1 & essai$MODEL == "T"),]$T
       - essai[which(essai$ID == 1 & essai$MODEL == "TSCD"),]$T



cleanEx()
nameEx("m4plSummary")
### * m4plSummary

flush(stderr()); flush(stdout())

### Name: m4plSummary
### Title: Summary of the Results of Estimation with the m4pl Models
### Aliases: m4plSummary m4plMoreSummary m4plNoMoreSummary
### Keywords: distribution

### ** Examples


## GENERATION OF VECTORS OF RESPONSE
 # NOTE THE USUAL PARAMETRIZATION OF THE ITEM DISCRIMINATION,
 # THE VALUE OF THE PERSONNAL FLUCTUATION FIXED AT 0,
 # AND THE VALUE OF THE PERSONNAL PSEUDO-GUESSING FIXED AT 0.30.
 # IT COULD BE TYPICAL OF PLAGIARISM BEHAVIOR.
 nSubjects <- 1
 nItems <- 40
 a      <- rep(1.702,nItems); b <- seq(-5,5,length=nItems)
 c      <- rep(0,nItems); d <- rep(1,nItems)
 theta     <- seq(-2,-2,length=nSubjects)
 S         <- runif(n=nSubjects,min=0.0,max=0.0)
 C         <- runif(n=nSubjects,min=0.3,max=0.3)
 D         <- runif(n=nSubjects,min=0.0,max=0.0)
 rep <- 100
 set.seed(seed = 10)
 X         <- ggrm4pl(n=nItems, rep=rep,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)

## Estimation of the model integrating the T and the C parameters
 model <- "C"
 test  <- m4plPersonParameters(x=X, b=b, s=1/a, c=c, d=d, m=0, model=model,
                               prior="uniform", more=TRUE)

## Summary of the preceding model (report and first 5 subjects)
 essai <- m4plSummary(X=test, out="report")
 # Rounding the result of the list to two decimals
 lapply(essai, round, 2)
 essai <- m4plSummary(X=test, out="result")[1:5,]
 lapply(essai, round, 2)
 essai <- m4plSummary(X=test, out="report", thetaInitial=theta)
 lapply(essai, round, 2)
 essai <- m4plSummary(X=test, out="result", thetaInitial=theta)[1:5,]
 lapply(essai, round, 2)

## Results directly from m4plMoreSummary()
 essai <- m4plMoreSummary(x=test, out="report")
 lapply(essai, round, 2)
 essai <- m4plMoreSummary(x=test, out="result")[1:5,]
 round(essai, 2)

## To obtain more general statistics on the result report
 essai <- m4plMoreSummary(x=test, out="result")
 m4plNoMoreSummary(essai)
 summary(m4plMoreSummary(x=test, out="result"))



cleanEx()
nameEx("modelChoose")
### * modelChoose

flush(stderr()); flush(stdout())

### Name: modelChoose
### Title: Functions For Choosing The m4pl Best Model(s)
### Aliases: meanModels modelChoose modelChooseAdd
### Keywords: distribution

### ** Examples

## GENERATION OF VECTORS OF RESPONSE
 # NOTE THE USUAL PARAMETRIZATION OF THE ITEM DISCRIMINATION,
 # THE VALUE OF THE PERSONNAL FLUCTUATION FIXED AT 0,
 # AND THE VALUE OF THE PERSONNAL PSEUDO-GUESSING FIXED AT 0.30.
 # IT COULD BE TYPICAL OF PLAGIARISM BEHAVIOR.
 nItems <- 40
 a      <- rep(1.702,nItems); b <- seq(-5,5,length=nItems)
 c      <- rep(0,nItems); d <- rep(1,nItems)
 nSubjects <- 1; rep <- 100
 theta     <- seq(-1,-1,length=nSubjects)
 S         <- runif(n=nSubjects,min=0.0,max=0.0)
 C         <- runif(n=nSubjects,min=0.3,max=0.3)
 D         <- runif(n=nSubjects,min=0,max=0)
 set.seed(seed = 100)
 X         <- ggrm4pl(n=nItems, rep=rep,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)

## Results for each subjects for each models
 essai <- m4plModelShow(X, b=b, s=1/a, c=c, d=d, m=0, prior="uniform")

## Means of choosen variables from m4plModleShow previous call
 round(meanModels(essai, statistic=c("LL","BIC","T","C")), 2)

## Which model(s) are the best for each subject (LL criteria)
 res <- modelChoose(essai, criteria = "LL", tol = 0.2); res
 
## Which model(s) are the best for each of the first 20 subjects (BIC criteria)
 res <- modelChoose(essai[which(essai$ID < 20) ,], criteria="BIC"); res

## A look at the 15th subject parameters estimation for each models
 essai[which(essai$ID == 15) ,]
 
## Add the logical critLL variable to the data frame
 criteria <- "LL"
 res1    <- modelChooseAdd(essai, criteria=criteria)
 # Create a charcater string composed from "crit" and criteria
 crit    <- paste("crit",criteria,sep="")
 # To Show only the lines where the models is choosen according to critLL
 res2    <- res1[which(res1[crit] == TRUE),]; mean(res2$T);sd(res2$T,na.rm=TRUE);res2
 # Give the mean for choosen variables from m4plModelShow
 meanModels(essai, statistic=c("LL","BIC","T","SeT","S","C","D"))
 # Tabulate only the choosen models
 table(res2$MODEL)
 # Show the information for the subject for which the model TSCD was choosen
 res2[which(res2$MODEL == "TSCD") ,]
 # Show only the results for the 5th subject, the with the TSCD model choosen
 res1[which(res1$ID == 5) ,]
 # Same, but without critLL
 essai[which(essai$ID == 5) ,]

## Simulation whith cheating
 # High proficiency students responding at random to 20% of the first items
 # easiest ones)
 XHProficiency             <- X
 pourcHasard               <- 0.20; nHasard <- abs(dim(X)[2]*pourcHasard)
 XHProficiency[,1:nHasard] <- rbinom(dim(X)[1]*nHasard, 1, 0.5)
 XHProficiency             <- m4plModelShow(XHProficiency, b=b, s=1/a, c=c, d=d,
                                            m=0, prior="uniform")
 XHProficiency             <- modelChooseAdd(XHProficiency, criteria=criteria)
 XHProficiency             <- XHProficiency[which(XHProficiency$critLL == TRUE),]
 mean(XHProficiency$T);sd(XHProficiency$T,na.rm=TRUE);XHProficiency[1:10,]
 meanModels(XHProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XHProficiency$MODEL)
 
 # Low proficiency students responding at random to 20% of the last items
 # (more difficult ones)
 XLProficiency                             <- X
 pourcHasard                               <- 0.20
 nHasard                                   <- abs(dim(X)[2]*pourcHasard)
 XLProficiency[,(nItems-nHasard+1):nItems] <- rbinom(dim(X)[1]*nHasard, 1, 0.5)
 XLProficiency                             <- m4plModelShow(XLProficiency, b=b,
                                              s=1/a, c=c, d=d, m=0, prior="uniform")
 XLProficiency                             <- modelChooseAdd(XLProficiency,
                                                             criteria=criteria)
 XLProficiency <- XLProficiency[which(XLProficiency$critLL == TRUE),]
 mean(XLProficiency$T);sd(XLProficiency$T,na.rm=TRUE);XLProficiency[1:10,]
 meanModels(XLProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XLProficiency$MODEL)

 # High proficiency students giving incorrect responses to 20% of the first items
 # (easiest ones)
 XHProficiency             <- X
 pourcCheat                <- 0.20; nCheat  <- abs(dim(X)[2]*pourcCheat)
 XHProficiency[,1:nCheat]  <- rep(0, dim(X)[1]*dim(X)[2]*pourcCheat)
 XHProficiency             <- m4plModelShow(XHProficiency, b=b, s=1/a, c=c, d=d,
                                            m=0, prior="uniform")
 XHProficiency             <- modelChooseAdd(XHProficiency, criteria=criteria)
 XHProficiency             <- XHProficiency[which(XHProficiency$critLL == TRUE),]
 mean(XHProficiency$T);sd(XHProficiency$T,na.rm=TRUE);XHProficiency[1:10,]
 meanModels(XHProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XHProficiency$MODEL)
 
 # Low proficiency students giving correct responses to 20% of the last items
 # (more difficult ones)
 XLProficiency                             <- X
 pourcCheat                                <- 0.20
 nCheat                                    <- abs(dim(X)[2]*pourcCheat);
 XLProficiency[,(nItems-nCheat+1):nItems]  <- rep(1, dim(X)[1]*dim(X)[2]*pourcCheat)
 XLProficiency                             <- m4plModelShow(XLProficiency, b=b,
                                              s=1/a, c=c, d=d, m=0, prior="uniform")
 XLProficiency                             <- modelChooseAdd(XLProficiency,
                                                             criteria=criteria)
 XLProficiency <- XLProficiency[which(XLProficiency$critLL == TRUE),]
 mean(XLProficiency$T);sd(XLProficiency$T,na.rm=TRUE);XLProficiency[1:10,]
 meanModels(XLProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XLProficiency$MODEL)
 


cleanEx()
nameEx("modelShowClassFunctions")
### * modelShowClassFunctions

flush(stderr()); flush(stdout())

### Name: modelShowClassFunctions
### Title: Functions To Manipulate modelShow Class Objects
### Aliases: is.modelShow Round Round.modelShow summary.modelShow
### Keywords: distribution

### ** Examples

## GENERATION OF VECTORS OF RESPONSE
 # NOTE THE USUAL PARAMETRIZATION OF THE ITEM DISCRIMINATION,
 # THE VALUE OF THE PERSONNAL FLUCTUATION FIXED AT 0,
 # AND THE VALUE OF THE PERSONNAL PSEUDO-GUESSING FIXED AT 0.30.
 # IT COULD BE TYPICAL OF PLAGIARISM BEHAVIOR.
 nItems <- 40
 a      <- rep(1.702,nItems); b <- seq(-5,5,length=nItems)
 c      <- rep(0,nItems); d <- rep(1,nItems)
 nSubjects <- 1; rep <- 100
 theta     <- seq(-2,-2,length=nSubjects)
 S         <- runif(n=nSubjects,min=0.0,max=0.0)
 C         <- runif(n=nSubjects,min=0.3,max=0.3)
 D         <- runif(n=nSubjects,min=0,max=0)
 set.seed(seed = 100)
 X         <- ggrm4pl(n=nItems, rep=rep,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)

## Results for each subjects for each models
 essai <- m4plModelShow(X, b=b, s=1/a, c=c, d=d, m=0, prior="uniform")

## Is essai of class modelShow?
 is.modelShow(essai)
 
## Rounding to 2 decimals the first 5 results of essai
 Round(essai[1:5,], 2)

## Means for each models rounded to 3 decimals
 summary(essai, report="means", statistics=c("LL","AIC","BIC","T","SeT"), digits=3)

## Model choosen for each of the first 5 subjects
## and the frequency of these choices with the BIC criteria
 summary(essai[which(essai$ID == (1:5)),], report="choose", criteria="BIC")

## Frequency of the models choosen for all the subjects
## with the LL, AIC and BIC criteria
## Generally, BIC chooses the less models AIC the more.
 summary(essai, report="table", criteria="LL")
 summary(essai, report="table", criteria="AIC")
 summary(essai, report="table", criteria="BIC")
 
## Frequency of the models choosen for all the subjects
## with the BIC criteria, but with a histogram
 summary(essai, report="histogram", criteria="BIC", color="blue")

## The choosen model is added to the essai modelShow object for all the subjects
## with the LL, AIC and BIC criteria and statistics about theta are computed
## Recall thet rhe generating theta was fixed at -2.00
## The LL criteria seems the best one her according to bias and standard error
 resultLL  <- summary(essai, report="add", criteria="LL")
 resultAIC <- summary(essai, report="add", criteria="AIC")
 resultBIC <- summary(essai, report="add", criteria="BIC")
 # LL
 summary(resultLL[which(resultLL$critLL == TRUE),]$T)
 sd(resultLL[which(resultLL$critLL == TRUE),]$T, na.rm=TRUE)
 # AIC
 summary(resultAIC[which(resultAIC$critAIC == TRUE),]$T)
 sd(resultAIC[which(resultAIC$critAIC == TRUE),]$T, na.rm=TRUE)
 # BIC
 summary(resultBIC[which(resultBIC$critBIC == TRUE),]$T)
 sd(resultBIC[which(resultBIC$critBIC == TRUE),]$T, na.rm=TRUE)
 


cleanEx()
nameEx("responses4pl")
### * responses4pl

flush(stderr()); flush(stdout())

### Name: responses4pl
### Title: Simulation of Response Patterns and Computation of the
###   Probability of the Patterns
### Aliases: gr4pl ggr4pl pggr4pl
### Keywords: distribution

### ** Examples

## ....................................................................
# Generation of reponses (0,1) from r4pl() for N subjects (default value of N= 10)
 gr4pl(c = 1)
 gr4pl(N = 5, theta = c(-4, 4), c = 0)

# Generation of a 7 responses pattern (0,1) for [rep * length(theta)] subjects
#  The subjects number is equal to [rep * length(theta)]]
#  a,b,c et d are item parameters vectors
 nitems <- 7
 N      <- 10
 a      <- rep(1, nitems)
 b      <- rnorm(nitems)
 c      <- rep(0, nitems)
 d      <- rep(1, nitems)
 theta  <- seq(-4,4,length=5)
 x      <- ggr4pl(n = nitems, rep = N, theta = theta, a = a, b = b, c = c, d = d)
 x

## Probability of a 10 responses pattern and test caracteristic curve (TCC)
 nitems <- 10
 a      <- rep(1,nitems)
 b      <- seq(-4,4,length=nitems)
 c      <- rep(0,nitems)
 d      <- rep(1,nitems)
 N      <- 3
 theta  <- seq(-1,1,length=12)
 # Generation of the response patterns
 x      <- ggr4pl(n = nitems, rep = N, theta = theta, a = a, b = b, c = c, d = d)
 x
 # Without TCC
 res    <- pggr4pl(x=x, rep=N, theta=theta,a=a,c=c,d=d,TCC=FALSE); res
 # With TCC for each response pattern
 res    <- pggr4pl(x=x, rep=N, theta=theta,a=a,c=c,d=d,TCC=TRUE); res
## ....................................................................
 


cleanEx()
nameEx("responsesm4pl")
### * responsesm4pl

flush(stderr()); flush(stdout())

### Name: responsesm4pl
### Title: Simulation of Response Patterns and Computation of the
###   Probability of the Patterns from m4pl
### Aliases: grm4pl ggrm4pl pggrm4pl
### Keywords: distribution

### ** Examples

## ....................................................................
# Generation of response patterns (0,1) from r4pl() for N subjects (default value
# of N = 10)

# Generation of a response (0,1) from rm4pl for N subjects
 grm4pl(theta=0)
 grm4pl(N=5, theta=c(-4,4), c=0)

# Generation of n m4pl response patterns (0,1) for [rep * length(theta)] subjects
#  The subject number ia equal to [rep * length(theta)]
#  a,b,c et d are item parameters vectors
 nitems <- n <- 7; N <- 1
 s     <- rep(0,nitems); b <- seq(-4,4,length=nitems); c <- rep(0,nitems)
 d     <- rep(1,nitems)
 theta <- seq(-4,4,length=5)
 x     <- ggrm4pl(n=nitems, rep=N, theta=theta,s=s,b=b,c=c,d=d)
 x

# TO BE REWORKED - Probability of a response pattern and test caracteristic curve
# (TCC)
 nItems <- n <- 7; N <- 1
 s      <- rep(0,nItems); b <- seq(-4,4,length=nItems)
 c      <- rep(0,nItems); d <- rep(1,nItems)
 theta <- seq(-4,4,length=5);     S <- rep(1/1.702,length(theta));
 C     <- rep(0.3,length(theta)); D <- rep(0,length(theta))
 x <- ggrm4pl(n=nItems, rep=N, theta=theta, S=S, C=C, D=D, s=s, b=b, c=c, d=d)
 x
 res <- pggrm4pl(x=x, rep=N, theta=theta, S=1/1.702, C=0.3, D=0, s=s, c=c, d=d,
                 TCC=TRUE)
 res
 res <- pggrm4pl(x=x, rep=N, theta=rep(2,length(theta)), S=1/1.702, C=0, D=0,
                 s=s, c=c, d=d, TCC=FALSE)
 res
 pggrm4pl(theta=3)
 pggrm4pl(n=10, theta=seq(-4,4,length=5), x=ggrm4pl(rep=1), TCC=TRUE)
## ....................................................................
 


cleanEx()
nameEx("utilities")
### * utilities

flush(stderr()); flush(stdout())

### Name: utilities
### Title: Utility Functions
### Aliases: rmultinomial propCorrect
### Keywords: distribution

### ** Examples

## Comparison of the results from the multinomial and multinom functions
 x <- c(1,4,9)
 # Values draws
 rmultinomial(x=x, n=10)
 # Binary vectors draws
 rmultinom(n=10, size = 1, prob=rep(1,length(x))/length(x))

## Computation of the expected proportion of correct responses varying values
 # of theta (-3 to 3) and of pseudo-guessing (C = 0.0 to 0.6) person parameters
 nItems  <- 40
 a       <- rep(1.702,nItems); b <- seq(-3,3,length=nItems)
 c       <- rep(0,nItems); d <- rep(1,nItems)
 theta   <- seq(-3.0, 3.0, by=1.0)
 C       <- seq( 0.0, 1.0, by=0.1)
 D       <- S <- 0
 
 results <- matrix(NA, ncol=length(C), nrow=length(theta))
 colnames(results) <- C; rownames(results) <- theta
 for (i in (1:length(theta))) {
  results[i,] <- propCorrect(theta = theta[i], S = 0, C = C, D = 0,
                             s = 1/a, b = b, c = c, d = d)
  }
  round(results, 2)

## Computation of the expected proportion of correct responses varying values
 # of theta (-3 to 3) and of pseudo-guessing (C = 0.0 to 0.6) person parameters
 # if we choose the correct modelisation itegrating the C pseudo-guessing papameter
 # and if we choose according to a model selection by LL criteria
 nItems <- 40
 a      <- rep(1.702,nItems); b <- seq(-3,3,length=nItems)
 c      <- rep(0,nItems); d <- rep(1,nItems)
 nSubjects <- 300
 theta     <- rmultinomial(c(-1), nSubjects)
 S         <- rmultinomial(c(0), nSubjects)
 C         <- rmultinomial(seq(0,0.9,by=0.1), nSubjects)
 D         <- rmultinomial(c(0), nSubjects)
 set.seed(seed = 100)
 X         <- ggrm4pl(n=nItems, rep=1,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)
 # Results for each subjects for each models
 essai     <- m4plModelShow(X, b=b, s=1/a, c=c, d=d, m=0, prior="uniform")
 total     <- rowSums(X)
 pourcent  <- total/nItems * 100
 pCorrect  <- numeric(dim(essai)[1])
 for ( i in (1:dim(essai)[1]))
  pCorrect[i] <- propCorrect(essai$T[i],0,0,0,s=1/a,b=b,c=c,d=d)
 resultLL  <- summary(essai, report="add", criteria="LL")
 resultLL  <- data.frame(resultLL, theta=theta, TS=S, TC=C, errorT=resultLL$T - theta,
                         total=total, pourcent=pourcent, tpcorrect=pCorrect)
 # If the only theta model is badly choosen
 results <- resultLL[which(resultLL$MODEL == "T" ),]
 byStats <- "TC"; ofStats <- "tpcorrect"
 MeansByThetaT <- cbind(
  aggregate(results[ofStats], by=list(Theta=factor(results[,byStats]) ),
            mean, na.rm=TRUE),
  aggregate(results[ofStats], by=list(Theta=factor(results[,byStats])), sd, na.rm=TRUE),
  aggregate(results["SeT"], by=list(Theta=factor(results[,byStats])), mean, na.rm=TRUE),
  aggregate(results[ofStats], by=list(theta=factor(results[,byStats])), length)
  )[,-c(3,5,7)]
  names(MeansByThetaT) <- c("C", "pCorrect", "seE", "SeT", "n")
  MeansByThetaT[,-c(1,4,5)] <- round(MeansByThetaT[,-c(1,4,5)], 2)
  MeansByThetaT[,-c(4,5)]
 # Only for the TC model
 results <- resultLL[which(resultLL$MODEL == "TC" ),]
 byStats <- "TC"; ofStats <- "tpcorrect"
 MeansByThetaC <- cbind(
  aggregate(results[ofStats], by=list(Theta=factor(results[,byStats]) ),
            mean, na.rm=TRUE),
  aggregate(results[ofStats], by=list(Theta=factor(results[,byStats])), sd, na.rm=TRUE),
  aggregate(results["SeT"], by=list(Theta=factor(results[,byStats])), mean, na.rm=TRUE),
  aggregate(results[ofStats], by=list(theta=factor(results[,byStats])), length)
  )[,-c(3,5,7)]
  names(MeansByThetaC) <- c("C", "pCorrect", "seE", "SeT", "n")
  MeansByThetaC[,-c(1,4,5)] <- round(MeansByThetaC[,-c(1,4,5)], 2)
  MeansByThetaC[,-c(4,5)]
 # For the model choosen according to the LL criteria
 results <- resultLL[which(resultLL$critLL == TRUE),]
 byStats <- "TC"; ofStats <- "tpcorrect"
 MeansByThetaLL <- cbind(
  aggregate(results[ofStats], by=list(Theta=factor(results[,byStats]) ),
            mean, na.rm=TRUE),
  aggregate(results[ofStats], by=list(Theta=factor(results[,byStats])), sd, na.rm=TRUE),
  aggregate(results["SeT"], by=list(Theta=factor(results[,byStats])), mean, na.rm=TRUE),
  aggregate(results[ofStats], by=list(theta=factor(results[,byStats])), length)
  )[,-c(3,5,7)]
  names(MeansByThetaLL) <- c("C", "pCorrect", "seE", "SeT", "n")
  MeansByThetaLL[,-c(1,4,5)] <- round(MeansByThetaLL[,-c(1,4,5)], 2)
  MeansByThetaLL[,-c(4,5)]
  # Grapical comparison of the estimation of the % of correct responses
  # by means of the 3 preceeding models
  plot(MeansByThetaT$pCorrect   ~ levels(MeansByThetaT$C),  type="l", lty=1,
       xlab="Pseudo-Guessing", ylab="% of Correct Responses")
  lines(MeansByThetaC$pCorrect  ~ levels(MeansByThetaC$C),  type="l", lty=2)
  lines(MeansByThetaLL$pCorrect ~ levels(MeansByThetaLL$C), type="l", lty=3)
  text(x=0.60, y=0.80, "Without correction", cex=0.8)
  text(x=0.50, y=0.38, "Without Knowledge of the Correct Model", cex=0.8)
  text(x=0.65, y=0.50, "With Knowledge of the Correct Model", cex=0.8)
 


### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

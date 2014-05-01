\name{utilities}
\alias{rmultinomial}
\alias{propCorrect}

\title{ Utility Functions}

\description{
 Some functions to help in the generation of binary data or to interpret m4pl
 models results.
 
 \code{rmultinomial} is used to draw \code{n} values from a \code{x}
 vector of values according to a multinomial probability distribution. \code{rmultinomial}
 is different from the \code{stats::rmultinom} function in that it return only the value of the
 selected draw and not a binary vector corresponding tho the position of the draw

 \code{propCorrect} computes the expected proportion of correct responses to a test
 according to the item and person parameters of a \code{m4pl} models.
 }

\usage{
 rmultinomial(x, n = 100, prob = rep(1, length(x))/length(x))

 propCorrect(theta, S, C, D, s, b, c, d)
 }

\arguments{
  \item{x}{ numeric: vector of values to draw from.}
  \item{n}{ numeric: number of draws. }
  \item{prob}{ numeric: probability assigned to each value of the x vector.
               Default to equiprobability. }
  \item{theta}{ numeric; vector of person proficiency (\eqn{\theta}) levels scaled on a normal \code{z} score. }
  \item{S}{ numeric: positive vector of personal fluctuation parameters (\eqn{\sigma}). }
  \item{C}{ numeric: positive vector of personal pseudo-guessing parameters (\eqn{\chi}, a probability between 0 and 1). }
  \item{D}{ numeric: positive vector of personal inattention parameters (\eqn{\delta}, a probability between 0 and 1). }
  \item{s}{ numeric: positive vector of item fluctuation parameters.}
  \item{b}{ numeric: vector of item difficulty parameters.}
  \item{c}{ numeric: positive vector of item pseudo-guessing parameters (a probability between 0 and 1).}
  \item{d}{ numeric: positive vector of item inattention parameters (a probability between 0 and 1).}
 }

\value{
 \code{propCorrect}
  \item{numeric:}{ return \code{n} draws from a multinimial distribution.}
 \code{rmultinomial}
  \item{numeric:}{ return the expected proportion of correcte responses to a test.}
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\seealso{
 \code{\link[stats]{Multinom}}
 }

\examples{
\dontrun{
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
       xlab="Pseudo-Guessing", ylab="\% of Correct Responses")
  lines(MeansByThetaC$pCorrect  ~ levels(MeansByThetaC$C),  type="l", lty=2)
  lines(MeansByThetaLL$pCorrect ~ levels(MeansByThetaLL$C), type="l", lty=3)
  text(x=0.60, y=0.80, "Without correction", cex=0.8)
  text(x=0.50, y=0.38, "Without Knowledge of the Correct Model", cex=0.8)
  text(x=0.65, y=0.50, "With Knowledge of the Correct Model", cex=0.8)
 }
 }

\keyword{ distribution }


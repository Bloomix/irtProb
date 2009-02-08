\name{m4pl}
\alias{pm4pl}
\alias{dm4pl}
\alias{qm4pl}
\alias{rm4pl}
\title{ Multidimensional One, Two, Three and Four Person Parameters Logistic Distributions }

\description{
 Density, distribution function, quantile function and random generation for the multidimensional one,
 two, three and four person parameters logistic distributions.
 }

\usage{
 pm4pl(theta = 0, S = 0, C = 0, D = 0, s = 1/1.702, b = 0, c = 0, d = 1,
       lower.tail = TRUE, log.p = FALSE)
      
 dm4pl(theta = 0, S = rep(0,length(theta)), C = rep(0,length(theta)),
       D = rep(0,length(theta)), b = 0, s = 1/1.702, c = 0, d = 1, log.p = FALSE)
      
 qm4pl(p = 0.05, S = 0, C = 0, D = 0, s = 1/1.702, b = 0, c = 0, d = 1,
       lower.tail = TRUE, log.p = FALSE)
      
 rm4pl(N = 100, S = 0, C = 0, D = 0, s = 1/1.702, b = 0, c = 0, d = 1)
 }

\arguments{
  \item{theta}{ numeric; vector of person proficiency (\eqn{\theta}) levels scaled on a normal \code{z} score. }
  \item{S}{ numeric; positive vector of personal fluctuation parameters (\eqn{\sigma}). }
  \item{C}{ numeric; positive vector of personal pseudo-guessing parameters (\eqn{\chi}, a probability between 0 and 1). }
  \item{D}{ numeric; positive vector of personal inattention parameters (\eqn{\delta}, a probability between 0 and 1). }
  \item{N}{ numeric; number of observations. }
  \item{p}{ numeric; vector of probability. }
  \item{s}{ numeric; positive vector of item fluctuation parameters.}
  \item{b}{ numeric; vector of item difficulty parameters.}
  \item{c}{ numeric; positive vector of item pseudo-guessing parameters (a probability between 0 and 1).}
  \item{d}{ numeric; positive vector of item inattention parameters (a probability between 0 and 1).}
  \item{lower.tail}{ logical; if TRUE (default), probabilities are \eqn{P(X_{j} <= x_{ij})},
  otherwise, \eqn{P(X_{j} > x_{ij})}.}
  \item{log.p}{ logical; if TRUE, probabilities p are given as log(p).}
 }

\details{
 The multidimensional 4 persons parameters logistic distribution (cdf) is equal to:
  \deqn{
 P(x_{ij}  = 1|\theta _j ,\sigma _j ,\chi _j ,\delta _j ,s_i ,b_i ,c_i ,d_i ) = (\chi _j  + c_i ) + {{(d_i  - \delta _j ) - (\chi _j  + c_i )} \over {1 + e^{{{ - Da_i (\theta _j  - b_i )} \over {\sqrt {\sigma _j^2  + s_i^2 } }}} }}
 }
 where the parameters are defined in the section arguments and \emph{i} and \emph{j} are respectively
 the items and the persons indices. The \eqn{\sigma_j}, \eqn{\chi_j} and \eqn{\delta_j} parameters are respectively
 the personnal fluctuation, pseudo-guessing and inattention parameters. The multidimensional 4 persons
 parameters logistic model (M4PL) was described by Raiche,
 Magis and Blais (2008; Raiche, Magis and Beland, 2009; Raiche, Blais and Magis, 2009).
 }

\value{
  \item{pm4pl}{numeric; gives the distribution function (\code{cdf}).}
  \item{dm4pl}{numeric; gives the density (derivative of p4pl).}
  \item{qm4pl}{numeric; gives the quantile function (inverse of p4pl).}
  \item{rm4pl}{numeric; generates theta random deviates.}
 }

\references{
 Blais, J.-G., Raiche, G. and Magis, D. (2009). La detection des patrons de reponses problematiques
 dans le contexte des tests informatises. \emph{In} Blais, J.-G. (Ed.):
 \emph{Evaluation des apprentissages et technologies de l'information et de la
 communication : enjeux, applications et modeles de mesure}. Ste-Foy, Quebec:
 Presses de l'Universite Laval.

 Raiche, G., Magis, D. and Beland, S. (2009). \emph{La correction du resultat d'un etudiant en presence de tentatives de fraudes}.
 Communication presentee a l'Universite du Quebec a Montreal. Retrieved from \url{http://www.camri.uqam.ca/camri/camriBase/}

 Raiche, G., Magis, D. and Blais, J.-G. (2008). \emph{Multidimensional item response theory models integrating additional
 inattention, pseudo-guessing, and discrimination person parameters}. Communication at the annual international
 Psychometric Society meeting, Durham, New Hamshire. Retrieved from \url{http://www.camri.uqam.ca/camri/camriBase/}
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }


\seealso{ \code{\link{grm4pl}}, \code{\link{ggrm4pl}}, \code{\link{pggrm4pl}} }

\examples{
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
 stats <- c(mean=mean(res), sd=sd(res), skewness=skewness(res),
            kurtosis=3-kurtosis(res))
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
 stats <- c(mean=mean(res), sd=sd(res), skewness=skewness(res), kurtosis=3-kurtosis(res))
 print(stats)
 # pdf of this distribution
 theta   <- seq(-4,4,length=100)
 density <- dm4pl(theta, S=S, C=C, D=D)
 print(c(max=max(density)))
 plot(theta, density, type="l")
## ....................................................................
 }

\keyword{ distribution }


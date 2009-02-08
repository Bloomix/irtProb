\name{responsesm4pl}
\alias{grm4pl}
\alias{ggrm4pl}
\alias{pggrm4pl}

\title{ Simulation of Response Patterns and  Computation of the Probability of the Patterns from m4pl}

\description{
  Simulation of response patterns and computation of the probability of the patterns according
  to the multidimensional one, two, three and four person parameters logistic item response models.
 }

\usage{
 grm4pl(N = 10, theta = 0, S = 0, C = 0, D = 0, s = 1/1.702, b = 0, c = 0, d = 1)

 ggrm4pl(n=5,rep=1,theta=0,S=rep(0,length(theta)),C=rep(0,length(theta)),
         D=rep(0,length(theta)),s=rep(1/1.702,n),b=rep(0,n),c=rep(0,n),
         d=rep(1, n))
 
 pggrm4pl(x=ggrm4pl(rep=1),rep=1,n=dim(x)[2],N=dim(x)[1],theta=rep(0,N),
          S=0,C=0,D=0,s=rep(1/1.702,n),b=rep(0,n),c=rep(0,n),d=rep(1,n),
          log.p=FALSE, TCC=FALSE)
 }

\arguments{
  \item{x}{ integer matrix; response patterns (0 or 1). }
  \item{rep}{ numeric; number of replications of the simulation of the response patterns.}
  \item{n}{ numeric; number of items. }
  \item{N}{ numeric; number of response patterns }
  \item{theta}{ numeric; vector of proficiency levels (z sscores). }
  \item{S}{ numeric; person fluctuation parameter. }
  \item{C}{ numeric; person pseud0-guessing parameter. }
  \item{D}{ numeric; person inattention parameter. }
  \item{s}{ numeric; item fluctuation parameters.}
  \item{b}{ numeric; item difficulty parameters. }
  \item{c}{ numeric; item pseudo-guessing parameters. }
  \item{d}{ numeric; item inattention parameters. }
  \item{log.p}{ logical; if TRUE, probabilities p are given as log(p). }
  \item{TCC}{ logical; if TRUE generate the TCC figures for each response patterns. Default FALSE.}
 }
 
\details{
  The function \code{grm4pl} generates \code{N} responses to an item according to the person parameters and the items parameters.
  The funcfion \code{ggrm4pl} will be used to generate \code{rep} respose patterns at \code{n} items. To compute
  the probability of the response patterns, according to known person and item parameters, the function \code{pggrm4pl} will be applied.
 }

\value{
 \item{grm4pl}{ integer; vector of item responses (0 or 1).}
 \item{ggrm4pl}{ integer data.frame; responses for n items.}
 \item{pggrm4pl}{ graphic; if (TCC ==TRUE) return(list(prob=prob, tcc=tcc)).  If (TCC==FALSE) return(prob).}
 }

\references{
 Ferrando, P. J. (2004). Person reliability in personality measurement: an item response theory analysis.
 \emph{Applied Psychological Measurement, 28}(2), 126-140.

 Hulin, C. L., Drasgow, F., and Parsons, C. K. (1983). \emph{Item response theory}. Homewood, IL: Irwin.

 Levine, M. V., and Drasgow, F. (1983). Appropriateness measurement: validating studies and variable
 ability models. \emph{In} D. J. Weiss (Ed.): \emph{New horizons in testing}. New York, NJ: Academic Press.

 Magis, D. (2007). Enhanced estimation methods in IRT. \emph{In} D. Magis (Ed.): \emph{Influence, information and item
 response theory in discrete data analysis}. Doctoral dissertation, Liege, Belgium: University de Liege.

 Trabin, T. E., and Weiss, D. J. (1983). The person response curve: fit of individuals to item response
 theory models. \emph{In} D. J. Weiss (Ed.): \emph{New horizons in testing}. New York, NJ: Academic Press.
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\seealso{
 \code{\link{gr4pl}}, \code{\link{ggr4pl}}, \code{\link{pggr4pl}},
 \code{\link{ctt2irt}}, \code{\link{irt2ctt}}
 }

\examples{
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
 }

\keyword{ distribution }


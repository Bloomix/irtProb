\name{PersonParametersEstimate}
\alias{m4plEstimate}
\alias{m4plEstimateMore}
\alias{m4plPersonParameters}

\title{ Estimation of the Personal Parameters from the mpl4 Logistic Model}

\description{
 Estimation of the Personal Parameters from the mpl4 Logistic Model.
 }

\usage{
 m4plEstimate(        x , s = 1/1.702, b = 0, c = 0, d = 1, m = 0,
                      model = "T", prior = "uniform")
                 
 m4plEstimateMore(    x, s = 1/1.702, b = 0, c = 0, d = 1, m = 0,
                      model = "T", prior = "uniform")

 m4plPersonParameters(x, s = 1/1.702, b = 0, c = 0, d = 1, m = 0,
                      model = "T", prior = "uniform", more = FALSE)
 }

\arguments{
  \item{x}{ integer; vector of item responses for only one subject. Cannot be a matrix for the moment. }
  \item{s}{ numeric; vector of item fluctuation parameter or the inverse of
            item discrimination (s= 1/a). }
  \item{b}{ numeric; vector of item discrimination parameter. }
  \item{c}{ numeric; vector of item pseudo-guessing parameter. }
  \item{d}{ numeric; vector of item inattention parameter. }
  \item{m}{ a priori distribution mean. }
  \item{model}{ character; different combinations of personnal parameters can be
                estimated (\code{"T", "S", "C", "D", "SC", "SD", "CD" or "SCD"}). }
  \item{prior}{ character; a priori distribution can be "uniform" (\code{U(m-4,m+4)})
                or "normal" (\code{N(m,1)}). }
  \item{more}{ logical: if \code{TRUE} use \code{m4plEstimateMore}, if \code{FALSE} use \code{m4plEstimate}. }
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
 Function \code{m4plEstimate}
  \item{m4plEstimate }{ numeric; return a vector of personal parameters estimates.}
 Function \code{m4plEstimateMore}
  \item{res }{ numeric; return a vector of personal parameters estimates.}
  \item{se }{ numeric; return a vector of standard errors.}
  \item{corr }{ numeric; return a correlation matrix between the estimated parameters.}
  \item{llikelihood }{ numeric; return a vector of -log likelihood.}
 Function \code{m4plPersonParameters}
  Values returned are from \code{m4plEstimate} or \code{m4plEstimateMore} depending on the \code{more} condition.
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
 round(c(mean=mean(results, na.rm=TRUE)),2); round(c(se=sd(results, na.rm=TRUE)),2)

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
 round(c(mean=mean(results, na.rm=TRUE)),2); round(c(se=sd(results, na.rm=TRUE)),2)
 ## Average correlation between the person parameters
 nParameters <- length(personParameters[1,])
 personCor   <- unlist(lapply(tests,extract,3))
 personCor   <- matrix( mean(data.frame(t(matrix(personCor,ncol=rep))), na.rm=TRUE),
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
 round(c(mean=mean(results, na.rm=TRUE)),2); round(c(se=sd(results, na.rm=TRUE)),2)
 ## Average correlation between the person parameters
 nParameters <- length(personParameters[1,])
 personCor   <- unlist(lapply(tests,extract,3))
 personCor   <- matrix( mean(data.frame(t(matrix(personCor,ncol=rep))), na.rm=TRUE),
                ncol=nParameters)
 personCor
 # ....................................................................
 }

\keyword{ distribution }


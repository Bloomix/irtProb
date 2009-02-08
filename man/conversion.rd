\name{conversion}
\alias{ctt2irt}
\alias{irt2ctt}
\title{ Functions to Convert from and to CTT and IRT Models }

\description{
 \command{ctt2irt} and \command{irt2ctt} are converter functions to change the parametrization of item parameters
 from and to classical test theory (difficulty and discrimination parameters) and item response theory (difficulty
 and discrimination parameters). Consequently, the conversion is only valid between ctt and 2 parameters logistic or normal models.
 }

\usage{
ctt2irt(rpbis = 0.7071068, difficulty = 0.5)

irt2ctt(a = 1, b = 0, c = 0, d = 1, model = "LOGISTIC")
}

\arguments{
  \item{rpbis}{ numeric; vector of discrimination parameters: point biserial correlation between the item response and the total score. }
  \item{difficulty}{ vector of difficulty parameters: proportion of corrected responses. }
  \item{a}{ numeric; vector of discrimination parameters.}
  \item{b}{ numeric; vector of difficulty parameters. }
  \item{c}{ numeric; vector of pseudo-guessing parameters (not used for the moment). }
  \item{d}{ numeric; vector of inattention parameters (not used for the moment). }
  \item{model}{ character; if NORMAL the constant D (1.702) is used. Default to LOGISTIC with constant D=1.  }
}

\details{
  Eventually the 3 and 4 parameters logistic and normal models will be taken in account according to Urry approximation (1974).
}

\value{
\item{For ctt2irt}{...................................}
  \item{note }{ character; warnings about the use of the \code{c} and \code{d} item parameters.}
  \item{normal.parameters }{ numeric; vector returning difficulty \code{b} and discrimination \code{a} parameters from the normal model.}
  \item{irt.parameters }{ numeric; vector returning difficulty \code{b} and discrimination \code{a} parameters from the logistic model.}
  
\item{For irt2ctt}{...................................}
  \item{note }{ character; warnings about the use of the \code{c} and \code{d} item parameters.}
  \item{normal.parameters }{ numeric; vector returning difficulty \code{p} and discrimination \code{rpbis} parameters from the normal model.}
  \item{irt.parameters }{ numeric; vector returning difficulty \code{p} and discrimination \code{rpbis} parameters from the logistic model.}
}

\references{
Lord, F. M. and Novick, M. R. (1968). \emph{Statistical theories of mental test
scores, 2nd edition}. Reading, Massacusett: Addison-Wesley.

Urry, V. W. (1974). Approximations to item parameters of mental tests models and their uses.
\emph{Educational and psychological measurement, 34}, 253-269.
}

\author{
Gilles Raiche, Universite du Quebec a Montreal (UQAM),

Departement d'education et pedagogie

\email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
}


\seealso{
\code{\link{gr4pl}}, \code{\link{ggr4pl}}, \code{\link{ctt2irt}}, \code{\link{irt2ctt}}
}

\examples{
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
}

\keyword{ distribution }

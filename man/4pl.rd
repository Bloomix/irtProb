\name{4pl}
\alias{p4pl}
\alias{q4pl}
\alias{d4pl}
\alias{r4pl}
\title{ One, Two, Three and Four Parameters Logistic Distributions }

\description{
Density, distribution function, quantile function and random generation for the one,
two, three and four parameters logistic distributions.
}

\usage{
p4pl(theta = 0, a = 1, b = 0, c = 0, d = 1, lower.tail = TRUE,
log.p = FALSE)

d4pl(theta = 0, a = 1, b = 0, c = 0, d = 1,
log.p = FALSE)

q4pl(p  = 0.05, a = 1, b = 0, c = 0, d = 1, lower.tail = TRUE,
log.p = FALSE)

r4pl(N  = 100,  a = 1, b = 0, c = 0, d = 1)
}

\arguments{
  \item{N}{ numeric; number of observations. }
  \item{p}{ numeric; vector of probability. }
  \item{theta}{ numeric; vector of person proficiency levels scaled on a normal \code{z} score. }
  \item{a}{ numeric; positive vector of item discrimination parameters.}
  \item{b}{ numeric; vector of item difficulty parameters.}
  \item{c}{ numeric; positive vector of item pseudo-guessing parameters (a probability between 0 and 1).}
  \item{d}{ numeric; positive vector of item inattention parameters (a probability between 0 and 1).}
  \item{lower.tail}{ logical; if TRUE (default), probabilities are \eqn{P(X_{j} <= x_{ij})}, otherwise, \eqn{P(X_{j} > x_{ij})}.}
  \item{log.p}{ logical; if TRUE probabilities p are given as log(p).}
}

\details{
 The 4 parameters logistic distribution (cdf) is equal to:
 \deqn{
P(x_{ij}  = 1|\theta _j ,a_i ,b_i ,c_i ,d_i ) = c_i  + \frac{{d_i  - c_i }}{{1 + e^{ - Da_i (\theta _j  - b_i )} }},
}
where the parameters are defined in the section arguments and \emph{i} and \emph{j} are respectively
the items and the persons indices. A normal version of the 4PL model was described by McDonald (1967, p. 67), Barton and Lord (1981),
like Hambleton and Swaminathan (1985, p. 48-50).
}

\value{
  \item{p4pl}{numeric; gives the distribution function (\code{cdf}).}
  \item{d4pl}{numeric; gives the density (derivative of p4pl).}
  \item{q4pl}{numeric; gives the quantile function (inverse of p4pl).}
  \item{r4pl}{numeric; generates theta random deviates.}
}

\references{
Barton, M. A. and Lord, F. M. (1981). \emph{An upper asymptote for the tree-parameter logistic item-response model}.
Research Bulletin 81-20. Princeton, NJ: Educational Testing Service.

Hambleton, R. K. and Swaminathan, H. (1985). \emph{Item response theory - Principles and applications}.
Boston, Massachuset: Kluwer.

Lord, F. M. and Novick, M. R. (1968). \emph{Statistical theories of mental test scores, 2nd edition}.
Reading, Massacusett: Addison-Wesley.

McDonald, R. P. (1967). Non-linear factor analysis. \emph{Psychometric Monographs, 15}.
}

\author{
Gilles Raiche, Universite du Quebec a Montreal (UQAM),

Departement d'education et pedagogie
 
\email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
}

\note{  Code inspired by the \code{pnorm} function structure from the R base package. }

\seealso{ \code{\link{gr4pl}}, \code{\link{ggr4pl}}, \code{\link{ctt2irt}}, \code{\link{irt2ctt}} }

\examples{
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
}

\keyword{ distribution }


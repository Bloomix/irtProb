\name{m4plSummary}
\alias{m4plSummary}
\alias{m4plMoreSummary}
\alias{m4plNoMoreSummary}

\title{ Summary of the Results of Estimation with the m4pl Models }

\description{
  Summary of the results of estimation with the m4pl models.
 }

\usage{
 m4plSummary(      X, ...)

 m4plMoreSummary(  x, out = "result", thetaInitial = NULL)

 m4plNoMoreSummary(x)
 }

\arguments{
  \item{X}{ data.frame or list: if a \code{list}  results from
            \code{m4plPersonParameters} function, if a \code{data.frame},
            any all numeric \code{data.frame}. }
  \item{x}{ list: result from \code{m4plPersonParameters} with \code{more} set to \code{TRUE}.}
  \item{out}{ character: if \code{out="results"}, the output is for each subjects.
              If \code{out="report"}, statistics on all results are computed.}
  \item{thetaInitial}{ numeric: if initial theta valeus are used
                       the error of estimation is also reported. }
  \item{...}{ generic: to be able to pass parameters from the \code{m4plMoreSummary} function. }
 }

\value{
..............

\code{m4plSummary}

 ..............

 The result of \code{m4plSummary} depends of the \code{out} condition and the class of \code{X}.
 If \code{X} is a \code{data.frame}, the function \code{m4plNoMoreSummary} is called
 and a data.frame with 2 rows is returned: mean and sd rows.

 If \code{out="result"} and \code{X} is a list, the function \code{m4plMoreSummary} is called
 and a data.frame with the mean of the parameters and their theoretical standard errors is returned:

 If \code{out="report"} and \code{X} is a list, \code{m4plMoreSummary} is called and the following
 list taking in account each parameters is returned:

  \item{parameters }{data.frame: with mean, median, sd an N observations for each parameters.}
  \item{se }{data.frame: with mean, median, sd an N observations for the theoretical
             values of the standard error for each parameters.}
  \item{logLikelihood }{data.frame: mean, median, sd an N observations loglikelihood,
                        AIC and BIC for the model.}
  \item{eCorrelation }{matrix: empirical correlations between the parameters.}
  \item{tCorrelation }{matrix: theoretical correlations between the parameters.}
  
 ..............

 \code{m4plNoMoreSummary}

 ..............

 A data.frame with 2 rows is returned: mean and sd rows.

 ..............

 \code{m4plMoreSummary}

 ..............

 All other outputs from the \code{m4plSummary} function.
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

\seealso{ \code{\link{m4plPersonParameters}}}

\examples{

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
}

\keyword{ distribution }


\name{m4plModelShow}
\alias{m4plModelShow}

\title{ Results For Each Subject To Each Model}

\description{
  Show all the information about the estimation of all the possible m4pl models
  for each subjects.
 }

\usage{
 m4plModelShow(x, ...)
 }

\arguments{
  \item{x}{ data.frame: a matrix of binary 0-1 item responses. }
  \item{\dots}{ varying: parameters to be passed to the
                \code{m4plPersonParameters} function.}
 }

\value{
  \item{ID }{integer: subject identificator.}
  \item{MODEL }{charavter: model identification (T,TS,TC,TD,TSC,TSD,TCD or TSCD)}
  \item{LL }{numeric: loglikelihood.}
  \item{AIC }{numeric: Akaike information criteria.}
  \item{BIC }{numeric: Bayes (Schwartz) information criteria.}
  \item{T }{numeric: theta parameter value.}
  \item{SeT }{numeric: theta parameter theoretical standard error.}
  \item{S }{numeric: person fluctuation parameter value.}
  \item{SeS }{numeric: person fluctuation theoretical standard error}
  \item{C }{numeric: person pseudo-guessing parameter value.}
  \item{SeC }{numeric: person pseudo-guessing theoretical standard error}
  \item{D }{numeric: person inattention parameter value.}
  \item{SeD }{numeric: person inattention theoretical standard error}
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
 
  Raiche, G., Magis, D., Blais, J.-G., and Brochu, P. (2013). Taking atypical response patterns into account: a multidimensional measurement model from item response theory. In M. Simon, K. Ercikan, and M. Rousseau (Eds), \emph{Improving large-scale assessment in education}. New York, New York: Routledge.
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\seealso{ \code{\link{m4plPersonParameters}}}

\examples{
\dontrun{
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
}
}

\keyword{ distribution }


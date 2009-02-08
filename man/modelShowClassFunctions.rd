\name{modelShowClassFunctions}
\alias{is.modelShow}
\alias{Round}
\alias{Round.modelShow}
\alias{summary.modelShow}

\title{ Functions To Manipulate modelShow Class Objects }

\description{
  A set of functions to manipulate modelShow class objects.
  \code{is.modelShow} is used to verify if the object correspond to a \code{modelShow} class object.
  \code{Round.modelShow}is used to control the number of decimals of the report.
  Finally, \code{summary.modelShow} produces different summary reports according th the \code{report} value.
 }

\usage{
 \method{is}{modelShow}(x)

 Round(x, digits)

 \method{Round}{modelShow}(x, digits = 6)

 \method{summary}{modelShow}(object, ..., report="means",
                             statistics=c("LL","BIC","T"),
                             criteria="LL", digits=6, tol=0.20, color="grey")
 }

\arguments{
  \item{x}{ modelShow: modelShow object. }
  \item{object}{ modelShow: modelShow object. }
  \item{digits}{ numeric: number of report decimal digits. }
  \item{report}{ character: summary type to be reported (means, choose, table or add). }
  \item{statistics}{ character: a vector of variables for which means of statistics
                     from \code{m4plModelShow} will be computed.}
  \item{criteria}{ character: criteria used to choose between models (LL, AIC or BIC). }
  \item{tol}{ numeric: tolerance around the choose criteria, so that more models can be considered. }
  \item{color}{ character: color of the bars if a histogram is choosen for the report. }
  \item{...}{ generic: to be able to pass parameters from the generic \code{summary} function. }
 }

\value{
 Generic functions for the modelShow class:

  \item{is.modelShow}{ logical: is the object of the class modelShow? }
  \item{Round.modelShow }{ data.frame: return the modelShow object results rounded to \code{digits} decimals.}
  
 Different reports returned by \code{summary.modelShow}

  \item{report="means"}{ data.frame: means of the \code{statistics} choosen for each models. }
  \item{report="choosen"}{ list: which model is choosen for each subject. }
  \item{report="table"}{ table: table of frequencies of each model chhosen. }
  \item{report="histogram"}{ histogram: histogram of frequencies of each model choosen. }
  \item{report="add"}{ data.frame: the choosen model is added to the essai modelShow object. }
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\seealso{ \code{\link{m4plModelShow}} }

\examples{
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
 }

\keyword{ distribution }


\name{graphics}
\alias{PCC}

\title{ Graphic Functions to Illustrate Response Curves and Parameter Estimation}

\description{
 Graphic functions to illustrate response curves and parameter estimation.
 }
 
\usage{
PCC(theta  = 0, S = 0, C = 0, D = 0,
    s      = 1/1.702, b = seq(-5, 5, length = 300), c = 0, d = 1,
    groups = TRUE, ID = "ID",
    main   = "Person Characteristic Curve",
    xlab   = "Item Difficulty Parameter (b)", ylab = "P(x = 1)",
    type   = c("g", "a"))
 }

\arguments{
  \item{theta}{ numeric; vector of person proficiency (\eqn{\theta}) levels scaled on a normal \code{z} score. }
  \item{S}{ numeric: positive vector of personal fluctuation parameters (\eqn{\sigma}). }
  \item{C}{ numeric: positive vector of personal pseudo-guessing parameters (\eqn{\chi}, a probability between 0 and 1). }
  \item{D}{ numeric: positive vector of personal inattention parameters (\eqn{\delta}, a probability between 0 and 1). }
  \item{s}{ numeric: vector of item fluctuation parameter or the inverse of
            item discrimination (s= 1/a). }
  \item{b}{ numeric: vector of item discrimination parameter. }
  \item{c}{ numeric: vector of item pseudo-guessing parameter. }
  \item{d}{ numeric: vector of item inattention parameter. }
  \item{ID}{ character: curves identification information displayed ("ID", "ALL", "THETA2 or NULL)}
  \item{groups}{ logical: default to \code{TRUE}. If \code{TRUE}, Lattice \code{xyplot} by groups.
                 If \code{FALSE}, \code{xyplot} with shingles. }
  \item{main}{ character: first line of main title. }
  \item{xlab}{ character: label of x axis. }
  \item{ylab}{ character: label of y axis.  }
  \item{type}{ character: type of \code{xyplot} graphic. One of the following:
               "p", "l", "h", "b", "o", "s", "S", "r", "a", "g", "smooth".}
 }


\value{
\code{PCC} returns a list:
  \item{ graphic }{ trellis object: figures for each subject (group or shingle representation). }
  \item{ probability }{ data.frame: item snd person parameters, like th eprobability of a correct response.}
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\examples{
## PCC curves grouped on a single figure
 res1 <- PCC(theta=c(-2,-2,-2),S=0, C=c(0.0, 0.1, 0.6), D=0.2,
             b=seq(-5,5,length=3000), ID=NULL, groups=TRUE, type=c("g","a"))
 res1
 
## PCC curves shingled on a single figure for each subject
 res2 <- PCC(theta=c(-2,-1,0),S=c(4.0,0.0, 1.0), C=c(0.0, 0.1, 0.6), D=0.2,
             b=seq(-5,5,length=3000), ID=NULL, groups=FALSE, type=c("g","a"))
 res2
 }

\keyword{ graphs }


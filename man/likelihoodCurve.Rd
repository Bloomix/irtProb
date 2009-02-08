\name{likelihoodCurve}
\alias{likelihoodCurve}
\alias{groupLikelihoodCurves}

\title{ Functions to Graph m4pl Likelihood Curves }

\description{
  \code{likelihoodCurve} and \code{groupLikelihoodCurve} are used to graph the
  likelihood function curves according to the only theta, theta anc pseudo-guessing,
  theta and fluctuation, like theta and inattention m4pl models: only two simultaneous
  person parameters are taken in account.
 }

\usage{
 likelihoodCurve(x, s, b, c, d,
                 limitT = c(min = -4, max = 4), limitS = c(min = 0, max = 4),
                 limitC = c(min = 0, max = 1), limitD = c(min = 0, max = 1),
                 grain         = 150, annotate = TRUE,
                 logLikelihood = FALSE, color = TRUE,
                 main          = "Likelihood Curve",
                 xlab          = expression(theta), ylab = NULL, zlab = "P(X)",
                 type          = "levelplot", m = 0)

 groupLikelihoodCurves(plotT, plotS, plotC, plotD, main=NULL, cex=0.7)
 }

\arguments{
  \item{x}{ numeric: binary (0,1) response pattern. }
  \item{s}{ numeric: vector of inverse a discrimination item parameters. }
  \item{b}{ numeric: vector of b difficulty item parameters. }
  \item{c}{ numeric: vector of c pseudo-guessing item parameters. }
  \item{d}{ numeric: vector of d inattention item parameters. }
  \item{limitT}{ numeric: minimum and maximum of the proficiency person parameter used for the x axis. }
  \item{limitS}{ numeric: minimum and maximum of the fluctuation person parameter used for the y axis. }
  \item{limitC}{ numeric: minimum and maximum of the pseudo-guessing person parameter used for the y axis. }
  \item{limitD}{ numeric: minimum and maximum of the inattention person parameter used for the y axis. }
  \item{grain}{ numeric: number of theta values  used to compute pattern distribution probability. }
  \item{annotate}{ logical: does annotation is applied to the graphs? }
  \item{logLikelihood}{ numeric: data.frame of the log likelihood of the studied models. }
  \item{color}{ logical: does color is applied to contourplot or wireframe. }
  \item{main}{ character: main title. }
  \item{xlab}{ character: x axis label. }
  \item{ylab}{ character: y axis label. }
  \item{zlab}{ character: z axis label. }
  \item{type}{ character: type of 3D plot ("levelplot", "contourplot" or "wireframe"). }
  \item{m}{ numeric: mean of the a priori probability distribution. }
  \item{plotT}{ trellis: 2D theta likelihood curve. }
  \item{plotS}{ trellis: 3D theta * S likelihood curve. }
  \item{plotC}{ trellis: 3D theta * C likelihood curve. }
  \item{plotD}{ trellis: 3D theta * D likelihood curve. }
  \item{cex}{ numeric: zaxis label size. }
 }

\value{
 \code{likelihoodCurve}
   \item{plotT}{ trellis: theta likelihood functions curves. }
   \item{plotS}{ trellis: theta * S likelihood functions curves. }
   \item{plotC}{ trellis: theta * C likelihood functions curves. }
   \item{plotD}{ trellis: theta * D likelihood functions curves. }
   \item{parameters}{ numeric: list of data.frame of person parameters for each model
         studied. Each element of the list shows estimation with different a priori
         probability distributions (uniform, normal and none). }
   \item{logLikelihood}{ numeric: data.frame of the log likelihood for each model studied. }

  
 \code{groupLikelihoodCurves}
   \item{graphic}{graphic: all the likelihood functions curves are displayed. }
 }

\author{
 Gilles Raiche, Universite du Quebec a Montreal (UQAM),

 Departement d'education et pedagogie

 \email{Raiche.Gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/}
 }

\examples{
 ## SIMULATION OF A RESPONSE PATTERN WITH 60 ITEMS
 nItems <- 60
 a      <- rep(1.702,nItems); b <- seq(-4,4,length=nItems)
 c      <- rep(0,nItems);     d <- rep(1,nItems)
 nSubjects <-  1
 theta     <-  -1
 S         <-  0.0
 C         <-  0.5
 D         <-  0.0
 
 set.seed(seed = 100)
 x         <- ggrm4pl(n=nItems, rep=1,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)

 ## Likelihood curves, person parameters estimates
  # and log likelihood of models graphed
 test <- likelihoodCurve(x=x, s=1/a, b=b, c=c, d=d, color=TRUE,
                         main="Likelihood Curve",
                         xlab=expression(theta), ylab=NULL, zlab="P(X)",
                         type="wireframe" , grain=50, limitD=c(0,1),
                         logLikelihood=FALSE, annotate=TRUE )

 # Contentd of the object test
 test$plotT
 test$plotC
 test$plotS
 test$plotD
 test$par
 round(test$logLikelihood,2)

 ## Graph of all the likelihood function curves
 groupLikelihoodCurves(test$plotT, test$plotS, test$plotC, test$plotD,
                       main=NULL, cex=0.7)
 }

\keyword{ multivariate }


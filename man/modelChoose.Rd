\name{modelChoose}
\alias{meanModels}
\alias{modelChoose}
\alias{modelChooseAdd}

\title{ Functions For Choosing The m4pl Best Model(s) }

\description{
  Functions to help to choose the best models from the m4pl person response models family (Raiche et al., 2013).
  The function \code{meanModels} gives the mean of selected and available statistics
  from a \code{m4plModelShow} result. The function \code{modelChoose} shows which
  model(s) are choosen for each subject, while the function \code{modelChooseAdd}
  adds a supplementary logical variable indicating which model(s) are choosen for each subject
  to the data.frame returned previously by a \code{m4plModelShow} call  .
 }

\usage{
 meanModels(    modelShow, statistics=c("T","S"))

 modelChoose(   modelShow, criteria = "BIC", tol = 0.2)

 modelChooseAdd(modelShow, criteria="LL")
 }

\arguments{
  \item{modelShow}{ data.frame: result returned by a \code{m4plModelShow} call. }
  \item{criteria}{ character: criteria used to choose between models (LL, AIC or BIC). }
  \item{tol}{ numeric: tolerance around the choose criteria, so that more models can be considered. }
  \item{statistics}{ character: a vector of variables for which means of statistics
                     from \code{m4plModelShow} will be computed.}
 }

\details{
  A tolerance varying between 0.10 and 0.20 is suggested. Lower the value, less models are choosen.
 }

\value{
  \item{meanModels }{data.frame: this function return a data.frame of means for each choosen variables.}
  \item{modelChoose }{list: return a list of the models(s) choosen for each subject.}
  \item{modelChooseAdd }{data.frame: return a data.frame adding a logical indicator of the choosen model(s).}
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

\seealso{ \code{\link{m4plModelShow}} }

\examples{
\dontrun{
## GENERATION OF VECTORS OF RESPONSE
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
 D         <- runif(n=nSubjects,min=0,max=0)
 set.seed(seed = 100)
 X         <- ggrm4pl(n=nItems, rep=rep,
                      theta=theta, S=S, C=C, D=D,
                      s=1/a, b=b,c=c,d=d)

## Results for each subjects for each models
 essai <- m4plModelShow(X, b=b, s=1/a, c=c, d=d, m=0, prior="uniform")

## Means of choosen variables from m4plModleShow previous call
 round(meanModels(essai, statistic=c("LL","BIC","T","C")), 2)

## Which model(s) are the best for each subject (LL criteria)
 res <- modelChoose(essai, criteria = "LL", tol = 0.2); res
 
## Which model(s) are the best for each of the first 20 subjects (BIC criteria)
 res <- modelChoose(essai[which(essai$ID < 20) ,], criteria="BIC"); res

## A look at the 15th subject parameters estimation for each models
 essai[which(essai$ID == 15) ,]
 
## Add the logical critLL variable to the data frame
 criteria <- "LL"
 res1    <- modelChooseAdd(essai, criteria=criteria)
 # Create a charcater string composed from "crit" and criteria
 crit    <- paste("crit",criteria,sep="")
 # To Show only the lines where the models is choosen according to critLL
 res2    <- res1[which(res1[crit] == TRUE),]; mean(res2$T);sd(res2$T,na.rm=TRUE);res2
 # Give the mean for choosen variables from m4plModelShow
 meanModels(essai, statistic=c("LL","BIC","T","SeT","S","C","D"))
 # Tabulate only the choosen models
 table(res2$MODEL)
 # Show the information for the subject for which the model TSCD was choosen
 res2[which(res2$MODEL == "TSCD") ,]
 # Show only the results for the 5th subject, the with the TSCD model choosen
 res1[which(res1$ID == 5) ,]
 # Same, but without critLL
 essai[which(essai$ID == 5) ,]

## Simulation whith cheating
 # High proficiency students responding at random to 20% of the first items
 # easiest ones)
 XHProficiency             <- X
 pourcHasard               <- 0.20; nHasard <- abs(dim(X)[2]*pourcHasard)
 XHProficiency[,1:nHasard] <- rbinom(dim(X)[1]*nHasard, 1, 0.5)
 XHProficiency             <- m4plModelShow(XHProficiency, b=b, s=1/a, c=c, d=d,
                                            m=0, prior="uniform")
 XHProficiency             <- modelChooseAdd(XHProficiency, criteria=criteria)
 XHProficiency             <- XHProficiency[which(XHProficiency$critLL == TRUE),]
 mean(XHProficiency$T);sd(XHProficiency$T,na.rm=TRUE);XHProficiency[1:10,]
 meanModels(XHProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XHProficiency$MODEL)
 
 # Low proficiency students responding at random to 20% of the last items
 # (more difficult ones)
 XLProficiency                             <- X
 pourcHasard                               <- 0.20
 nHasard                                   <- abs(dim(X)[2]*pourcHasard)
 XLProficiency[,(nItems-nHasard+1):nItems] <- rbinom(dim(X)[1]*nHasard, 1, 0.5)
 XLProficiency                             <- m4plModelShow(XLProficiency, b=b,
                                              s=1/a, c=c, d=d, m=0, prior="uniform")
 XLProficiency                             <- modelChooseAdd(XLProficiency,
                                                             criteria=criteria)
 XLProficiency <- XLProficiency[which(XLProficiency$critLL == TRUE),]
 mean(XLProficiency$T);sd(XLProficiency$T,na.rm=TRUE);XLProficiency[1:10,]
 meanModels(XLProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XLProficiency$MODEL)

 # High proficiency students giving incorrect responses to 20% of the first items
 # (easiest ones)
 XHProficiency             <- X
 pourcCheat                <- 0.20; nCheat  <- abs(dim(X)[2]*pourcCheat)
 XHProficiency[,1:nCheat]  <- rep(0, dim(X)[1]*dim(X)[2]*pourcCheat)
 XHProficiency             <- m4plModelShow(XHProficiency, b=b, s=1/a, c=c, d=d,
                                            m=0, prior="uniform")
 XHProficiency             <- modelChooseAdd(XHProficiency, criteria=criteria)
 XHProficiency             <- XHProficiency[which(XHProficiency$critLL == TRUE),]
 mean(XHProficiency$T);sd(XHProficiency$T,na.rm=TRUE);XHProficiency[1:10,]
 meanModels(XHProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XHProficiency$MODEL)
 
 # Low proficiency students giving correct responses to 20% of the last items
 # (more difficult ones)
 XLProficiency                             <- X
 pourcCheat                                <- 0.20
 nCheat                                    <- abs(dim(X)[2]*pourcCheat);
 XLProficiency[,(nItems-nCheat+1):nItems]  <- rep(1, dim(X)[1]*dim(X)[2]*pourcCheat)
 XLProficiency                             <- m4plModelShow(XLProficiency, b=b,
                                              s=1/a, c=c, d=d, m=0, prior="uniform")
 XLProficiency                             <- modelChooseAdd(XLProficiency,
                                                             criteria=criteria)
 XLProficiency <- XLProficiency[which(XLProficiency$critLL == TRUE),]
 mean(XLProficiency$T);sd(XLProficiency$T,na.rm=TRUE);XLProficiency[1:10,]
 meanModels(XLProficiency, statistic=c("LL","BIC","T","SeT","S","C","D"))
 table(XLProficiency$MODEL)
 }
 }

\keyword{ distribution}


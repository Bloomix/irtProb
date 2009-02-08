`irt2ctt` <-
function(a=1, b=0, c=0, d=1, model="LOGISTIC") {
 ac                <- a
 note              <- NULL
 if (model=="LOGISTIC")  ac <- a/1.702
 if (c>0 || d>1) note = noquote("Considering the values of c and d parameteers, the rpbis value is not valid, nor the normal.parameters vector.")
 # rbis and p values approximated for a normal population distribution N(0,1)
 rpbis             <- ac/sqrt(1 + ac^2)                             # Equation 3-8
 tc                <- ifelse(model=="NORMAL",rpbis*b,b)             # Transformation de la fonction 3-10
 sd                <- ifelse(model=="NORMAL",1,1.702/a)             # Correction de Haley pour la corrspondance entre la normale et la logistique
 pNormal           <- 1 - pnorm(tc, mean=0, sd=sd, lower.tail=TRUE) # Fonction 3-9
 # p values approximated from a 4PL population distribution with theta fixed at 0
 ti                <- ifelse (model=="LOGISTIC",b,tc)
 a                 <- ifelse(model=="LOGISTIC",a,a*1.702)           # La constante 1.702 de Haley est utilisée pour approximer la distribution N(0,1)  (voir Baker)
 pIrt              <- 1 - p4pl(theta=ti,a=a,b=0,c=c,d=d)
 # mRel              <- marginal.reliability(a=a,b=b,c=c,d=d)       # Eventually rbis will be approximated by marginal reliability (Wainer, p. 165)#
 parameters.normal <- c(rpbis=rpbis, difficulty=pNormal)            # Paramètres d'items si la distribution de l'habileté est nomale N(0,1) dans la population
 parameters.irt    <- c(rpbis=rpbis, difficulty=pIrt)               # Paramètres d'items si la distribution de l'habileté est logistique dans la population
 result            <- list(note=note, normal.parameters=parameters.normal, irt.parameters=parameters.irt)
 return(result)
 }


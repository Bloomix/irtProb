`ctt2irt` <-
function(rpbis=0.7071068, difficulty=0.5) {
 note              <- noquote("For the moment, c and d parameters don't seems possible to be recoverd from p and rpbis. These models cannot be compared for the moment.")
 # a and b values approximated for a normal population distribution N(0,1)
 t                 <- qnorm(1-difficulty, mean = 0, sd = 1, lower.tail = TRUE)  # From equation 3-9
 a                 <- rpbis/sqrt(1 - rpbis^2)
 b                 <- t/rpbis                                                   # Equation 3-10
 parameters.normal <- c(a=a, b=b)                                               # Paramètres d'items si la distribution de l'habileté est normale N(0,1) dans la population et le modèle IRT utilisé est NORMAL
 parameters.irt    <- c(a=a*1.702, b=b)                                         # Paramètres d'items si la distribution de l'habileté est normale N(0,1) dans la population et le modèle IRT utilisé est LOGISTIC
 result            <- list(note=note, normal.parameters=parameters.normal, irt.parameters=parameters.irt)
 return(result)
 }


`r4pl` <-
function(N=100,a=1,b=0,c=0,d=1) log(abs((d-c)/(runif(N)-c) - 1))/-a + b


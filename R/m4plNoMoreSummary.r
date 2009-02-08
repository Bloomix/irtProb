`m4plNoMoreSummary` <-
function(x){
 stat1 <- mean(x, na.rm=TRUE)
 stat2 <- sd(  x, na.rm=TRUE)
 res <- data.frame(t(data.frame(mean=stat1,sd=stat2)))
 return(res)
 }

#' @export
vecapply <- function(x=list(),FUN){
    temp <- 0
    for (i in 1:length(x)) temp <- temp + x[[i]]
    for (i in 1:length(x)) x[[i]] <- x[[i]] + temp - temp
    return(apply(t(matrix(unlist(x),length(x[[1]]))),2,FUN))
}

# daily_demand <- rnorm(100,100,40)
# vecapply(list(daily_demand,100),min)


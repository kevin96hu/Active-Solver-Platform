#' Apply a function over two or more vectors with different length
#'
#' vecapply returns a list of the same length as the biggest length among all
#' vectors, each elament of which is the result of applying FUN to each element
#' of all vectors
#'
#' @param x list, including two or more vectors
#' @param FUN the function to be applied to each element of x
#'
#' @author Kevin Hu \email{kevinhu@bu.edu}
#'
#' @example
#'
#' daily_demand <- rnorm(100,100,40)
#' vecapply(list(daily_demand,100),min)
#'
#' @export
vecapply <- function(x=list(),FUN){
    temp <- 0
    for (i in 1:length(x)) temp <- temp + x[[i]]
    for (i in 1:length(x)) x[[i]] <- x[[i]] + temp - temp
    return(apply(t(matrix(unlist(x),length(x[[1]]))),2,FUN))
}


gicdf <- function(fun,
                  min=-3.5,
                  max=3.5,
                  bins=1000,
                  nqratio=10,
                  grouping=mean,
                  ...) {
    # Generate an inverse CDF of an arbitrary function
    fun <- match.fun(fun)
    grouping <- match.fun(grouping)

    # Number of points to draw
    nq=nqratio*bins

    # Draw indexes
    qdraw <- seq(min, max,length.out=nq)

    # Calculate proportional probability of each draw
    pdraw <- fun(qdraw,...)

    # Rescale probability sum to 1, rescale
    pdraw <- pdraw/sum(pdraw)

    # Calculate the cumulative probability at each qdraw
    cpdraw <- cumsum(pdraw)

    # Caculate the cumulative probability at each bin
    pbin <- (1:bins)/bins
    xbin <- NA*(1:bins)

    for (i in 1:bins) {
        xbin[i] <- grouping(qdraw[cpdraw<pbin[i]&cpdraw>0])
        cpdraw[cpdraw<pbin[i]] <- 2
    }

    (draw.set <- list(digits=floor(log10(bins)), xbin=xbin, pbin=pbin))
}

# Draw from acdf
ricdf <- function(N, draw.set) {
    digits <- draw.set$digits
    pdraws <- ceiling(runif(N)*10^digits)/10^digits
    draw.set$xbin[match(pdraws,draw.set$pbin)]
}

#' generate random numbers
#'
#' distribution helps generate random numbers with different diatributions
#' It includes uniform, normal, gamma, triangular, cumulative distributions.
#' confidence interval of a mean value, percentile of a number among a
#' list of numbers and the least number of trials for a specific width of confidence
#' interval are included
#'
#' @param n number of observations
#' @param min the lower bound of a distribution
#' @param max the higher bound of a diatribution
#' @param mean vector of means
#' @param stdev vector of standard deviations
#' @param shape,scale shape and scale parameters. Must be positive, scale strictly
#' @param likely the most likely number for triangular diatribution
#' @param range a vector for breaks of a distribution
#' @param prob cumulative probability for each breaks
#' @param alpha confidence degree
#' @param width required width for confidence interval
#'
#' @author Kevin Hu \email{kevinhu@bu.edu}
#'
#' @examples
#' a <- triangular(10000,100,130,200)
#' b <- cumul(1000,3,10,c(4,6),c(0.5,0.6))
#' meanci(100,20,1000,alpha=0.9)
#' target(b,0.8)
#' citrials(20,1,alpha=0.9)
#'
#'
#' @rdname distribution
#' @export
uniform <- function(n,min,max){
    return(runif(n,min,max))
}

#' @rdname distribution
#' @export
normal <- function(n,mean,stdev){
    return(rnorm(n,mean,stdev))
}

#' @rdname distribution
#' @export
gamma <- function(n,shape,scale){
    return(rgamma(n,shape,1/scale))
}

#' @rdname distribution
#' @export
triangular <- function(n,min,likely,max){
    f <- function(x) {
        p <- rep(0, length(x))
        p[x>min&x<likely] <- (2/(max-min)/(likely-min))*x[x>min&x<likely]-(2/(max-min)/(likely-min))*min
        p[x>likely&x<max] <- -(2/(max-min)/(max-likely))*x[x>likely&x<max]+(2/(max-min)/(max-likely))*max
        p
    }
    myicdf <- gicdf(f,min=min,max=max, bins=1000)
    samples <- ricdf(n,myicdf)
    return(samples)
}

#' @rdname distribution
#' @export
cumul <- function(n,min,max,range,prob){
    f <- function(x) {
        p <- rep(0, length(x))
        p[x>min&x<range[1]] <- prob[1]/(range[1]-min)
        for (i in 2:length(range)){
            p[x>range[i-1]&x<range[i]] <- (prob[i]-prob[i-1])/(range[i]-range[i-1])
        }
        p[x>range[length(range)]&x<max] <- (1-prob[length(range)])/(max-range[length(range)])
        p
    }
    myicdf <- gicdf(f,min=min,max=max, bins=1000)
    samples <- ricdf(n,myicdf)
    return(samples)
}


# psipercentile <- function(data,p){
#     return(quantile(data,prob=p))
# }

#' @rdname distribution
#' @export
meanci <- function(mean,stdev,n,alpha=0.95){
    lower <- mean-qnorm((1+alpha)/2)*stdev/sqrt(n)
    higher <- mean+qnorm((1+alpha)/2)*stdev/sqrt(n)
    return(c(lower,higher))
}

#' @rdname distribution
#' @export
target <- function(data,value){
    if (length(value)==1) return(sum(data<=value)/length(data))
    else {
        a <- NULL
        for (i in 1:length(value)) a[i] <- sum(data<=value[i])/length(data)
        return(a)
    }
}

#' @rdname distribution
#' @export
citrials <- function(stdev,width,alpha=0.95){
    return( ceiling((qnorm((1+alpha)/2)*stdev/width)^2) )
}


## use genNORTARA in NORTARA package to generate random number with specific correlation




ks_fitdist <- function(x){
    if (!require(MASS)){
        install.packages("MASS")}
    library(MASS)
    dist_name <- c("beta", "cauchy", "chi-squared", "exponential", "f", "gamma", "geometric", "lognormal", "logistic", "negative binomial", "normal", "Poisson", "t", "weibull")
    distr <- list()
    result <- list()
    ks_result <- list()
    n <- length(x)
    distr[["beta"]] <- list(function_name="rbeta",parameter=list(shape1=4,shape2=1))
    distr[["cauchy"]] <- list(function_name="rcauchy",parameter=list(location=200,scale=300))
    distr[["chi-squared"]] <- list(function_name="rchisq",parameter=list(df=5,ncp=0))
    distr[["exponential"]] <- list(function_name="rexp",parameter=list(rate=10))
    distr[["f"]] <- list(function_name="rf",parameter=list(df1=60,df2=50))
    distr[["gamma"]] <- list(function_name="rgamma",parameter=list(shape=5,rate=0.1))
    distr[["geometric"]] <- list(function_name="rgeom",parameter=list(prob=0.5))
    distr[["lognormal"]] <- list(function_name="rlnorm",parameter=list(meanlog=5,sdlog=1.5))
    distr[["logistic"]] <- list(function_name="rlogis",parameter=list(location=5,scale=2))
    distr[["negative binomial"]] <- list(function_name="rnbinom",parameter=list(size=300,prob=0.3))
    distr[["normal"]] <- list(function_name="rnorm",parameter=list(mean=15,sd=2))
    distr[["Poisson"]] <- list(function_name="rpois",parameter=list(lambda=400))
    distr[["t"]] <- list(function_name="rt",parameter=list(m=3,s=2,df=5))
    distr[["weibull"]] <- list(function_name="rweibull",parameter=list(shape=5,scale=9))

    for (name in dist_name){
        if (name %in% c("cauchy","exponential","geometric","lognormal","negative binomial","normal","Poisson","t")){
            trynext <- try(fitdistr(x,name),silent=TRUE)
            if ('try-error' %in% class(trynext)) result[[name]] <- NA
            else result[[name]] <- fitdistr(x,name)[[1]]
        }
        else{
            trynext <- try(fitdistr(x,name,start=distr[[name]][["parameter"]]),silent=TRUE)
            if ('try-error' %in% class(trynext)) result[[name]] <- NA
            else result[[name]] <- fitdistr(x,name,start=distr[[name]][["parameter"]])[[1]]
        }
    }

    for (name in dist_name){
        if (!identical(result[[name]],NA)){
            if (name %in% c("exponential","Poisson","geometric")){
                y <- eval(parse(text=paste0(distr[[name]]$function_name,"(n,",result[[name]],")")))
                ks_result[[name]] <- ks.test(x,y)[["statistic"]]
            }
            else{
                if (name=="t"){
                    y <- eval(parse(text=paste0(distr[[name]]$function_name,"(n,",result[[name]][3],")")))
                    ks_result[[name]] <- ks.test(x,y)[["statistic"]]
                }
                else{
                    y <- eval(parse(text=paste0(distr[[name]]$function_name,"(n,",result[[name]][1],",",result[[name]][2],")")))
                    ks_result[[name]] <- ks.test(x,y)[["statistic"]]
                }
            }
        }
    }
    ks_result <- as.data.frame(t(as.data.frame(ks_result)))
    ks_result$dist_name <- row.names(ks_result)
    ks_result <- ks_result[order(ks_result$D),c(2,1)]
    row.names(ks_result) <- seq(1,nrow(ks_result))
    #result[["ks_result"]] <- ks_result

    for (name in ks_result$dist_name){
        if (name %in% c("exponential","Poisson","geometric")){
            hist(x,freq=F,main=paste("Histogram of",name,"distribution \n parameter:",names(distr[[name]]$parameter),"=",round(result[[name]],3)),col="gray")
            lines(density(x),lwd=2,col="blue")
            y <- eval(parse(text=paste0(distr[[name]]$function_name,"(",n,",",result[[name]],")")))
            lines(density(y),lwd=2,col="red")
        }
        else{
            if (name=="t"){
                hist(x,freq=F,main=paste("Histogram of",name,"distribution \n parameter:",names(distr[[name]]$parameter)[3],"=",round(result[[name]][3],3)),col="gray")
                lines(density(x),lwd=2,col="blue")
                y <- eval(parse(text=paste0(distr[[name]]$function_name,"(",n,",",result[[name]][3],")")))
                lines(density(y),lwd=2,col="red")
            }
            else{
                hist(x,freq=F,main=paste("Histogram of",name,"distribution \n parameter:",names(distr[[name]]$parameter)[1],"=",round(result[[name]][1],3),names(distr[[name]]$parameter)[2],"=",round(result[[name]][2],3)),col="gray")
                lines(density(x),lwd=2,col="blue")
                y <- eval(parse(text=paste0(distr[[name]]$function_name,"(",n,",",result[[name]][1],",",result[[name]][2],")")))
                lines(density(y),lwd=2,col="red")
            }
        }
    }

    return(ks_result)
}


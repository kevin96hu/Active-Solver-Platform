treeplot <- function(x,...) UseMethod("treeplot",x)

x_one <- function(xlim,maxdepth,depth){
    # 5*(maxdepth+2)*(maxdepth-depth)-5*(maxdepth+depth+1)*(maxdepth-depth)/2
    # (maxdepth+6)*(maxdepth-depth)-(maxdepth+depth+1)*(maxdepth-depth)/2
    # 40*(maxdepth-depth+1)
    return(xlim-(3*maxdepth-3*depth+53)*(maxdepth-depth)-75)
}

x_two <- function(xlim,maxdepth,depth){
    return(xlim-(3*maxdepth-3*depth+58)*(maxdepth-depth)-85)
}

x_three <- function(xlim,maxdepth,depth){
    return(xlim-(3*maxdepth-3*depth+58.5)*(maxdepth-depth)-88)
}

x_r <- function(maxdepth,depth){
    return((maxdepth+6-depth)/2)
}

x_four <- function(xlim,maxdepth,depth){
    return(xlim-(3*maxdepth-3*depth+59)*(maxdepth-depth)-91)
}

treeplot.default <- function(k){
    if(!require(plotrix)){
        install.packages("plotrix")}
    library(plotrix)
    md <- max(k$yval)
    xlim <- (5+md/2)*(md+1)+40*(md+1)+5*(md+3)*md/2
    depth <- md+1
    len <- 0
    for (item in k$leafnode_value) len <- len + length(item)
    ylim <- 25*(len-1)+10
    plot(x=c(0,xlim),c(0,ylim),type = "n",xlab = "",ylab = "",asp=1,xaxs="i",yaxs="i",axes=F)
    for (i in 1:len) lines(c(xlim-35,xlim-35+2.5*sqrt(3)),c(20*i-10,20*i-7.5))
    for (i in 1:len) lines(c(xlim-35,xlim-35+2.5*sqrt(3)),c(20*i-10,20*i-12.5))
    for (i in 1:len) lines(c(xlim-35+2.5*sqrt(3),xlim-35+2.5*sqrt(3)),c(20*i-7.5,20*i-12.5))
    coor <- list()
    for (i in length(k$var):1) {
        if (k$var[i] %in% k$father_name){
            for (j in 1:length(k$leafnode_value[[which(k$var[i] == k$father_name)]])){
                lines(c(xlim-35,xlim-75),c(20*len-10,20*len-10))
                text(xlim-15,20*len-10,k$leafnode_value[[which(k$var[i] == k$father_name)]][j])
                if (!identical(k$leaf_name[[which(k$var[i] == k$father_name)]],NA)){
                    text(xlim-55,20*len-6,k$leaf_name[[which(k$var[i] == k$father_name)]][j])
                    if (!identical(k$prob[[i]],NA)) text(xlim-55,20*len,k$prob[[i]][j])
                }
                else if (!identical(k$prob[[i]],NA)) text(xlim-55,20*len-6,k$prob[[i]][j])
                coor$y[j] <- 20*len-10
                len <- len-1
            }
            for (j in 1:length(k$leafnode_value[[which(k$var[i] == k$father_name)]])){
                lines(c(x_two(xlim,md,k$yval[i]),x_one(xlim,md,k$yval[i])),c(mean(coor$y),coor$y[j]))
            }
            if (k$type[i]=="chance"){
                draw.circle(x_three(xlim,md,k$yval[i]),mean(coor$y),x_r(md,k$yval[i]))
                text(x_three(xlim,md,k$yval[i]),mean(coor$y)+5+md-k$yval[i],k$value[[i]])
                coor[[k$var[i]]] <- mean(coor$y)
            }
            else {
                rect(x_four(xlim,md,k$yval[i]),mean(coor$y)-x_r(md,k$yval[i]),x_two(xlim,md,k$yval[i]),mean(coor$y)+x_r(md,k$yval[i]))
                text(x_three(xlim,md,k$yval[i]),mean(coor$y)+5+md-k$yval[i],k$value[[i]])
                coor[[k$var[i]]] <- mean(coor$y)
            }
        }
        else {
            y <- 0
            for (name in k$sub_name[[i]]){
                if (name %in% k$var){
                    y <- y + coor[[name]]
                }
                else {
                    y <- y + 20*len-10
                }
            }
            for (name in k$sub_name[[i]]){
                if (name %in% k$var){
                    lines(c(x_one(xlim,md,k$yval[i]),x_four(xlim,md,k$yval[i]+1)),c(coor[[name]],coor[[name]]))
                    text(x_one(xlim,md,k$yval[i])+20,coor[[name]]+4,name)
                    if (!identical(k$prob[[i]],NA)){
                        text(x_one(xlim,md,k$yval[i])+20,coor[[name]]+10,k$prob[[i]][which(name==k$sub_name[[i]])])
                    }
                }
                else {
                    lines(c(x_one(xlim,md,k$yval[i]),xlim-35),c(20*len-10,20*len-10))
                    text(x_one(xlim,md,k$yval[i])+20,20*len-6,name)
                    text(xlim-15,20*len-10,k$leafnode_value[[which(name == k$father_name)]])
                    if (!identical(k$prob[[i]],NA)){
                        text(x_one(xlim,md,k$yval[i])+20,20*len,k$prob[[i]][which(name==k$sub_name[[i]])])
                    }
                }
            }
            for (name in k$sub_name[[i]]){
                if (name %in% k$var){
                    lines(c(x_two(xlim,md,k$yval[i]),x_one(xlim,md,k$yval[i])),c(y/k$n[i],coor[[name]]))
                    coor$y <- y/k$n[i]
                }
                else {
                    lines(c(x_two(xlim,md,k$yval[i]),x_one(xlim,md,k$yval[i])),c(y/k$n[i],20*len-10))
                    len <- len - 1
                }
            }
            if (k$type[i]=="chance"){
                draw.circle(x_three(xlim,md,k$yval[i]),coor$y,x_r(md,k$yval[i]))
                text(x_three(xlim,md,k$yval[i]),coor$y+5+md-k$yval[i],k$value[[i]])
                coor[[k$var[i]]] <- coor$y
            }
            else {
                rect(x_four(xlim,md,k$yval[i]),mean(coor$y)-x_r(md,k$yval[i]),x_two(xlim,md,k$yval[i]),mean(coor$y)+x_r(md,k$yval[i]))
                text(x_three(xlim,md,k$yval[i]),coor$y+5+md-k$yval[i],k$value[[i]])
                coor[[k$var[i]]] <- coor$y
            }
        }
    }
}

treeplot.dist <- function(k,...){
    hist(k$value[[1]],...)
}

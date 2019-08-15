# start <- function(){
#     t <- list(var=NA,type=NA,yval=NA,n=NA,sub_name=list(NA),prob=list(NA),value=NA,father_name=NA,leaf_name=list(NA),leafnode_value=list(NA),order=1,nn=1)
#     attr(t,'class') <- c('tree')
#     return(t)
# }
tree <- function(x,...) UseMethod("tree")
tree.default <- function(x){
    t <- list(var=NA,type=NA,yval=NA,n=NA,sub_name=list(NA),prob=list(NA),value=NA,father_name=NA,leaf_name=list(NA),leafnode_value=list(NA),order=1,nn=1)
    attr(t,'class') <- c('tree')
    return(t)
}
tree.create <- function(x,name,type,n,sub_name,prob=NA){
    x$var <- name
    x$type <- type
    x$yval <- 1
    x$n <- n
    x$sub_name[[x$order]] <- sub_name
    if (!(type %in% c("choice","chance"))) stop("choose the correct node type")
    if (type=="chance" & identical(prob,NA)) stop("type the probability of choices")
    if (type=="chance" & abs(sum(prob)-1)>0.001) stop("check the sum of probability")
    if (type=="chance" & n!=length(prob)) stop("check number of probability")
    if (type=="choice") x$prob[[x$order]] <- NA
    else x$prob[[x$order]] <- prob
    x$value <- NA
    x$order <- x$order+1
    return(x)
}
tree.addnode <- function(x,name,type,n,sub_name=NA,prob=NA,leaf_nodevalue=NA){
    if (identical(type,NA)){
        if (identical(x$father_name,NA)) x$father_name <- name
        else x$father_name <- append(x$father_name,name)
        x$leaf_name[[x$nn]] <- sub_name
        x$leafnode_value[[x$nn]] <- leaf_nodevalue
        x$nn <- x$nn+1
    }
    else{
        if (name %in% x$var) stop("type in a different node name")
        else x$var <- append(x$var,name)
        x$type <- append(x$type,type)
        for (i in 1:length(x$sub_name)) {if (name %in% x$sub_name[[i]]) k <- x$yval[i]+1}
        x$yval <- append(x$yval,k)
        x$n <- append(x$n,n)
        for (s_name in sub_name[]){
            for (item in sub_name){
                if (s_name %in% item) stop("check the sub name of the node")
            }
        }
        x$sub_name[[x$order]] <- sub_name
        if (!(type %in% c("choice","chance"))) stop("choose the correct node type")
        if (type=="chance" & identical(prob,NA)) stop("type the probability of choices")
        if (type=="chance" & abs(sum(prob)-1)>0.001) stop("check the sum of probability")
        if (type=="chance" & n!=length(prob)) stop("check number of probability")
        if (type=="choice") x$prob[[x$order]] <- NA
        else x$prob[[x$order]] <- prob
        x$value <- append(x$value,NA)
        if (!identical(leaf_nodevalue,NA)){
            if (identical(x$father_name,NA)){
                x$father_name <- name
                x$leaf_name[[x$nn]] <- sub_name
                x$leafnode_value[[x$nn]] <- leaf_nodevalue
                x$nn <- x$nn+1
            }
            else{
                x$father_name <- append(x$father_name,name)
                x$leaf_name[[x$nn]] <- sub_name
                x$leafnode_value[[x$nn]] <- leaf_nodevalue
                x$nn <- x$nn+1
            }
        }
        x$order <- x$order+1
    }
    return(x)
}

# tree.leafnode <- function(x,father_node,leaf_name,value){
#     if (identical(x$father_name,NA)) {
#         x$nn <- 1
#         x$father_name <- father_node
#         if (father_node %in% x$var){
#             if (!identical(x$sub_name[[which(father_node == x$var)]],leaf_name) & !identical(x$sub_name[[which(father_node == x$var)]],NA) & !identical(leaf_name,NA)) stop("Make sure the leaf name is the same with subname of the father node")
#             else if (identical(leaf_name,NA)) x$leaf_name[[x$nn]] <- x$sub_name[[which(father_node == x$var)]]
#                  else x$leaf_name[[x$nn]] <- leaf_name
#         }
#         else x$leaf_name[[x$nn]] <- leaf_name
#         x$leafnode_value[[x$nn]] <- value
#     }
#     else{
#         x$father_name <- append(x$father_name,father_node)
#         if (father_node %in% x$var){
#             if (!identical(x$sub_name[[which(father_node == x$var)]],leaf_name) & !identical(x$sub_name[[which(father_node == x$var)]],NA) & !identical(leaf_name,NA)) stop("Make sure the leaf name is the same with subname of the father node")
#             else if (identical(x$leaf_name,NA)) x$leaf_name[[x$nn]] <- x$sub_name[[which(father_node == x$var)]]
#                  else x$leaf_name[[x$nn]] <- leaf_name
#         }
#         else x$leaf_name[[x$nn]] <- leaf_name
#         x$leafnode_value[[x$nn]] <- value
#     }
#     x$nn <- x$nn + 1
#     return(x)
# }

tree.eval <- function(x,opti){
    for (i in length(x$value):1){
        if (x$var[i] %in% x$father_name) {
            if (x$type[i]=="chance") x$value[i] <- x$prob[[i]] %*% x$leafnode_value[[which(x$var[i]==x$father_name)]]
            else if (opti=="max") x$value[i] <- max(x$leafnode_value[[which(x$var[i]==x$father_name)]])
                 else x$value[i] <- min(x$leafnode_value[[which(x$var[i]==x$father_name)]])
        }
        else {
            t <- NULL
            for (j in 1:length(x$sub_name[[i]])){
                if (x$sub_name[[i]][j] %in% x$var) t[j] <- x$value[which(x$sub_name[[i]][j]==x$var)]
                else t[j] <- x$leafnode_value[[which(x$sub_name[[i]][j]==x$father_name)]]
            }
            if (x$type[i]=="chance"){
                x$value[i] <- x$prob[[i]] %*% t
                }
             else {
                 if (opti=='max') x$value[i] <- max(t)
                 else x$value[i] <- min(t)
             }
        }
    }
    return(x)
}

create <- function(x,name,type,n,sub_name,prob=NA){
    attr(x,"class")<-c("tree","create")
    x <- tree(x,name,type,n,sub_name,prob)
    return(x)
}

addnode <- function(x,name,type=NA,n=NA,sub_name=NA,prob=NA,leaf_nodevalue=NA){
    attr(x,"class")<-c("tree","addnode")
    x <- tree(x,name,type,n,sub_name,prob,leaf_nodevalue)
    return(x)
}

# leafnode <- function(x,father_node,leaf_name=NA,value){
#     attr(x,"class")<-c("tree","leafnode")
#     x <- tree(x,father_node,leaf_name,value)
#     return(x)
# }

value <- function(x,opti){
    attr(x,"class")<-c("tree","eval")
    x <- tree(x,opti)
    return(x)
}


# k <- start()
# k <- create(k,"a","choice",2,c("treasury","LLC"))
# k <- addnode(k,"LLC","chance",2,c("fav","unfav"),c(0.3,0.7))
# k <- addnode(k,"fav","chance",2,prob=c(0.75,0.25))
# k <- addnode(k,"unfav","chance",2,prob=c(0.75,0.25))
# k <- leafnode(k,"fav",c(190000,-110000))
# k <- leafnode(k,"unfav",c(-110000,190000))
# k <- leafnode(k,"treasury",c(2000))
# k <- value(k,"max")

# k <- start()
# k <- create(k,"a","choice",2,c("treasury","LLC"))
# k <- addnode(k,"fav","choice",2,c("LLC1","treasury1"))
# k <- addnode(k,"unfav","choice",2,c("LLC2","treasury2"))
# k <- leafnode(k,"fav",c(115000,2000))
# k <- leafnode(k,"unfav",c(-35000,2000))
# k <- leafnode(k,"no",3000)
# k <- value(k,"max")
#
# k <- start()
# k <- create(k,"a","choice",2,c("treasury","LLC"))
# k <- addnode(k,"LLC","chance",2,c("fav1","unfav1"),c(0.3,0.7))
# k <- addnode(k,"fav1","chance",2,c("yes","no"),prob=c(0.5,0.5))
# k <- addnode(k,"treasury","chance",2,c("unfav2","fav2"),c(0.75,0.25))
# k <- addnode(k,"unfav2","chance",2,prob=c(0.75,0.25))
# k <- leafnode(k,"fav1",c("yes","no"),c(190000,-110000))
# k <- leafnode(k,"unfav1",value=c(190000))
# k <- leafnode(k,"unfav2",value=c(-110000,190000))
# k <- leafnode(k,"fav2",value=c(2000))
# k <- value(k,"max")

# k <- tree()
# k <- create(k,"a","choice",2,c("treasury","LLC"))
# k <- addnode(k,"LLC","chance",2,c("fav1","unfav1"),c(0.3,0.7))
# k <- addnode(k,"fav1","chance",2,c("yes","no"),prob=c(0.5,0.5),leaf_nodevalue=c(190000,-110000))
# k <- addnode(k,"unfav1",leaf_nodevalue=190000)
# k <- addnode(k,"treasury","chance",2,c("unfav2","fav2"),c(0.75,0.25))
# k <- addnode(k,"fav2","chance",2,prob=c(0.75,0.25),leaf_nodevalue=c(-110000,190000))
# k <- addnode(k,"unfav2",leaf_nodevalue=2000)
# k <- value(k,"max")


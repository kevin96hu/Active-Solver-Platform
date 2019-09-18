#' Create a decision tree
#'
#' the list create stores all the info needed for a decision tree structure
#' function tree() for initializing teh tree
#' function create() for adding root node
#' function addnode() for adding rest nodes
#' function eval() for calculate the value for each node
#'
#' @param x an object of the tree
#' @param name name of a decision node
#' @param type character, means type of a decision node (chance or choice)
#' @param n number of the branches of a node
#' @param sub_name vector, the names of each branch, cannot be repeated
#' @param prob vector, the probability for each branch. sum should be 1
#' @param leafnode_value vector, the value of the leaf node
#' @param opti character, "max" or "min" for type choice
#'
#' @author Kevin Hu \email{kevinhu@bu.edu}
#'
#' @examples
#'
#' k <- tree()
#' k <- create(k,"a","choice",2,c("treasury","LLC"))
#' k <- addnode(k,"LLC","chance",2,c("fav1","unfav1"),c(0.3,0.7))
#' k <- addnode(k,"fav1","chance",2,c("yes","no"),prob=c(0.5,0.5),leaf_nodevalue=list(7000,20000))
#' k <- addnode(k,"unfav1",leaf_nodevalue=list(190000))
#' k <- addnode(k,"treasury","chance",2,c("unfav2","fav2"),c(0.75,0.25))
#' k <- addnode(k,"fav2","chance",2,prob=c(0.75,0.25),leaf_nodevalue=list(-110000,190000))
#' k <- addnode(k,"unfav2",leaf_nodevalue=list(2000))
#' k <- value(k,"max")
#'
#' # we can input some random numbers as leaf node value
#' k <- tree()
#' k <- create(k,"a","choice",2,c("treasury","LLC"))
#' k <- addnode(k,"LLC","chance",2,c("fav1","unfav1"),c(0.3,0.7))
#' k <- addnode(k,"fav1","chance",2,c("yes","no"),prob=c(0.5,0.5),leaf_nodevalue=list(rnorm(10000,7000,100),20000))
#' k <- addnode(k,"unfav1",leaf_nodevalue=list(190000))
#' k <- addnode(k,"treasury","chance",2,c("unfav2","fav2"),c(0.75,0.25))
#' k <- addnode(k,"fav2","chance",2,prob=c(0.75,0.25),leaf_nodevalue=list(rnorm(100,-110000,20000),190000))
#' k <- addnode(k,"unfav2",leaf_nodevalue=list(2000))
#' k <- value(k,"max")
#'
#' @rdname decision_tree
#' @export
tree <- function(x){
    t <- list(var=NULL,type=NULL,yval=NULL,n=NULL,sub_name=list(),prob=list(),value=list(),father_name=NULL,leaf_name=list(),leafnode_value=list(),order=1,nn=1)
    return(t)
}
tree.create <- function(x,name,type,n,sub_name,prob=NULL){
    x$var <- name
    x$type <- type
    x$yval <- 1
    x$n <- n
    x$sub_name[[x$order]] <- sub_name
    if (!(type %in% c("choice","chance"))) stop("choose the correct node type")
    if (type=="chance" & identical(prob,NULL)) stop("type the probability of choices")
    if (type=="chance" & abs(sum(prob)-1)>0.001) stop("check the sum of probability")
    if (type=="chance" & n!=length(prob)) stop("check number of probability")
    if (type=="choice") x$prob[[x$order]] <- NA
    else x$prob[[x$order]] <- prob
    x$value[[x$order]] <- numeric(0)
    x$order <- x$order+1
    return(x)
}
tree.addnode <- function(x,name,type,n,sub_name=NULL,prob=NULL,leaf_nodevalue=NULL){
    if (identical(type,NULL)){
        if (identical(x$father_name,NULL)) x$father_name <- name
        else x$father_name <- append(x$father_name,name)
        if (identical(sub_name,NULL)) x$leaf_name[[x$nn]] <- NA
        else x$leaf_name[[x$nn]] <- sub_name
        x$leafnode_value[[x$nn]] <- leaf_nodevalue
        for (item in x$leafnode_value[[x$nn]]){
            if (length(item)>=2) attr(x,"class") <- c("dist")
        }
        x$nn <- x$nn+1
    }
    else{
        if (name %in% x$var) stop("type in a different node name")
        else {
            temp = 0
            for (item in x$sub_name) temp = temp + (name %in% item)
            if (temp==0) stop("type in the name of an existing node")
            else x$var <- append(x$var,name)
             }
        x$type <- append(x$type,type)
        for (i in 1:length(x$sub_name)) {if (name %in% x$sub_name[[i]]) k <- x$yval[i]+1}
        x$yval <- append(x$yval,k)
        x$n <- append(x$n,n)
        for (s_name in sub_name){
            for (item in x$sub_name){
                if (s_name %in% item) stop("check the sub name of the node")
            }
        }
        x$sub_name[[x$order]] <- sub_name
        if (!(type %in% c("choice","chance"))) stop("choose the correct node type")
        if (type=="chance" & identical(prob,NULL)) stop("type the probability of choices")
        if (type=="chance" & abs(sum(prob)-1)>0.001) stop("check the sum of probability")
        if (type=="chance" & n!=length(prob)) stop("check number of probability")
        if (type=="choice") x$prob[[x$order]] <- NA
        else x$prob[[x$order]] <- prob
        x$value[[x$order]] <- numeric(0)
        if (!identical(leaf_nodevalue,NULL)){
            if (identical(x$father_name,NULL)){
                x$father_name <- name
                if (identical(sub_name,NULL)) x$leaf_name[[x$nn]] <- NA
                else x$leaf_name[[x$nn]] <- sub_name
                x$leafnode_value[[x$nn]] <- leaf_nodevalue
                for (item in x$leafnode_value[[x$nn]]){
                    if (length(item)>=2) attr(x,"class") <- c("dist")
                }
                x$nn <- x$nn+1
            }
            else{
                x$father_name <- append(x$father_name,name)
                if (identical(sub_name,NULL)) x$leaf_name[[x$nn]] <- NA
                else x$leaf_name[[x$nn]] <- sub_name
                x$leafnode_value[[x$nn]] <- leaf_nodevalue
                for (item in x$leafnode_value[[x$nn]]){
                    if (length(item)>=2) attr(x,"class") <- c("dist")
                }
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
            # if ("dist" %in% attr(x,"class")){
            temp <- 0
            num <- which(x$var[i]==x$father_name)
            for (j in 1:length(x$leafnode_value[[num]])) temp <- temp + x$leafnode_value[[num]][[j]]
            for (j in 1:length(x$leafnode_value[[num]])) x$leafnode_value[[num]][[j]] <- x$leafnode_value[[num]][[j]] + temp - temp
            ma <- t(matrix(unlist(x$leafnode_value[[num]]),ncol=x$n[which(x$var[i]==x$father_name)]))
            if (x$type[i]=="chance") x$value[[i]] <- as.vector(x$prob[[i]] %*% ma)
            else if (opti=="max") x$value[[i]] <- as.vector(apply(ma,2,max))
                 else x$value[[i]] <- as.vector(apply(ma,2,min))
        }
        else {
            t <- list()
            for (j in 1:length(x$sub_name[[i]])){
                if (x$sub_name[[i]][j] %in% x$var) t[[j]] <- as.vector(x$value[[which(x$sub_name[[i]][j]==x$var)]])
                else t[[j]] <- as.vector(x$leafnode_value[[which(x$sub_name[[i]][j]==x$father_name)]][[1]])
            }
            temp <- 0
            for (j in 1:length(t)) temp <- temp + t[[j]]
            for (j in 1:length(t)) t[[j]] <- t[[j]] + temp - temp
            ma <- t(matrix(unlist(t),ncol=length(t)))
            if (x$type[i]=="chance") x$value[[i]] <- as.vector(x$prob[[i]] %*% ma)
            else if (opti=="max") x$value[[i]] <- as.vector((apply(ma,2,max)))
                  else x$value[[i]] <- as.vector((apply(ma,2,min)))
        }
    }
    return(x)
}

#' @rdname decision_tree
#' @export
create <- function(x,name,type,n,sub_name,prob=NULL){
    x <- tree.create(x,name,type,n,sub_name,prob)
    return(x)
}

#' @rdname decision_tree
#' @export
addnode <- function(x,name,type=NULL,n=NULL,sub_name=NULL,prob=NULL,leaf_nodevalue=NULL){
    x <- tree.addnode(x,name,type,n,sub_name,prob,leaf_nodevalue)
    return(x)
}

# leafnode <- function(x,father_node,leaf_name=NA,value){
#     attr(x,"class")<-c("tree","leafnode")
#     x <- tree(x,father_node,leaf_name,value)
#     return(x)
# }

#' @rdname decision_tree
#' @export
value <- function(x,opti="max"){
    x <- tree.eval(x,opti)
    return(x)
}


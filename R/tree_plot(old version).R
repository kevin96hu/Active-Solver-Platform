# tree.plot <- function(k){
#     if(!require(plotrix)){
#         install.packages("plotrix")}
#     library(plotrix)
#     if (!("tree" %in% class(k))) stop("Please use a tree class")
#     xlim <- (5+max(k$yval)/2)*(max(k$yval)+1)+40*(max(k$yval)+1)+5*(max(k$yval)+3)*max(k$yval)/2
#     depth <- max(k$yval)+1
#     len <- 0
#     for (item in k$leafnode_value) len <- len + length(item)
#     ylim <- 25*(len-1)+10
#     plot(x=c(0,xlim),c(0,ylim),type = "n",xlab = "",ylab = "",asp=1,xaxs="i",yaxs="i",axes=F)
#     for (i in 1:len) lines(c(xlim-35,xlim-35+2.5*sqrt(3)),c(20*i-10,20*i-7.5))
#     for (i in 1:len) lines(c(xlim-35,xlim-35+2.5*sqrt(3)),c(20*i-10,20*i-12.5))
#     for (i in 1:len) lines(c(xlim-35+2.5*sqrt(3),xlim-35+2.5*sqrt(3)),c(20*i-7.5,20*i-12.5))
#     coor <- list()
#     for (i in length(k$var):1) {
#         if (k$var[i] %in% k$father_name){
#             for (j in 1:length(k$leafnode_value[[which(k$var[i] == k$father_name)]])){
#                 lines(c(xlim-35,xlim-75),c(20*len-10,20*len-10))
#                 text(xlim-15,20*len-10,k$leafnode_value[[which(k$var[i] == k$father_name)]][j])
#                 if (!identical(k$leaf_name[[which(k$var[i] == k$father_name)]],NA)){
#                     text(xlim-55,20*len-6,k$leaf_name[[which(k$var[i] == k$father_name)]][j])
#                     if (!identical(k$prob[[i]],NA)) text(xlim-55,20*len,k$prob[[i]][j])
#                 }
#                 else if (!identical(k$prob[[i]],NA)) text(xlim-55,20*len-6,k$prob[[i]][j])
#                 coor$x[j] <- xlim-75
#                 coor$y[j] <- 20*len-10
#                 len <- len-1
#             }
#             for (j in 1:length(k$leafnode_value[[which(k$var[i] == k$father_name)]])){
#                 lines(c(coor$x[j]-5*(depth-k$yval[i]+1),coor$x[j]),c(mean(coor$y),coor$y[j]))
#                 coor$x[j] <- coor$x[j]-5*(depth-k$yval[i]+1)
#             }
#             if (k$type[i]=="chance"){
#                 draw.circle(coor$x[1]-(depth-k$yval[i]+5)/2,mean(coor$y),(depth-k$yval[i]+5)/2)
#                 text(coor$x[1]-(depth-k$yval[i]+5)/2,mean(coor$y)+5+max(k$yval)-k$yval[i],k$value[i])
#                 coor[[k$var[i]]] <- c(coor$x[1]-(depth-k$yval[i]+5),mean(coor$y))
#             }
#             else {
#                 rect(coor$x[1]-(depth-k$yval[i]+5),mean(coor$y)-(depth-k$yval[i]+5)/2,coor$x[1],mean(coor$y)+(depth-k$yval[i]+5)/2)
#                 text(coor$x[1]-(depth-k$yval[i]+5)/2,mean(coor$y)+5+max(k$yval)-k$yval[i],k$value[i])
#                 coor[[k$var[i]]] <- c(coor$x[1]-(depth-k$yval[i]+5),mean(coor$y))
#             }
#         }
#         else {
#             y <- 0
#             for (name in k$sub_name[[i]]){
#                 if (name %in% k$var){
#                     y <- y + coor[[name]][2]
#                 }
#                 else {
#                     y <- y + 20*len-10
#                 }
#             }
#             for (name in k$sub_name[[i]]){
#                 if (name %in% k$var){
#                     lines(c(coor[[name]][1]-40,coor[[name]][1]),c(coor[[name]][2],coor[[name]][2]))
#                     text(coor[[name]][1]-20,coor[[name]][2]+5,name)
#                     if (!identical(k$prob[[i]],NA)){
#                         text(coor[[name]][1]-20,coor[[name]][2]+10,k$prob[[i]][which(name==k$sub_name[[i]])])
#                     }
#                 }
#                 else {
#                     lines(c( xlim-(3*(max(k$yval)-k$yval[i])^2+53*(max(k$yval)-k$yval[i])+75) ,xlim-35),c(20*len-10,20*len-10))
#                     text(xlim-(3*(max(k$yval)-k$yval[i])^2+53*(max(k$yval)-k$yval[i])+75)+20,20*len-5,name)
#                     text(xlim-15,20*len-10,k$leafnode_value[[which(name == k$father_name)]])
#                     if (!identical(k$prob[[i]],NA)){
#                         text(xlim-(3*(max(k$yval)-k$yval[i])^2+53*(max(k$yval)-k$yval[i])+75)+20,20*len,k$prob[[i]][which(name==k$sub_name[[i]])])
#                     }
#                 }
#             }
#             for (name in k$sub_name[[i]]){
#                 if (name %in% k$var){
#                     lines(c(coor[[name]][1]-40-5*(depth-k$yval[i]+1),coor[[name]][1]-40),c(y/length(k$sub_name[[i]]),coor[[name]][2]))
#                     coor$x <- coor[[name]][1]-40-5*(depth-k$yval[i]+1)
#                     coor$y <- y/length(k$sub_name[[i]])
#                 }
#                 else {
#                     lines(c( xlim-(3*(max(k$yval)-k$yval[i])^2+58*(max(k$yval)-k$yval[i])+85) , xlim-(3*(max(k$yval)-k$yval[i])^2+53*(max(k$yval)-k$yval[i])+75) ),c(y/length(k$sub_name[[i]]),20*len-10))
#                     len <- len - 1
#                 }
#             }
#             if (k$type[i]=="chance"){
#                 draw.circle(coor$x-(depth-k$yval[i]+5)/2,coor$y,(depth-k$yval[i]+5)/2)
#                 text(coor$x-(depth-k$yval[i]+5)/2,coor$y+5+max(k$yval)-k$yval[i],k$value[i])
#                 coor[[k$var[i]]] <- c(coor$x-(depth-k$yval[i]+5),coor$y)
#             }
#             else {
#                 rect(coor$x-(depth-k$yval[i]+5),coor$y-(depth-k$yval[i]+5)/2,coor$x,coor$y+(depth-k$yval[i]+5)/2)
#                 text(coor$x-(depth-k$yval[i]+5)/2,coor$y+5+max(k$yval)-k$yval[i],k$value[i])
#                 coor[[k$var[i]]] <- c(coor$x-(depth-k$yval[i]+5),coor$y)
#             }
#         }
#     }
# }

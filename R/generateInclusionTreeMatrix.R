#' generateInclusionTreeMatrix
#' The function generates adjacency matrix of a graph which is a spanning tree of a graph generated with `generateInclusionMatrix`. It is rooted in the maximal set.
#' @param sets The set system
#' @export
generateInclusionTreeMatrix <- function(sets){
  stopifnot(class(sets) == "list")
  C <- generateInclusionMatrix(sets)
  for(i in 1:nrow(C)){
    temp <- 1:nrow(C)
    for(j in 1:ncol(C)){
      temp[j] <- ifelse(C[i, j] == 0, Inf, length(sets[[j]]))
    }
    for(j in 1:ncol(C)){
      temp[j] <- ifelse(length(setdiff(sets[[j]], sets[[i]])) != 0, temp[j], Inf)
    }
    temp1 <- ifelse(temp==temp[which.min(temp)][[1]] & temp[which.min(temp)][[1]] != Inf, 1, 0)
    check <- temp1 != 0
    if(!(all(temp1 == 0))){
      indexes <- (1:length(temp1))[check]
      if(length(indexes)!=1){
        index <- sample(as.vector(indexes), 1)
        indexes2 <- 1:length(temp1) != index
        temp1[indexes2] <- 0
      }
    }
    C[i,] <- temp1
  }
  for(i in 1:nrow(C)){
    for(j in 1:nrow(C)){
      C[i, j] <- max(C[i, j], C[i, j])
    }
  }
  C
}
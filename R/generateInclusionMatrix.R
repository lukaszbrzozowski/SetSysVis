#' generateInclusionMatrix
#' The function generates adjacency matrix of a graph connecting a set with all its subsets
#' @param sets The set system
#' @export
generateInclusionMatrix <- function(sets){
  stopifnot(class(sets) == "list")
  B <- generateCommonElementMatrix(sets)
  for(i in 1:ncol(B)){
    temp <- B[,1]
    for(j in 1:length(temp)){
      if((!(j == i)) & (length(setdiff(sets[[i]], sets[[j]])) != 0 & length(setdiff(sets[[j]], sets[[i]])) != 0)){
        B[i, j] <- 0
      }
    }
  }
  for(i in 1:ncol(B)){
    for(j in 1:nrow(B)){
      B[i, j] <- max(B[i, j], B[j, i])
    }
  }
  B
}
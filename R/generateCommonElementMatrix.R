#' generateCommonElementMatrix
#' The function takes a set family (alist of vectors) and generates the adjacency matrix of a graph with edges connecting sets having at least one common element  
#' @param sets The set system
#' @export
generateCommonElementMatrix <- function(sets){
  stopifnot(class(sets) == "list")
  A <- matrix(nrow = length(sets), ncol = length(sets))
  z <- generateNamesVector(sets)
  colnames(A) <- z
  rownames(A) <- z
  
  for(i in 1:length(sets)){
    for(j in 1:length(sets)){
      if((!(j==i)) && (length(union(sets[[i]], sets[[j]])) != (length(sets[[i]]) + length(sets[[j]])))){
        A[i, j] <- 1
      }else{
        A[i, j] <- 0
      }
    }
  }
  A
}

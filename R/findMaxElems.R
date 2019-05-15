findMaxElems <- function(sets){
  stopifnot(class(sets) == "list")
  temp <- unlist(sets, use.names=FALSE)
  as.numeric(names(sort(table(temp),decreasing=TRUE)))
}

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){ ## setting the matrix
    x <<- y
    k <<- NULL
  }
  ## setting the inverse of the matrix
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k ## getting the inverse 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return inverted matrix
  k <- x$getInverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
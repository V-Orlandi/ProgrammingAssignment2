# Author Vagner Orlandi
# Date 27/04/2014
# ver 1.0 
# R programming @ Coursera

## List of funtions to cache a inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv 
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function will compute inverse of a matrix
## if done before get cached matrix
cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return (inv)
  }
  data <- x$get()
  #Compute matrix
  inv <- solve(data, ...)
  x$set_inverse(inv)
  return (inv)
}

#
# Cached analog to the matrix() function
# 
#   Returns a list of functions and data representing the
#   cached matrix value.
#
makeCacheMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  m = matrix(data, nrow, ncol, byrow, dimnames)
  cached_inverse = NULL
  set <- function(new_matrix) {
    m <<- new_matrix
    cached_inverse <<- NULL
  }
  get <- function() m
  setinverse <- function(inv) cached_inverse <<- inv
  getinverse <- function() cached_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#
# Return and cache the inverse of the matrix stored in x
#
# Example:
#   x = makeCacheMatrix(rnorm(16), 4, 4)
#   cacheinverse(x)
#   cacheinverse(x)
#
cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
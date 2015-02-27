#
# Cached analog to the matrix() function
# 
#   Returns a list of functions and data representing the
#   cached matrix value.
#
makeCacheMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  # Create a new matrix from the parmaters
  m = matrix(data, nrow, ncol, byrow, dimnames)
  cached_inverse = NULL

  # Create the functionifor the cache object
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
#   cacheSolve(x)
#   cacheSolve(x)
#
cacheSolve <- function(x, ...) {
  # Get the current solved matrix from the cache object
  inv <- x$getinverse()

  # If the solve is already complete, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # Solve the matrix and store in the cache and return
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

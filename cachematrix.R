## this file contains 2 functions:
## - makeCacheMatrix()
## - cacheSolve()

## makeCacheMatrix() returns a list of functions of the following:
## - setmatrix(): sets the matrix to be used by cacheSolve
## - getmatrix(): returns the matrix that has been given using set function
## - set(matrix): used by cacheSolve function to save matrix result cache
## - get(): used by cacheSolve function to save matrix result cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve(cacheMatrix) function return the inverse of the matrix set in <cacheMatrix> parameter
## , will use result from cache if available as calculating inverse of a matrix is expensive operation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(m)
  x$setmatrix(m)
  m
}

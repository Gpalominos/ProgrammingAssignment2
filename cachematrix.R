## These function try to use cache information to evoid recalculating inverse matrix operation
## The fist function create a list using a matrix with capability of call and store information
## the second one, inverse a matrix if it hasn´t done before.

## makeCacheMatriz function create a list with four functions using a matrix. These can get the matrix or
## get its inverse if there are, or set the matrix or set its inverse in the other case.  
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverso) inv <<- inverso
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve function calculate a matrix inverse if there is not calculated yet, or print the inverse if
## there is cached.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data,...)
  x$setinv(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
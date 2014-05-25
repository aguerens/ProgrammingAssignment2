## This function creates a special "matrix" object that can cache its inverse.
##
## Methods for calculation and content retrieval are prepared for calls from an
## external function that shares a cache built on function's scope.
## The inverse is calculated when the function is called for the first time.
## Otherwise, the function returns the result from the cached area based on 
## shared areas between caller and called function environments.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  ## Execute the inverse using `solve`
  setInverse <- function(solve) m <<- solve
  
  ## Return the inverse matrix
  getInverse <- function() m
  
  ## use list as a function container
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This is a companion for the 'makeCacheMatrix' function
##
## 

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("Matrix sourced from cache")
    return(m)
  }  
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

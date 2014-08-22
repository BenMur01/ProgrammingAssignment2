## Course ID: rprog - 006
## Progamming Assignment 02
## Program to compute and cache the inverse of an invertible matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Functions run when cacheSlove is called
  ## These are object methods to get values for x or i(solve) 
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Function to computes the inverse special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated then this function will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## If inverse of matrix is already calculated (is not null), it is returned
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If inverse of matrix is NOT calculated, inverse is calculated and returned
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
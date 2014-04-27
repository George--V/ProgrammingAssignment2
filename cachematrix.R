## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix through two main funcions:
## makeCacheMatrix and cacheSolve


## Function makeCache creates a matrix object that can cache its inverse
## implements get and set for the matrix value as its inverse (cached)
## using R function solve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
## If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

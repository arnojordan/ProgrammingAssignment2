## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) xinverse <<- solve
  getsolve <- function() xinverse
  list(set = set, get = get,
       setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
## The second function cacheSolve calculates the inverse of the special "matrix"
## created with the function makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinverse <- x$getsolve()
  if(!is.null(xinverse)) {
    message("getting cached data")
    return(xinverse)
  }
  data <- x$get()
  xinverse <- solve(x, ...)
  x$setsolve(xinverse)
  xinverse
}

## Put comments here that give an overall description of what your
## functions do

## Write the following functions:
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.


## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  localz_m <- NULL
  set <- function(y) {
    cache_x <<- y 
    cache_m <<- NULL
  }
  get <- function() cache_x 
  set_cache_m <- function(localz_m) cache_m <<- localz_m
  get_cache_m <- function() cache_m 
  list(set = set, get = get,
       set_cache_m = set_cache_m,
       get_cache_m = get_cache_m)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  localz_m<- x$get_cache_m()
  if(!is.null(localz_m)) {
    message("getting cached data...")
    return(localz_m)
  } 
  smatrix <- x$get() 
  ematrix <- solve(smatrix)
  x$set_cache_m(ematrix)
  ematrix
}



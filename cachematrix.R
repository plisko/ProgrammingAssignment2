## R Programming - Peer Assessment 
## 
## The objective of the two functions below is to implement a caching system
##  for the computation of the inverse of a matrix.

# makeCacheMatrix creates a list of functions used to contain a matrix and its inverse
# (if already computed! - cached).
# In any subsequent computation of the same inverse, the cached version should be used.
# This method does a smart use of R scoping rules.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  # the following list a cacheMatrix object
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cahceSolve is a version of the solve function that allows the use of the caching
## mechanism implemented with the makeCacheMatrix function.
## The input argument should be a cacheMatrix (returned by makeCacheMatrix).
## Any other argument of the standard solve() function can be passed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


## Example of usage ##
#
# > test <- makeCacheMatrix(matrix(rnorm(10000, mean=10, sd=1), nrow=100, ncol=100))
# > r <- cacheSolve(test)
# > r <- cacheSolve(test)
# getting cached data
# > test <- makeCacheMatrix(matrix(rnorm(10000, mean=10, sd=1), nrow=100, ncol=100))
# > s <- cacheSolve(test)
# > s <- cacheSolve(test)
# getting cached data
#
# ## Just to be sure, check that the two inverses are different
# > all(s == r)
# [1] FALSE
##

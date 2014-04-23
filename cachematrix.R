## R Programming - Peer Assessment (02)
## 
## The objective of the two functions below is to implement a caching system
##  for the computation of the inverse of a matrix.


# makeCacheMatrix creates a list of functions used to contain a matrix and its inverse
# (if already computed! - cached).
# In any subsequent computation of the same inverse, the cached version should be used.
# This method does a smart use of R scoping rules.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # setter/getter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    # setter/getter for the inverse
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    # the following list is a cacheMatrix object
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cahceSolve is a version of the solve() function that allows the use of the caching
## mechanism implemented with the makeCacheMatrix function.
## The input argument should be a cacheMatrix (returned by makeCacheMatrix).
## Any other argument of the standard solve() function can be passed.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # if the inv is already cached..
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, we need to compute the inverse..
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
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


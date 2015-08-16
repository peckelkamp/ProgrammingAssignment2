## Contains the functions makeCacheMatrix and cacheSolve
## which should be used when the inverse of a matrix is 
## needed multiple times.
## Insted of calculating the inverse on demand, the inverse
## will be cached and only calculated once, when it is first 
## needed

## makeCacheMatrix
## inputs: x = a matrix, defaults to an empty 0x0 matrix
## returns: list containing get and set methods for the
##          matrix itself and the inverse
##
## The list returns contains everything needed to store a matrix
## and cache it's inverse so that it may be used many times
## and much quicker than calculating the inverse every time.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y)
    {
        x <<- y
        inverse <- NULL
    }
    
    get <- function() x
    
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve 
## inputs: x = a matrix created using makeCacheMatrix
## returns: inverse of matrix x
## 
## function will first check to see if the inverse has already
## been calculated. If it has, will just return the cached value
## of the inverse. If it has not already been calculated, will
## calculate the inverse, store the result using x$setInverse
## and then return the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

## This file contains a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix function creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix if the inverse has not already been 
## calculated or the matrix has changed since last time. Otherwise it
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}

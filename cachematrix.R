####################################################################
# Coursera R Programming Assignment 2:-
# A pair of functions that cache the inverse of a matrix.
# Author: Jishnu
####################################################################

# The first function creates a special kind of "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the cached value  
        cachedInverse <- NULL
        
        ## set the values of the matrix
        set <- function(newValue) {
                x <<- newValue
                cachedInverse <<- NULL
        }
        
        ## get the values of the matrix
        get <- function() x
        
        ## set the cached Inverse value
        setInverse <- function(inverse) cachedInverse <<- inverse
        
        ## get the cached Inverse value
        getInverse <- function() cachedInverse
        
        ## Returns the list of all the above functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# The second function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
# If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## get the cached value
        inv <- x$getInverse()
        
        ## return cached value if it exists
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }

        ## If the inverse has not yet been calculated
        ###############################################
        ## getting the matrix from the object passed as argument        
        data <- x$get()
        
        ## calculating the inverse by using solve()
        inv <- solve(data)
        
        ## store the inverse to the matrix to future usage
        x$setInverse(inv)
        
        ## return the Inverse
        inv 
}

## R Programming course - Week 3
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## G.Scotti

## This module defines two functions providing an efficient mechanism for
## calculating the inverse of a given n*n invertible matrix, caching the result, and make it
## further available to the application, if required, saving computation time.

## makeCacheMatrix
##
## This function gets an n*n invertible matrix as input x, stores a copy of it
## in the global environment and returns a special "matrix" object that is
## a list of.. functions. The following:
##
## get() : return the stored matrix x
##
## setinverse(y) : store the value of y, y being a matrix n*n, 
##  inverse of the stored matrix x 
##
## getinverse() : returns the value of the inverse matrix, stored with setinverse(y)
##
## Additionally this method is provided to store a new matrix, if needed
## set(x): store the value of x, where x is an n*n invertible matrix
##
makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
    ## define methods:
    
    set <- function(y) {
        
        ## the matrix x is stored in the global environment using the 
        ##  superassignment operator <<-
        x <<- y
        
        ## similarly, the value of the inverse 'i' is initialised to NULL, 
        # again in the global environment
        i <<- NULL
    }
    
    ## get the stored matrix
    get <- function() x
    
    ## The inverse matrix inv is stored in the variable 'i' (global environment)
    setinverse <- function(inv) i <<- inv
    
    ## get the stored inverse matrix
    getinverse <- function() i
    
    
    ## create and return a list containing the methods defined above 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve
##
## This function computes the inverse of x, a special "matrix" object returned by 
## makeCacheMatrix above.
## The function computes the inverse of x just once, the first time it is called,
## the inverse is cached for reuse.
## subsequent calls to this functions returns the cached inverse matrix
## avoiding to recalculate it again.
cacheSolve <- function(x, ...) {
    
    # get the inverse matrix from x with getinverse(). 
    # If this is NULL, that 
    # means that it was never computed before
    i <- x$getinverse()
    if(!is.null(i)) {
        # not NULL : already computed. Reuse the value, don't compute it again
        message("getting cached data")
        return(i)
    }
    
    # NULL : we have to compute the inverse
    data <- x$get()
    i <- solve(data, ...)
    
    # the computed inverse is cached, i.e. stored in the "matrix" object using  setinverse
    x$setinverse(i)
    
    # the inverse is returned
    i
}

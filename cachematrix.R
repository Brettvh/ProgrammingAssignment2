## Caching the Inverse of a Matrix
##
## Created 5/22/2014 as a stub from  https://github.com/rdpeng/ProgrammingAssignment2 
##
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly.  The functions makeCacheMatrix 
## and cacheSolve work together to cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set the value of the initial matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## return the value of the initial matrix
    get <- function() x
    
    ## set the value of the (solved) inverse matrix
    setsolve <- function(solve) m <<- solve
    
    ## get the value of the (solved) inverse matrix
    getsolve <- function() m
    
    ## define a list of the functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## check cache to see if we have solved this already
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## data not in cache, execute solve function, save to cache, and return matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}

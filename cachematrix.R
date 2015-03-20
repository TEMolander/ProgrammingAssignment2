## This implementation can be used to calculate and cache the costly operation of 
## inversing a matrix.

## Provides a list of named functions needed to manage the cached
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y)
        {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solved) s <<- solved
        getInverse <- function() s
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the cached inverse of the matrix encapsulated by a list of functions 
## created by the "makeCacheMatrix".  If an inverse matrix is not cached, the inverse
## be calculated, then cached.  The parameter 'x' must be a list of functions
## returned by the function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if (!is.null(s))
        {
                message("getting cached data.")
                return(s)
        }
        
        data <- x$get()
        s <- solve(data)
        x$setInverse(s)
        s
}
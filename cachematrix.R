## These functions create a matrix and calculate its inverse.

## Creates a special "matrix": a list containing a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, get the value of the inverse.
## Change function argument to (x = matrix(1:4, ncol=2)) to see results for a specific matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns a matrix that is the inverse of 'x'.
## If the inverse is already calcuated and the matrix is unchanged, a cached value will be returned.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

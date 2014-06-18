## These functions create a matrix object class that stores a matrix and its
## inverse, and when the matrix changes, calculates the inverse of the matrix.

## This function creates a matrix object that can cache its inverse, with member
## functions set, get, setInverse, and getInverse. Using set to reset the matrix
## data, will also clear the inverse value.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL       ## resetting the data clears the inverse value
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {
        inv
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function checks whether the inverse of the matrix x has already been
## cached and if so, returns it.  Otherwise, it calculates the inverse using
## solve.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse value")
        return(inv)             ## if there is a cached value, returns it
    }
    theMatrix <- x$get()
    inv <- solve(theMatrix)   ## if there is no cached value, solve() is used
    x$setInverse(inv)       ## the new inverse is cached
    inv
}

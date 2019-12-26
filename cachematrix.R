## Coursera R Programming
## Programming Assignment 2 (Week 3)
##
## Task: Define matrices with cachable inverses


## Defines an enhanced matrix class that can cache its own inverse
## (Under the hood it is just a list of method functions)
## Optionally initialize with a standard *square* R-matrix
## Generally use set/get functions (e.g., cmat$get()) for interacting with the internal R-matrix
## For inverse, compute externally and use setInv, fetch with getInv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(newInv) inv <<- newInv
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Utility for computing and caching a cacheMatrix's inverse, which first checks whether
##   the inverse has already been computed and cached

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    x$setInv(solve(x$get()))
    x$getInv()
}

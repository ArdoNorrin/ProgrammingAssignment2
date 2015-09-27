## These functions first create a "matrix" based off an inputted
## matrix, then calculate its inverse.

## This function initiates our matrix and assigns some metadata
## to it. This metadata can be used to determine if it has already
## had its inverse calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes, as its input, the cache matrix created by
## the makeCacheMatrix function and calculates the inverse. If
## the inverse has been calculated previously, it returns that
## result. Otherwise, it calculates it from scratch.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

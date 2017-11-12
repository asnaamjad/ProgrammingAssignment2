## makeCacheMatrix: This function creates a "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(X = matrix()) {
        invrs <- NULL
        set <- function(y) {
                X <<- y
                invrs <<- NULL
        }
        get <- function() X
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() invrs
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## cacheSolve: This function computes the inverse of the "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.
cacheSolve <- function(X, ...) {
        invrs <- X$getInverse()
        if (!is.null(invrs)) {
                message("get cache data")
                return(invrs)
        }
        newMatrix <- X$get()
        invrs <- solve(newMatrix, ...)
        X$setInverse(invrs)
        invrs
}

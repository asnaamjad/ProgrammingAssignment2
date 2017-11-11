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
cacheSolve <- function(X, ...) {
        invrs <- X$getInverse()
        if (!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        mat <- X$get()
        invrs <- solve(mat, ...)
        X$setInverse(invrs)
        invrs
}

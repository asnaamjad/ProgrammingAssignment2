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
                message("get cache data")
                return(invrs)
        }
        newMatrix <- X$get()
        invrs <- solve(newMatrix, ...)
        X$setInverse(invrs)
        invrs
}

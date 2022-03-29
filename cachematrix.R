'''The makeCacheMatrix function will create a matrix object that will cache its inverse.
Sample is the matrix object that user will submit on the console'''
makeCacheMatrix <- function(x = matrix()) {
    inverse_sample <- NULL
    set <- function(x) {
        Sample <<- x
        inverse_sample <<- NULL
    }
    get <- function() sample
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inverse_sample
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


'''This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.
'''
cacheSolve <- function(x, ...) {
    inv <- sample$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inverse_sample)
    }
    mat <- sample$get()
    inverse_sample <- solve(mat, ...)
    sample$setInverse(inverse_sample)
    inv_sample
}

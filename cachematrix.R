## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# A function creating a special "matrix" object that can cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# A function computing the inverse of the special "matrix" created by makeCacheMatrix.
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the inverse and sets the value of the inv in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}




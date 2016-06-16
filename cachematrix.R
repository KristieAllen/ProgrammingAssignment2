## Caching the Inverse of a Matrix

## Function makeCacheMatrix sets a list of functions to be used in cacheSolve
## which is where the logic is (hopefully)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse
             , getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## First check for cashed version of inverse matrix and return if exists
## If not there, then calculate the inverse, cache it & return inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        datamatrix <- x$get()
        inv <- solve(datamatrix, ...)
        x$setinverse(inv)
        inv
}

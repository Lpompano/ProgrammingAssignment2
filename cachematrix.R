## This program creates a set of functions to cache the inverse of a matrix.

## Create a "makeCacheMatrix" function, which creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}



## Calculate the inverse of x, first checking whether it has already been inverted. 
##If so, gets the inverse from the cache, otherwise calculates the inverse and sets the value

cacheSolve <- function(x, ...) {
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

## Return a matrix that is the inverse of 'x'

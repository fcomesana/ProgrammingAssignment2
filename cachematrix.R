## This script take a matrix, output it inverse and save it in chache
## The functions are makeCacheMatrix and cacheSolve. It work together.

## makeCacheMatrix take a matrix and generate a vector of functions as output that allow 
## to save the inverse of the matrix on cache

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve takes the function makeCacheMatrix, check if the inverse of the matrix is in chache and display it.
## If the function it's not in chache, save it and display it.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    return(i)
}

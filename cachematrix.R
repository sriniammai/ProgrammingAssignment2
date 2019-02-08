## Makes a special 'matrix' object and generates inverse of the matrix. 
## Additionally it has cache mechanism to store the computed inverse matrix to avoid the repeated computation.

## makeCacheMatric function helps to create special 'matrix' object with set , get , setinverse and getinverse methods

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() { x}
    setinverse <- function(im) { inverse <<- im}
    getinverse <- function() { inverse}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve method computes the inverse of the given special 'matrix' object
## Only calls solve method if the inverse not found in the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (! is.null(inverse)) {
        message("Inverse matrix reterived from cache")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
    ## Return a matrix that is the inverse of 'x'
}

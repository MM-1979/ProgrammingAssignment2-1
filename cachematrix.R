
# These functions cache the inverse of a matrix to save computing time.
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
    inv <- NULL   # Initializes inverse matrix to NULL
    
    set <- function(y) {   # Sets the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function () x   # Gets the value of the matrix (passed through makeCacheMatrix call)
    setInverse <- function(solve) inv <<- solve   #sets inverse matrix
    getInverse <- function() inv   # gets inverse matrix
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Sets variable with data from makeCacheMatrix function (either NULL or cached data)
    inv <- x$getInverse()
    
    if(!is.null(inv)) {   # if inv is not NULL, returns inverse matrix 
        message("Getting cached data")
        return(inv)
    }
    
    data <- x$get()  # if inv is NULL, data gets the matrix of x
    inv <- solve(data, ...)  # inv is set to the inverse matrix
    x$setInverse(inv)   # the inverse matrix is cached
    inv   # the inverse matrix is returned
}
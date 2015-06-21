
# These functions cache the inverse of a matrix to save computing time.
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
    # Initializes inverse matrix to NULL
    inv <- NULL  

    # Sets the value of the matrix passed to the function
    set <- function(y) {   
        x <<- y
        inv <<- NULL
    }

    # Gets the value of the matrix (passed through makeCacheMatrix call)
    get <- function () x

    # store inverse matrix
    setInverse <- function(solve) inv <<- solve  

    # gets inverse matrix
    getInverse <- function() inv  

    # class returned is a list of functions
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Checks variable with data from makeCacheMatrix function (either NULL or cached data)
    inv <- x$getInverse()

    # if inv is not NULL, returns inverse matrix 
    if(!is.null(inv)) {   
        message("Getting cached data")
        return(inv)
    }

    # if inv is NULL, data gets the matrix of x
    data <- x$get()  
    # inv is set to the inverse matrix (calculated here)
    inv <- solve(data, ...)  
    # the inverse matrix is cached
    x$setInverse(inv)   
    # the inverse matrix is returned
    inv   
}
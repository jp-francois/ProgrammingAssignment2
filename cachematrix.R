## Functions to cache the inverse of a matrix.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initialize the value of the matrix inverse to NULL
    m <- NULL
    set <- function(y)
    {
        x <<- y
        # change the value of inverse of the matrix in case the matrix was changed.
        m <<- NULL
    }
    # get the value of the inverse
    get <- function() x
    # calc the inverse of the matrix using the solve function
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    # return the values of the functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function to compute the inverse of a matrix.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    # return the inverse if it exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # calc and return the inverse using solve
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

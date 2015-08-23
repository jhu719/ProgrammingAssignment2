## This function aims at caching the inverse of a matrix.

## The function makeCacheMatrix is to create a matrix
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL    ##set the inverse to be an empty object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x    ##Get the matrix
    setsolve <- function(solve){    ##Set m to be the inverse matrix of 'x'
        m <<- solve(x)
    }
    getsolve <- function() m    ##Get the value of m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


##The function cacheSolve calculates the inverse of the matrix returned by
##the function above. It will retrieve the stored inverse from the catche
##if the inverse has been calculated.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {                     ##Check to see if m is NULL. 
                                          ##If not, get the value of m directly
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)   ##If m is NULL, calculate the inverse matrix.
    x$setsolve(m)
    m   ## Return a matrix that is the inverse of 'x'
}

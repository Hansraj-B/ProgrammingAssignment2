## Function to cache the inverse of the function

## method for setting and getting inverse matrix object

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinv <- function(inv) invmat <<- inv
    getinv <- function() invmat
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## method for matrix and inverse matrix generation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinv()
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    data <- x$get()
    invmat <- solve(data)
    x$setinv(invmat)
    invmat
}

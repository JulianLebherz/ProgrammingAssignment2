## makeCacheMatrix allows to cache its inverse
## cacheSolve accesses a matrix created by makeCacheMatrix and solves the 
## inverse only if it has not been calculated yet

## makeCacheMatrix creates a special matrix, which is a list of functions to
## set or get the matrix and set or get its inverse
makeCacheMatrix <- function(x = matrix()) {
    i   <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve accesses a matrix created by makeCacheMatrix and checks, whether
## its inverse has already been solved/calculated - it thereby saves double 
## calculation. If the matrix has been overwritten, its inverse has to be re-
## calculated though.

cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}


## This function creates a special "matrix" object and 
## returns a set of functions to allow inverse matrix
## calculation to be cached
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the underlying value for the matrix
    get <- function() x
    ## set the cached inverse value
    setsolve <- function(solve) m <<- solve
    ## get the cached inverse value
    getsolve <- function() m
    ## return the vector of functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The following function calculates the mean of the matrix created with
## the makeCacheMatrix function. First it checks to see if it has
## already been calculated.  If so, it pulls from the cache and
## skips the computation.  Otherwise, it calculates the inversion
## of the matrix and sets the value in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
## checks for cache to see if can use the cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
## otherwise calculates inversion of the matrix susing solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
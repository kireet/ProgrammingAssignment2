## The below two functions provide a way to cache the inverse calculation of a 
## matrix.

## constructor function for the cachable value. should be used in conjunction
## with cacheSolve() below
#    argument x: the initial matrix
#    returns a list containing:
#      a setter function for the matrix, clears any previously cached inverse
#      a getter function for the matrix
#      a setter function for the calculated inverse
#      a getter function for the calculated inverse, possibly NULL
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    getinverse <- function() i
    setinverse <- function(inv) {
        i <<- inv
    }
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## invoke this function rather than solve() to take advantage of a cached
## computation
#  argument x: the special cachable matrix as constructed by makeCacheMatrix()
#  argument ...: will be passed to the solve computation iff the inverse has
#                not yet been calculated
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(is.null(i)) {
        i <- solve(x$get(),...);
        x$setinverse(i)
    }
    i
}

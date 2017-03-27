## Simple script to invert matrices with memory


## Initialize a cached matrix object with matrix data
## by calling makeCacheMatrix, resetting the memorized inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Instead of calling solve() on a matrix again and again (expensive),
## first turn that into a cached matrix object and then
## call cacheSolve() instead, supplying the cached matrix
## the inverse if not previously calculated will be recalculated only once using solve()
## and on subsequent calls the cached result will be returned
## along with a Message about the savings

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

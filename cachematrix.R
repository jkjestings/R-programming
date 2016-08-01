

## makeCacheMatrix: creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setmat <- function(solve) m <<- solve
    getmat <- function() m
    list(set = set, get = get,
         setmat = setmat,
         getmat = getmat)
    
}


## cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix. It retrieves
## the inverse from the cache if it has already been calculated. 

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat, ...)
    x$setmat(m)
    m
}
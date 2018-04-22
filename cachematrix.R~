

## The function, makeCacheMatrix creates a special "matrix", which can cache its inverse.

## It can
##	set the value of the matrix
##	get the value of the matrix
##	set the value of the inverse of the matrix
##	get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    cachedMatrix <- NULL
    set <- function(y) {
        x <<- y
        cachedMatrix <<- NULL
    }
    get <- function() x
    setcachedmatrix <- function(cachedM) cachedMatrix <<- cachedM
    getcachedmatrix <- function() cachedMatrix
    list(set = set, get = get,
         setcachedmatrix = setcachedmatrix,
         getcachedmatrix = getcachedmatrix	
    )
	
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedMatrix <- x$getcachedmatrix()
    if(!is.null(cachedMatrix)) {
        message("getting cached data")
        return(cachedMatrix)
    }
    data <- x$get()
    cachedMatrix <- solve(data, ...)
    x$setcachedmatrix(cachedMatrix)
    cachedMatrix
}

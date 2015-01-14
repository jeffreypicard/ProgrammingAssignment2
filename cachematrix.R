## cachematrix.R
##
## Author: Jeffrey Picard (jeff@jeffreypicard.com)
##
## Functions to create a matrix inverse caching structure and to
## compute the inverse using caching.

## Takes a matrix and returns a list structure with functions to
## get the matrix, set the matrix, get the inverse and set the
## inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL		
	set <- function(y) {
		x <<- y
		inv <- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv

	list(set = set,
	     get = get,
	     setinv = setinv,
	     getinv = getinv)
}


## Takes a cache matrix structure and computes the inverse
## or returns the cached version if it already exists.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()	
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setinv(inv)
	inv
}

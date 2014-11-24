#############################################################################
## R Programming
## Assignment #2
##
## These 2 functions work together to create a special object that stores
## a matrix and its inverse.  These functions allow matrix inverses to be
## cached so they are computed only once.
#############################################################################

## --------------------------------------------------------------------------
## This function creates a special vector, which is really a list containing 
## a function to:
##  * set the value of the matrix
##  * get the value of the matrix
##  * set the value of the inverse
##  * get the value of the inverse
## --------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

	# Initialize the inverse to null
	inv <- NULL

	# set(m) function will set x equal to m and clear inv 
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	# get() function will return the uninverted matrix
	get <- function() x

	# setinverse(i) will set the inv equal to i
	setinverse <- function(inverse) inv <<- inverse

	# getinverse() will return inv
	getinverse <- function() inv

	# Create list of functions
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## --------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve
## the inverse from the cache. If not found then this function will solve
## the matrix and cache its inverse. 
##
## Input: An invertible matrix
## Output The matrix's inverse
##
## --------------------------------------------------------------------------
cacheSolve <- function(x, ...) {

	# Look up the inverse from the cache
	inv <- x$getinverse()

	# If inverse found in cache then return it
	if (! is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	# If inverse not found in cache then determine the inverse and
	# save it to the cache.
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
 
	# Return the inverse to the caller
	inv

}

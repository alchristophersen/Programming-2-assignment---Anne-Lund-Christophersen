## This file will create two functions:
## makeCacheMatrix: Creates a matrix object
## cacheSolve: Caches the inverse of the matrix


## This function creates a "matrix" object which takes a matrix as input and attach
## four functions to it such that is is possible to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	## set: Function to set the matrix itself
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	## get: Function to get the matrix itself
	get <- function() x
	## setInverse: Function to set the iverse of the matrix
	setInverse <- function(inverse) inv <<- inverse
	## getInverse: Function to get the iverse of the matrix
	getInverse <- function() inv
	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## This function will return the inverse of the matrix (from a matrix-object defined above) as well as 
## cache the inverse of the matrix if it not cached allready.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	## If the inverse of the matrix 'x' is cached,
	## then return the cached inverse
	if (!is.null(inv)) {
		message("Getting cashed data")
		return(inv)
	}
	## If the inverse of 'x' is not cached, then calculate the inverse,
	## cache it, and return it
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setInverse(inv)
	inv 
}

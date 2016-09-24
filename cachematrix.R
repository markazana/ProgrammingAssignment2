## Functions to store and find the inverse of  
## a square matrix.

## creates a special vector that contains functions to 
## get/set value of vector
## get/set inverse if matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
	x <<- y
	i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set = set, get = get,
	   setinverse = setinverse,
	   getinverse = getinverse)
}


## Solves the square matrix of the special "vector" 
## created with the above function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
	message("getting cached data")
	return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

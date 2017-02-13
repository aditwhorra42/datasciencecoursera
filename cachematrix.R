## Calculating the inverse of a matrix requires a lot of computation time and memory. If it has to be done multiple times it is more efficient
## if one makes a Cache function which stores the inverse then one can return the same value istead of calculating it again given that the matrix remains the same.

## makeCacheMatrx function takes a matrix as its argument and constructs the 4 basic functions to create cache for the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {  ## sets the value of the matrix
		x <<- y
		inv <- NULL
	}
get <- function() x ## gets the value of the matrix
	setinv <- function(inverse) inv <<- inverse ## sets the value of the inverse
	getinv() <- function() inv ## gets the value of the inverse
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks whether the value of the inverse of a matrix already exists or not. If it does, then it returns the cached value. Otherwise, it calulates
## the inverse of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
        inver <- x$getinv()
		if(!is.null(inver)) {
			print("Getting Cached inverse")
			return(inver)
		}
		data <- x$get()
		inver <- solve(data)
		x$setinv(inver)
		inver
}
		
	## Return a matrix that is the inverse of 'x'
}

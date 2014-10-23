## The functions below calculate the inverse of a matrix and saves it
## to the cache so that the next time the user tries to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a matrix object that 1) sets the value of the matrix;
## 2) gets the value of the matrix; 3) sets the value of the inverse; and 4)
## gets the value of the inverse

## create matrix object x
makeCacheMatrix <- function(x = matrix()) {

	## define the cache inv
	inv <- NULL
	
	## "set" is used to alter the matrix and invalidates the cache
	set <- function(y) {

		## assign the input matrix y to the variable x in the parent environment
		x <<- y	
		
		## reset inv in the parent environment to null
		inv <<- NULL	
	}

	## "get" returns the matrix x
	get <- function() {
	  x
	}

	## "setinv" sets the cache inv equal to the inverse of the matrix x
	setinv <- function(inverse) {
		inv <<- inverse
	}

	## "getinv" returns the cached inverse of x
	getinv <- function() {
		inv
	}

	## return the matrix
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Compute the inverse of the "matrix" above that is returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	## retrieve the cached inverse
	inv <- x$getinv()
	
	## if the inverse is cached, then return it
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	## else, calculate the inverse and print
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

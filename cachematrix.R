##Matrix inversion takes lots of computational power of a system.
##So there is lot of benefits in caching the inversion of matrix rather than computing
##it again again if data is same 


## compCacheMat creates a list containing a function to perform following task
# 1. setting the value in matrix
# 2. getting the value from matrix
# 3. set the inverse of matrix
# 4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function first checks if inverse has already been compute.If it is
## it returns the previously computed result and skips the calculation other wise
## it calculates and returns the inverse 
cacheSolve<- function(x, ...){
	inv<- x$getinverse()	
	if(!is.null(inv)) {
	message("getting invert matrix from cache.")
	return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv

}

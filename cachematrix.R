## These functions reduce the potentially time-consuming matrix inverse operation
## by caching the inverse in the first function, and retrieving the inverse if it has been 
## already calculated in the second function


## MakeCacheMatrix is a function that creates a special matrix object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve is a function that computes the inverse of the special matrix 
## returned by MakeCacheMatrix. If the inverse has already been calculated and
## the matrix has not been changed, this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinv(m)
	m       

}
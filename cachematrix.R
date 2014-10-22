## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix- Takes in a matrix. Returns a list of functions that can be used to set/get matrix/inverse
#cacheSolve- Calculates inverse of matrix if it is already not been computed else gets from cache.

## Write a short comment describing this function
##This function returns a list of functions for a specific matrix. Those functions can be used to change the 
##matrix,get the matrix, get the inverse and set the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL
	set <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}
	get <- function() x;
	setinverse <- function(inverse) inverse_matrix <<- inverse
	getinverse <- function() inverse_matrix
	list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##This function calculates the inverse of a specific matrix, if the inverse is already computed returns the 
##cached inverse otherwise calculates using solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse_matrix <- x$getinverse()
	if(!is.null(inverse_matrix)) {
		message("getting cached inverse")
		return(inverse_matrix)
	}
	matrix <- x$get()
	inverse_matrix <- solve(matrix)
	x$setinverse(inverse_matrix)
	inverse_matrix
}

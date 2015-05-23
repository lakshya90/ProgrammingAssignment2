## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special "matrix", which is really a list containing ##a function to
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse
##The special matrix can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	##Initializing inverse to NULL
	inverse <- NULL
	##Setting the matrix, x
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	##Getting the matrix, x
  	get <- function() x
	##Caching the value of the inverse
	setinverse <- function(solve) inverse <<- solve
	##Get the inverse
	getinverse <- function() inverse
	##Returning inverse
	list(set = set, get=get,setinverse=setinverse,getinverse=getinverse)

}



## Write a short comment describing this function
##The following function calculates the inverse of the special "matrix" created ##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets the## value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	##Get inverse
	inverse <- x$getinverse()   
	##Check presence of inverse and display message accordingly
	if(!is.null(inverse)){
		message("Getting cached data")
		return(inverse)
	}
	##Get matrix
	data<-x$get()
	##Using solve to compute matrix
	inverse<-solve(data,...)
	##To cache inverse
	x$setinverse(inverse)
	##Returning the inverse
	inverse
}

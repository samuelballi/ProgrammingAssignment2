## These couple of functions accept any user input of a inversible matrix and
## calculates and stores the inverse of matrix in cache.
## If the cacheSolve() is called for first time, it calculates is inverse of matrix
## and caches it.
## If cacheSolve() is called for matrix Vector for which inverse is available
## from a previous operation, it prints the available chached inverse of matrix.

##Initializing a matrix vector as a funciton which accepts a matrix as user input
##This vector also recieves and retains the calculated inverse matrix via cacheSolve()
##The variable 'x' is assigned as user input and 'i' is reset for any new Vector

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){ 
       		x <<- y
       		i <<- NULL
      	}
       get <- function() x
       setinverse <- function(inverse) i <<- inverse 
       getinverse <- function() i
       list(set = set, 
	     get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

## This funciton verifies if the calculated vector already has an inverse stored in cache
## If the inverse is available in cache, the inverse is printed
## If the inverse is not available the inverse is calculated and stored in cache for future use

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
	if(!is.null(i)){
        	message("getting cached data")
            	return(i)
        	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
      	i
}
## Put comments here that give an overall description of what your
## functions do
## Week 3 Assigntment 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special matrix
        inv <- NULL			##initializes inv data as NULL
        set <- function(y) {		##define set function to assign new data
                x <<- y			##set data of matrix in a different enviroment  
                inv <<- NULL		##if there is a new matrix, reste inv to NULL
        }
        get <- function() x		## this function returns the value of the matrix
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ## This function computes the inverse of a matrix returned by makeCacheMatrix
	  ## It looks for a previous computed value and returns the inverse from the cache.
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

##	1	set the value of the vector
## 	2	get the value of the vector
##	3	set the value of the inverse
##	4	get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	
	        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "vector" created with the above function. It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it uses solve with the assumption this is a square matrix to calculates the inverse of the data and sets the value of the inverse  in the cache via the solve function.


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

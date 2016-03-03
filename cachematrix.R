## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function creates a special "matrix" object that can cache it's inverse
## 4 functions 1.set() 2.get() 3.setinv() 4.getinv()
makeCacheMatrix <- function(x = matrix()) {
	 inv <- NULL
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        setinv <- function(solve)inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
 	inv <- x$getinv()
        if(!is.null(inv)) { # Checks if the inverse is aldready been calculated
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
       inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

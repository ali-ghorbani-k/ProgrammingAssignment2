## This code includes two functions that caches the inverse of a 
## matrix by taking advantage of lexical scoping in R language.
## Matrix inversion is usually a costly computation 
## and there is computational benefit to caching the inverse of
## a matrix rather than compute it repeatedly.


## makeCacheMatrix bulits a set of functions and returns 
## the functinos within the list to the parent environment. 
## the <<- operator is used to assign a value to an object 
## in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve retrieve the inverse of previosuly-computed inverse 
## of a matrix from cache memory. If it is a new x matrix, 
## CacheSolve calculate the matrix invere using "solve" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

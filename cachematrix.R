## Put comments here that give an overall description of what your
## functions do

## accepts input matrix and caches its inverse if it is already computed else assigns it to null

makeCacheMatrix <- function(x = matrix()) {
##assign inverse to null initially
 inverse <- NULL

        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        get <- function() x

        setinverse <- function(inv) inverse <<- inv

        getinverse <- function() inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## accepts the output of the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$getinverse()

        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        data <- x$get()

        inv <- solve(data, ...)

        x$setinverse(inv)

        inv
}

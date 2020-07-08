## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets the value of the matrix, gets the value of the matrix, sets the value of the inverse and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created from the function above.  It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache.
## Othervwise it calculates the inverse and sets the value in the cache
cacheSolve <- function(x, ...) {
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

##This pair of functions caches the inverse of a matrix

##This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv <<-inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function computes the inverse of the matrix returned by makeCacheMatrix.
##If the inverse has already been calculated and the matrix has not changed, 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    } else {
        inv <- solve(x$get())
        x$setinverse(inv)
        return(inv)
    }
}

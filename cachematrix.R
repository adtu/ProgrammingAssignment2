## cachematrix function allows the value of the inverse of a matrix to be computed and cached for later retrieval. 
## This avoids the inverse having to be calculated repeatedly which is usually costly and can be avoided by caching 
## the calculated value and retreiving it from cache later on.
## It contains two functions with their descriptions below: makeCacheMatrix and cacheSolve.

## makeCacheMatrix function creates a special "matrix" which is a list containing a function to:
## 1. set value of the input matrix
## 2. get value of the input matrix
## 3. set value of the inverse of the input matrix
## 4. get value of the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function computes the inverse of the matrix from the above makeCacheMatrix function. 
## The function first checks to see if the inverse has already been computed. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the data and sets the value of the inverse 
## in cache using setinverse function.

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

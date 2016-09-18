## Wenju Cui   9-18-2016
# Week 3 Programming Assignment


## 1. makeCacheMatrix
## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It will: 
## set the value of the Matrix
## get the value of the Matrix
## set the inverse of the Matrix
## get the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## 2. cacheSolve
## The following function calculates the inverse of the matrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the result from the cache and skips the computation. 
## Otherwise, it calculates the inverse, and sets the value in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")    # message() is better than print()
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...) 
    x$setinverse(m)
    m
}


## Test
a <- matrix(1:4, nrow=2, ncol=2)
a
solve(a)  ## to check if results match

b <- makeCacheMatrix(a)
cacheSolve(b)
cacheSolve(b)    ## message printed in red -- getting cached data

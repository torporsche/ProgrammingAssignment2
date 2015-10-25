## rprog-033 R Programming Assignment 2

## Work is based on the makeVector and cachemean function examples provided
## in the assignment instrucions.

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The makeCacheMatrix
## and cacheSolve functions are used together to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initiates m as NULL
        m <- NULL
        
        ## Sets the value of the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
         }
       
        ## Gets the value of the matrix
        get <- function() x
       
        ## Sets the value of inverse of the matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## Gets the value of inverse of the matrix
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the matrix 
## from the cache and skips the computation. Otherwise, it computes the matrix and places it in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        ## Checks if m is not NULL
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        ## Solves inverse from cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m       
}

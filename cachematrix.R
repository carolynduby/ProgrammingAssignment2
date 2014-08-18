## Calculates and caches the inverse of a matrix.  
##
## Example:
## > cm1 <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
## > cacheSolve(cm1)
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(cm1)
## getting cached data
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
##
## Use these functions when the result of an expensive inverse operation is calculated and then used 
## several times.  
## Computes the inverse of the matrix the first time it is requested and then stores(caches) it. 
## On subsequent calls the stored(cached) inverse is returned.
## 
## WARNING: This implementation requires more memory because it stores the inverse of each defined matrix 
## in memory even if it is not currently used.
## 

## Creates a special matrix that stores its own value and the value of its inverse.
## To be used with the cacheSolve function.
##
## Arguments:
## x: The original value of the matrix to be inverted.  Stores an empty matrix by default.
## returns: a special matrix capable of storing its value and the value of its inverse
## 
## Example:
## > cm <- makeCacheMatrix()
## > cm$set(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
## > cm$get()
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > cm$setinverse(solve(cm$get()))
## > cm$getinverse()
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## change the value of the cache matrix
    ## null out its inverse because the previous value is no longer valid
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## return the value of the cache matrix
    get <- function() x
    
    ## set the value of the inverse of the cache matrix
    setinverse <- function(new_inverse) inverse <<- new_inverse
    
    ## return the value of the inverse of the cache matrix
    getinverse <- function() inverse
    
    ## return a list representing the special matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Return the inverse of the x argument.  Calculate the inverse only if it has not been calculated before.
## A matrix multiplied by its inverse equals the identity matrix.
##
## Example:
## > cm1 <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
## > cacheSolve(cm1)
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
cacheSolve <- function(x, ...) {
    
    ## If the inverse has already been calculated, return the stored value
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## calculate the inverse of the matrix
    data <- x$get()
    inverse <- solve(data, ...)
    
    ## store the inverse so we don't have to calculate it next time
    x$setinverse(inverse)
    
    ## return the inverse value
    inverse     
}

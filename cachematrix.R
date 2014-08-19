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
## On subsequent calls the stored(cached) inverse is returned as long as the cached value was calculated 
## with the same arguments.  If it is recalculated if the arguments don't match.
## 
## WARNING: This implementation requires more memory because it stores the inverse of each defined matrix 
## in memory even if it is not currently used.
## 

## Creates a special matrix that can store its own value and the cache the value of its inverse.
## To be used alone or with the cacheSolve function.
##
## Arguments:
## original: The original value of the matrix to be inverted.  Defaults to empty matrix.
## returns: a list of functions defined on the special matrix capable of storing and getting its value as well as
##          calculating the inverse
## 
## Example:
## > cm <- makeCacheMatrix()
## > cm$set(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
## > cm$get()
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > cm$getinverse()
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
makeCacheMatrix <- function(original = matrix()) {
    inverse <- NULL
    params <- NULL
    
    ## change the value of the original matrix
    ## null out its inverse and its parameters because the previous value may no longer valid
    set <- function(new_original) {
        original <<- new_original
        inverse <<- NULL
        params <<- NULL
    }
    
    ## return the value of the original matrix
    get <- function() original
    
    ## return the value of the inverse of the original matrix
    ## only calculate the value if it has not been calculated before
    getinverse <- function(...) {
        function_params <<- as.list( match.call())[-1]
        ## calculate and store the inverse if it has not been stored already
        ## recalculates if the parameters changed since the previous calculation
        if (is.null(inverse) || !identical(params, function_params)) {
            inverse <<- solve(original, ...)
            params <<- function_params
        } else {
            message("getting cached data")
        }
        inverse
    }
    
    ## return a list representing the functions of the special matrix
    list(set = set, get = get,
         getinverse = getinverse)
    
}


## Return the inverse of a cache matrix.  Calculate the inverse only if it has not been calculated before.
## A matrix multiplied by its inverse equals the identity matrix.
##
## Arguments:
## cache_matrix: The special matrix to be inverted.  Create using the makeCacheMatrix function.
## returns: The inverse of the special matrix
##
## Example:
## > cm1 <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow=2, ncol=2))
## > cacheSolve(cm1)
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
cacheSolve <- function(cache_matrix, ...) {
    
    cache_matrix$getinverse(...)
    
}

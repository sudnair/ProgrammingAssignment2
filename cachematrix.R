## This function creates a special "matrix" object that can cache its inverse
## Provides a set of functions to operate on the matrix value in cache
## such as get value, set value etc.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse matrix to NULL when caching is requested
    inverseMatrix <- NULL
    
    ## Can be used to reassign x to a new matrix value	
    ## without calling the entire makeCacheMatrix function 
    set <- function (y) {
        x <<- y
        inverseMatrix <<-NULL
    }
    
    ## Returns value of the matrix provided for caching
    get <- function () x
    
    ## Stores the value of the inverse matrix in the cache	
    setInverse <- function(inverseVal) inverseMatrix <<- inverseVal
    
    ## Retrieves the value of the inverse matrix stored in cache (if any)
    getInverse <- function() inverseMatrix
    
    ## Returns a list with all the functions. 
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Retrieves inverse matrix stored in the cache (if any)
    inverseMatrix <- x$getInverse()
    
    ## Checks if inverse matrix value is available in the cache. 
    if(!is.null(inverseMatrix)){
        ## If inverse matrix is available in cache, display message
        message("Retreiving Inverse Matrix from cache")
        ## Return the inverse matrix value
        return(inverseMatrix)
    }
    ## Inverse matrix value is not available in cache.	
    
    ## Retrieves the value of matrix stored in the cache.
    data <- x$get()	
    
    ## Caculates the inverse value of the matrix
    inverseMatrix <- solve(data,...)
    
    ## Stores the inverse value of the matrix in the cache
    x$setInverse(inverseMatrix)	
    
    inverseMatrix
    
}

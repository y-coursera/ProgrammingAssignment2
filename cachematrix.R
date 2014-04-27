## The following functions will cache the inverse of matrix and retreive the cached inverse

## The function makeCacheMatrix is used to cache the inverse of the specified matrix
## makeCacheMatrix returns a list containing the following functions:
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set the value of the inverse
## getinverse - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve returns the inverse of the matrix
## If the inverse has already been calculated and cached, the cached value is returned
## Otherwise, the inverse is calculated

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
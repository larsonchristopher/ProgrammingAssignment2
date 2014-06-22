## These functions calculate the inverse of a matrix that can be cached rather
## than computing the inverse repeatedly.  

## The makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## i = variable to cache the inverse matrix
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Get the matrix
        get <- function() x
        
        ## Set the inverse matrix
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## Get the inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.  If the inverse has already been calculated 
## and the matrix has not changed, then cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## First check to see if inverse has already been calculated
        ## If so, retrieve inverse from cache and skip the computation
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        
        ## If not, then get the inverse matrix
        data <- x$get()
        i <- solve(data, ...)
       
        ## Set the inverse matrix
        x$setinverse(i)
        i        
}

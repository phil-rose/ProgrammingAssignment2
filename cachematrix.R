makeCacheMatrix <- function(m = matrix()) {
        a <- NULL
        setMatrix <- function(matrix) {
                m <<- matrix
                a <<- NULL
        }
        
        ##function to get matrix
        getMatrix <- function() {
                m
        }
        
        ##function to call cache inverse of matrix
        cacheInverse <- function(inverse) {
                a <<- inverse 
        }
        
        ##function to get the inverse of matrix
        getInverse <- function() {
                a
        }
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
        cacheInverse = cacheInverse, getInverse = getInverse)
}

##computes the inverse of matrix object, maekCacheMatrix
cacheSolve <- function(m, ...) {
        ##defines call to get inverse of matrix
        a <- m$getInverse()
        ##sets a if not null condition to return a message if inverse is not null
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        ##a call to get data from the matrix
        data <- m$getMatrix()
        ##a call to get inverse of matrix
        a <- solve(data, ...)
        #a call to cache the inverse matrix
        m$cacheInverse(a)
        #a call to return inverse matrix
        a
}



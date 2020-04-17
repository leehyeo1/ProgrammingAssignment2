## makeCacheMatrix() takes matrix as an input argument and has a list of four functions.  
## First function, 'set' function, replace the matrix set by parent function with its argument.  
## Second function, 'get' function, returns current matrix stored.  
## Third function,'setinverse', set the inverse of the matrix if it is possible
## Finally, fourth function, 'getinverse', returns inverse matrix

## cacheSolve() extract cached data and use them to do its own calculation if cached data exists.

## A function that has a list of four functions
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        ## Below codes can be applied for both with and without assumption that matrix supplied is 
        ## always invertible
        setinverse <- function(inverse = NULL) {
                if(!is.null(inverse)) {
                        i <<- inverse
                } else {
                        if(nrow(x) != ncol(x) || det(x) == 0) {
                                return("The matrix is not invertible")
                        } else i <<- solve(x)
                }
        }
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## A function that extracts cached data and do some calculations depending on situations
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("cached data is extracted")
                return(i)
        }
        
        matrix <- x$get()
        
        ## Below codes can be applied for both with and without assumption that matrix supplied is 
        ## always invertible
        if(nrow(matrix) != ncol(matrix) || det(matrix) == 0) {
                return("The matrix is not invertible")
        }
        
        ## solve() does calculate inverse of computed matrix
        i <- solve(matrix)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

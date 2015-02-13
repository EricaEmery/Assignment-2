## Computing and caching the inverse of a square, invertible matrix 
## by Erica Emery

## Creates a list of 4 functions to set and get values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        # the list of 4 functions is returned
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Returns the matrix inverse given the output of the above function

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        # if inverse has already been calculated, it is not recomputed
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # if inverse hasn't been calculated, it is computed
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        ## Return a matrix that is the inverse of 'x'
        m        
}

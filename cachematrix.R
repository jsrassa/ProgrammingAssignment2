## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  This functions returns a list that gets the value of the matrix and also allows to set this value.
## Additionally, this function gets the inverse of the matrix (if it has been stored previously) and also allows to set the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function


##  This functions checks if the inverse of the matrix has already been calculated. If this is true, the function returns the matrix which it gets from the cache.
## If the inverse has not been calculated, this function gets the data, calculates the inverse and stores the inverse in the cache via the set inverse function.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        nr <- nrow(data)
        b <- diag(nr)
        m <- solve(data, b, ...)
        x$setinverse(m)
        m
}

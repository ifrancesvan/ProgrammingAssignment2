## This program will obtain the inverse of a matrix, but first
## it will check if the inverse of such matrix (that the user introduces in ## the program) has already been calculated, if it does then it returns the ## value, if not it will calculate the inverse of the matrix and then return ## the inverse.

## This function will set and get the matrix and its inverse. 

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


## This function will check if the inverse has already been calculates, if ## not it will calculate it.

cacheSolve <- function(x, ...) {
              m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

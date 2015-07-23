##My functions take a square invertible matrix and compute his inverse. Then, it caches its inverse for later use.

## This function takes for argument a matrix and returns a list with the matrix in it and when computed its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function takes the list from the above function and checks if the matrix given has been already been computed.
## If it has, it returns a message and gives you the cached inverse. If not, the function computes the inverse, than caches it and returns the inverse to the user.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

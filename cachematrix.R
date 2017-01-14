## The two pair of function below are used to create special "matrix" object that stores a matrix and caches the inverse of that matrix

## makeCacheMatrix: The first function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    i   <-  NULL
    set <-  function(a) {
        x <<- a
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve: The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above, if the inverse has already been calculated, then cacheSolve should retrive the inverse from the cache

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}


## The following is a pair of functions to calculate the inverse of a matrix. 
## Because calculating the inverse of a matrix can be computationally costly
## there is a benefit to avoiding repeated calculation if it is not necessary.
## This can be done by by caching the result (i.e. storing it) for later use.


## The makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned from the makeCacheMatrix function
## If the the inverse has already been calculated and has not changed, this function allows 
## the inverse to be pulled directly from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv  
}

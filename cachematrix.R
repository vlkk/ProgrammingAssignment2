## Coursera R Course
## Programming assigment 2
## Create a matrix that caches its inverse
## Testing Environment scoping properties in R.


## makeCacheMatrix creates an object that also can cache its inverse.
## Returns a list of functions for retrieving the matrix and its inverse.

makeCacheMatrix <- function(m = matrix()) {
    cachedInv <- NULL
    
    set <- function(m1) {
        m <<- m1
        cachedInv <<- NULL
    }
    
    get <- function() {
        m
    }
    
    setInverse <- function(inverse) {
        cachedInv <<- inverse
    }
    
    getInverse <- function() {
        cachedInv
    }
    
    list(set = set, get = get, 
        setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve returns the inverse of a matrix created makeCacheMatrix
## Will calculate inverse if necessary, otherwise return cached inverse value

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <- m$getInverse()
   if (is.null(inv)) {
      message("Calculating cache")
      # Calculate and cache inverse
      inv <- solve(m$get())
      m$setInverse(inv)
   }
   else {
      message("Returning cached data")
   }
   inv
}

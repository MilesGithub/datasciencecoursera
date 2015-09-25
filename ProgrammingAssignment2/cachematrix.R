## Put comments here that give an overall description of what your
## functions do

# Matrix inversion can be a costly computation and can benefit
# to caching the inverse of a matrix so it doesn't need to be
# compute more than once. The following two functions are used 
# to cache the inverse of a matrix.


## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Write a short comment describing this function

# The following function returns the inverse of the matrix. 
# It checks if the inverse has already been computed. 
# If the inverse has already been computed, it gets the result and skips the computation.
# If the inverse has already has not been computed the inverse, sets the value in the cache.


cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
    
  }
  
  m <- x$get()
  
  i <- solve(m, ...)
  
  x$setInverse(i)
  
  i
  
}



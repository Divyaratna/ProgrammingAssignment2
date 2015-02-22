# The following assignment contains a R function which is able to cache computations. It is composed
# of two parts and functions: makeCacheMatrix and CacheSolve.
#
# 1. Function makeCacheMatrix: Creates a special object (matrix), which caches its inverse. It stores
#    a matrix and a cached value.  
#    makeCacheMatrix consists of the following functions:
#   * setMatrix: sets the value of the matrix
#   * getMatrix: gets the value of the matrix
#   * setCacheValue: sets the cached value 
#   * getCacheValue: get the cached value 

# Function assigned to makeCacheMatrix

 makeCacheMatrix <- function(x = numeric()) {
  
# set initial value to NULL
  cacheValue <- NULL
  
# store the matrix to setMatrix
   setMatrix <- function(newValue) {
     x <<- newValue
    
# flush the cache value by assigning NULL to cacheValue
    cacheValue <<- NULL
  }
  
# Returns the stored matrix
  getMatrix <- function() {
    x
  }
  
# Allocate the solve parameter to the cache
  setCacheValue <- function(solve) {
    cacheValue <<- solve
  }
  
# Return the cached value
  getCacheValue <- function() {
    cacheValue
  }
  
# Return a list with all the above defined functions.
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setCacheValue = setCacheValue, getCacheValue = getCacheValue)
}


# 2. CacheSolve: The following function computes the inverse of a square invertible matrix, which is created
#    with and returned by makeCacheMatrix.

# Assign function to cacheSolve
 cacheSolve <- function(y, ...) {
  
# Get cached value and allocate it to inverse
  inverse <- y$getCacheValue()
  
# If there is already a calculated value in the cache, it is returned
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
# If not then get the matrix from function getMatrix, caclulate the inverse and store it in
# the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$setCacheValue(inverse)
  
# Return the inverse
  inverse
}

# Happy End  

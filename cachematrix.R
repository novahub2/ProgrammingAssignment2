# Creates a special matrix with functions to get and set 
# the matrix and its corresponding inverse matrix
#
# Args:
#   mat: defaults to an empty matrix if none provided
#
# Returns:
#   List containing functions for:
#     set - to set a matrix
#     get - to get current matrix 
#     setInverse - to set inverse of matrix
#     getInverse - to retrieve inverse of matrix

makeCacheMatrix <- function(mat = matrix()) {
  
  # Initialize the variable for inverse matrix
  invMat <- NULL
  
  # Set mat variable with matrix and set inverse matrix to null
  set <- function( matrix ) {
    mat <<- matrix
    invMat <<- NULL
  }
  
  # Gets matrix
  get <- function() {
    mat
  }
  
  # Sets inverse matrix
  setInverse <- function(inverse) {
    invMat <<- inverse
  }
  
  # Gets inverse matrix
  getInverse <- function() {
    invMat
  }
  
  # Returns a list functions for special matrix
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


# The following function will return an inverse of a special matrix
#
# Args:
#   x: needs a special matrix such as 'makeCacheMatrix' above with list of functions
#
# Returns:
#   The inverse of the matrix corresponding to the special matrix argument given.
#   Returns the cache of the inverse of the matrix if previously calculated

cacheSolve <- function(x, ...) {
  
  # Gets the already calculated inverse matrix (if any)
  invMat <- x$getInverse()
  
  # If invMat is not NULL return the already calculated inverse matrix
  if( !is.null(invMat) ) {
    message("getting cached data") # message user that it was derived from cache
    return(invMat)
  }
  
  # Gets the matrix 
  mat <- x$get()
  
  # Calculates inverse of matrix using solve function
  invMat <- solve(mat)
  
  # Sets inverse of matrix
  x$setInverse(invMat)
  
  # Returns inverse of matrix
  invMat
}
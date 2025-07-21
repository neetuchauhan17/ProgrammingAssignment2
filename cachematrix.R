## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will hold the cached inverse
  
  set <- function(y) {
    x <<- y     # Set new matrix
    inv <<- NULL  # Reset the cached inverse
  }
  
  get <- function() x  # Return the matrix
  
  setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getinverse <- function() inv  # Return the cached inverse
  
  # Return a list of the functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return cached inverse
  }
  
  data <- x$get()         # Get the original matrix
  inv <- solve(data, ...) # Compute the inverse
  x$setinverse(inv)       # Cache the inverse
  
  inv  # Return the inverse
}

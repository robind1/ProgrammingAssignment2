makeCacheMatrix <- function(x = matrix()) {
## Initialize a variable to store the inverse
  j <- NULL
  ## Function to set the matrix
  set <- function(y) {
    x <<- y  ## Using <<- to assign the value
    j <<- NULL  ## Reset the stored inverse 
  }
  ## Function to get the matrix
  get <- function() x
  ## Function to set the inverse 
  setInverse <- function(inverse) j <<- inverse  ## Using <<- to assign value
  ## Function to get the inverse 
  getInverse <- function() j
  ## Return a list of functions for matrix and inverse
  list(
    set = set,  ## Function to set the matrix
    get = get,  ## Function to get the matrix
    setInverse = setInverse,  ## Function to set the inverse
    getInverse = getInverse  ## Function to get the inverse
  )
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse() ## Check if the inverse is already cached
  if (!is.null(j)) {
    message("getting cached data")
    return(j)  # Return the cached inverse
  }
  ## calculate inverse if not cached
  mat <- x$get()  ## Get the matrix from the cache
  j <- solve(mat, ...)  ## Calculate the inverse
  ## Cache the calculated inverse for future
  x$setInverse(j)
  # Return the inverse
  j
}

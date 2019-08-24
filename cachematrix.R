## For Assignment 2 of R Programming
## The requirements for this question were a bit vague, I borrowed from the example that was presented.
## These functions will only cache one inverted matrix.
## Example usage
## mat <- matrix(rnorm(9), nrow = 3, ncol =3)
## cacheFunction <- makeCacheMatrix()
## cacheSolve(mat, cacheFunction)
## cacheSolve(mat, cacheFunction)

## This function takes in a matrix defulting to the empty matrix, it returns a list of fucntions for caching.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(
    set = set,
    get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix
  )
}


## Takes in a matrix as parameter x to be inverted,
## An instance of makeCacheMatrix function should be passed into the ellipses to allow for caching.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  cacheFunctionsList = as.list(...)
  cachedMatrix <- cacheFunctionsList$get()
  cachedMatrixValue <- cacheFunctionsList$getmatrix()
  
  if (!is.null(cachedMatrixValue) && is.matrix(x) && is.matrix(cachedMatrix) &&
      dim(x) == dim(cachedMatrix) && all(x == cachedMatrix)) {
    print("Get cached")
    cacheFunctionsList$getmatrix()
  } else {
    cacheFunctionsList$set(x)
    cacheFunctionsList$setmatrix(solve(x))
    cacheFunctionsList$getmatrix()
  }
}

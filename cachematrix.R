## Put comments here that give an overall description of what your
## functions do
# R Programming Week 3 Assignment
# Github user: esterhm
# Functions create a matrix, cache its inverse, then computes the inverse.

## Write a short comment describing this function
# This function creates a special "matrix" that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <-- y
            m <<- NULL
      }
      get <- function()
            x
      setInverse <- function(inverse)
            m <<- inverse
      getInverse <- function()
            m
      list(
            set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse
      )

}


## Write a short comment describing this function
# This function computes the inverse of the matrix returned by makeCacheMatrix.
# If the matrix is unchanged & the inverse already calculated,
# cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting caches matrix inverse")
            return(m)
      }
      dataInverse <- x$get()
      m <- solve(dataInverse)
      x$getInverse(m)
      m
}
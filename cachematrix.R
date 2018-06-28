## Two functions regarding the caching and computation of a matrix inverse 

## Function to cache a matrix inverse together with simple functions to get/set a matrix and to get/set the matrix inverse
## Input: x - a matrix, optional
## Output: List with functions to get/set the matrix and matrix inverse
makeCacheMatrix <- function(matrix = matrix()) {
      matrixInverse <- NULL
      setMatrix <- function(m) {          #set new matrix, clear inverse
            matrix <<- m
            matrixInverse <<- NULL
      }
      getMatrix <- function() {           #get matrix
            matrix
      }
      setInverse <- function(inverse) {   #set inverse
            matrixInverse <<- inverse
      }
      getInverse <- function() {          #get inverse
            matrixInverse
      }
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Computes the matrix inverse if none is cached
## Input:  Special Matrix x (list returned by makeCacheMatrix)
## Output: Inverse of matrix m
cacheSolve <- function(x, ...) {
      ## Check if cached inverse is available
      matrixInverse <- x$getInverse()
      if(!is.null(matrixInverse)){
            ##return cached inverse
            message("Use Cache") ##remove for a productive function
            matrixInverse
      } else {
            ## Compute new inverse and set it
            message("Compute new matrix") ##remove for a productive function
            matrix <- x$getMatrix()
            matrixInverse <- solve(matrix)
            x$setInverse(matrixInverse)
            matrixInverse
      }
}

## This file has the following 2 R functions - Part of the Programming assignment2
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## 	If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
## These have been tested with the unit test cases described in the discussion forum

makeCacheMatrix <- function (matrixIn=data.frame()) { 	## "makeCacheMatrix" Function that sets up the list of 4 atomic functions
  matrixInv = NULL					## Initialise the result/ output (Inverse)
  getMatrix  <- function() {matrixIn}			## "getMatrix" function - Returns the Input matrix
  setMatrix  <- function(matrixIn2) {			## "setMatrix" function - To reset the input
    matrixIn <<- matrixIn2				## sets up the input
    matrixInv <<- NULL					## initialises the output (inverse)
  }
  getInverse <- function () {matrixInv}			## "getInverse" function - retrieves the result from cache
  setInverse <- function (Inv) {matrixInv <<- Inv} 	## "setInverse" function - updates / sets the result in cache
  list (getMatrix = getMatrix, setMatrix = setMatrix 	## Return the above 4 functions as a list
	,getInverse = getInverse, setInverse = setInverse)
}

cacheSolve <- function (mcm, ...) {			## "cacheSolve" Function to compute / retrieve the inverse of a matrix
  Inverted <- mcm$getInverse()				## Get the cached value of the result
  if (is.null(Inverted)) {				## Not found scenario
    message ('not found - calculating inverse')		## Display the scenario - just for clarity
    Invalue <- mcm$getMatrix()				## use "getMatrix" to get the input
    Inverted <- solve(Invalue)				## use the SOLVE function to compute the inverse
    mcm$setInverse(Inverted)				## use "setMatrix" to set up the result in the cache
  }
  else {						## The "found in cache" scenario
    message ('Retrieving from Cache')
  }
  Inverted						## return the result
}
## R - ProgrammingAssignment2
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inmat <- NULL
  set <- function(y) {
    x <<- y
    inmat <<- NULL
  }
  get <- function() x
  setInver <- function(inver) inmat <<- inver
  getInver <- function() inmat
  list(set = set, get = get, setInver = setInver, getInver = getInver)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inmat <- x$getInver()
  if (!is.null(inmat)) {
    message("getting cached data")
    return(inmat)
  }
  matrx <- x$get()
  inmat <- solve(matrx, ...)
  x$setInver(inmat)
  inmat
}

## Testing
## samplematrix <- makeCacheMatrix(matrix(20:23, 2, 2))

## cacheSolve(samplematrix)
## [,1] [,2]
## [1,] -11.5   11
## [2,]  10.5  -10

## cacheSolve(samplematrix)
## getting cached data
## [,1] [,2]
## [1,] -11.5   11
## [2,]  10.5  -10
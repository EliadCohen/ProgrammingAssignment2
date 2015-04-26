## makeCacheMatrix and cacheSolve are an overloaded version of matrix and solve that allow 
##caching of the inverse matrix

## makeCacheMatrix creates an environment for a given matrix x 
##and stores the values of the matrix and the inverse matrix when calculated through cacheSolve
##Use makeCacheMatrix instead of matrix to create this special environment.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL ##This is the inverse matrix to x
  set <- function(y) {
    x <<- y 
    minv <<- NULL ##When setting a new matrix to x, need to clear the cached inverse matrix
  }
  get <- function() x
  setinv <- function(solve) minv <<- solve
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Use cacheSolve instead of solve with variables made using makeCacheMatrix to find
## the inverse matrix and store the solution in the makeCacheMatrix type variable (caching)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data for inverse matrix")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}


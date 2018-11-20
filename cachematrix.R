## Caching the inverse of a matrix

## makeCacheMatrix function creates a list containing 4 functions to set the matrix, get the matrix,
## set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve funciton calculates the inverse of the matrix with the above function. If the inverse has 
## already been calculated, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  inv
}

## These two functions allow storing the inverse of a matrix and use the stored value instead of re-calculating it. 
## If a new matrix is inputted then a new inverse matrix will be calculated but not more than once for the same input matrix

## makeCacheMatrix creates an object that contains a matrix that is inputted into it, an inverse of the matrix (null before it is calculated) and get,set,getinv,setinv functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will check if the matrix object that is inputted into it already has a calculated inverse. 
## If it does then it will return the cached value, if not then the inverse will be calculated and value cached for this particular matrix to be used later if needed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

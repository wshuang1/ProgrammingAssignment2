## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  InverseMat <<- NULL
  
  set <- function(y) {
    if (!isTRUE(all.equal(x ,y))) {
      x <<- y
      InverseMat <<- NULL
    }
  }
  get <- function() x
  setInverse <- function(inverse) InverseMat <<- inverse
  getInverse <- function() InverseMat
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  IM <- x$getInverse()
  if (!is.null(IM)) {
    message("Cached result return!!")
    return(IM)
  }
  m <- x$get()
  IM <- solve(m)
  x$setInverse(IM)
  IM
}

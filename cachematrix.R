## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly.
## The functions makeCacheMatrix() and cacheSolve() cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse value I to null
  I <- NULL
  # set the value of the matrix
  set <- function(y){
    # <<- modifies the variable x and I found by walking up the parent environments.
    x <<- y
    I <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inverse) I <<- inverse
  
  # get the value of the inverse
  getinverse <- function() I
  
  # return a list of set, get, setinverse and getinverse
  list(set = set, get = get,  setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It first checks if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the 
## value of the matrix in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  # first check if the inverse has already been cached
  I <- x$getinverse()
  if(!is.null(I)){
        message("getting cached data")
        return(I)
  }
  # if not, then get the matrix 
  data <- x.get()
  # compute the inverse
  I <- inv(data,...)
  #cache the inverse
  x$setinverse(I)
  # Return a matrix that is the inverse of 'x'
  I
}


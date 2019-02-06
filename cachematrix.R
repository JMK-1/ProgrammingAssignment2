## The makeCacheMatrix function creates a list containing a function that sets the values of the matrix, gets the values of the matrix, sets the value of the inverse of the matrix, and gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
      x<<- y
      m<<- NULL
  }
  get <- function(x) x
  setinverse<- function(inverse) m <<- inverse
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}


## The cachematrix function calculates the inverse of the matrix created with the above function. If the inverse of the matrix has already been calculated, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setmatrix function.

cachematrix <- function (x, ...) {
    m<- x$getinverse()
    if(!is.null(m)){
          message("getting cached data")
          return(m)
    }
    data<- x$get()
    m<- solve(data, ...)
    x$setinverse(m)
    m
}

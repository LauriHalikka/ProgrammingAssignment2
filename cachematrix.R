## makeCacheMatrix creates functions that are used in cacheSolve to invert the matrix.
## Note that using the set() function the undelying matrix can be easily changed in 
## the working environment e.g. a$set(((matrix(c(sample(1:10,16, replace=T)),4,4))))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  cacheSolve inverts the matrix if it is not found in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}

## test the code by running 
## x<-matrix(c(sample(1:10,16, replace=T)),4,4)
## a<-makeCacheMatrix(x)
## cacheSolve(a). 

## Put comments here that give an overall description of what your
## functions do
## (tmdata) Description of the steps:
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse
## Get the value of the inverse

## Write a short comment describing this function
## (tmdata) Description of the function:
## The first function, makeCacheMatrix creates a special "matrix" as per the steps described above. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## (tmdata) Description of the function:
## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks if the inverse has already been calculated, if yes, it gets the inverse from the cache and kips the computation.
## Otherwise, it calculates the inverse of the datas and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Computing the inverse of a matrix is a costly operation, so caching it  
## can speed up repetitive and computational expensive functions
## +makeCacheMatrix - this function is used to store a matrix object and its inverse
## +cacheSolve - used to calculate the inverse of a function, or retrive it,
##               if it was already calculated


## The function used to store the matrix and it's inverse
## The get and set functions are used to get the value of the matrix stored
## The getinverse and setinverse function retrieve the stored value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve - retrive the inverse for a matrix
## It first checks if it was already cached, and if not, it calculates it and stores it

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  ## If the inverse of the matrix was calculated then just return the cached value
  if (!is.null(inverse)){
    print("getting cached inverse")
    return(inverse)
  }

  ## Calculate the value of the inverse and store the result
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  
  ## Return the inverse
  inverse
}

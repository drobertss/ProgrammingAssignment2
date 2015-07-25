## Two functions used to cache the inverse of a matrix.

## makeCacheMatrix sets/gets a matrix and sets/gets the value of an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  
  get <- function()x
  
  setmatrix <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set =set, get = get,
       setmatrix = setmatrix,
       getinverse = getinverse)
       
}

# cacheSolve returns the value of an inverse of the matrix. If the value has already been calculated
# it will detect that value and return it. 

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix <-x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

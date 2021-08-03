makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message('obtaining cached data')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# COMPUTING INVERSE OF A SQUARE MATRIX
# Create z matrix
zmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
 zmatrix$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
 zmatrix$getInverse()
NULL
 cacheSolve(zmatrix)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
 cacheSolve(zmatrix)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
 zmatrix$getInverse()
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
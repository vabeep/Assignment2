makeCacheMatrix <- function(x = matrix()) {
  in_mat <- NULL
  set <- function(y) {
    x <<- y
    
    in_mat <<- NULL
  }
  get <- function(){x} 
  setinverse <- function(inverse){in_mat <<-inverce}
  getinverse <- function() {in_mat}
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  in_mat <- x$getinverse()
  if (!is.null(in_mat)) {
    message("getting cached inverse matrix")
    return(in_mat)
  }
  data <- x$get()
  in_mat <- solve(data, ...)
  x$setinverse(in_mat)
  in_mat
  

  }

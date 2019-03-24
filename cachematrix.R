## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   
  set <- function(x2){     
    x <<- x2     
    i <<- NULL   
  }   
  get <- function() x   
  setInverse <- function(mtx) 
    i <<- mtx   
  getInverse <- function() i   
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()   
  if(!is.null(i)){     
    return(i)   
  }   
  data <- x$get()   
  i <- solve(data)   
  x$setInverse(i)   
  i       

}

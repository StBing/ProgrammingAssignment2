## Function 1: makeCacheMatrix()

## A function which creates a 'special matrix' object, which can cache
## its inverse.


## takes in a square invertible matrix (x), and returns a list of functions
## which:
##            1. set matrix
##            2. get matrix
##            3. set the inverse matrix 
##            4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
  # set the inverse to NULL, a placeholder for a future value
  inv <- NULL
  
  # 1. defines a function...
  set <- function(y) 
  {
    # ...to set the matrix, x,  to a matrix, y. 
    # '<<-' assigns a value to the object in an 
    # environment different from the current environment
    x <<- y
    
    #resets the inverse, inv, to NULL
    inv <<- NULL
  }
  
  # 2.return the matrix, x  
  get <- function() x
  
  # 3. a funtion to set the inverse, inv, to 'inverse'
  setinv <- function(inverse) inv <<- inverse
  
  # 4. return the inverse, inv
  getinv <- function() inv
  
  # returns the special 'matrix' which contains the functions defined
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## function which returns the inverse of a square matrix (x), or
## if not stored, calculates the inverse

cacheSolve <- function(x, ...) 
{
    
  inv <- x$getinv()
  
  # if there is cached data, retrieve it. i.e. inv != NULL
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculate the inverse
  mdata <- x$get()
  inv <- solve(mdata, ...)
  
  # set the value of the inverse in the cache, use setinv function
  x$setinv(inv)
  
  ## Return 
  return(inv)
}

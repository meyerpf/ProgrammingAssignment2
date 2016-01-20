## Put comments here that give an overall description of what your
## functions do


#makeCacheMatrix: this function creates a list containing "set" the matrix, "get" values within the matrix
# "set" the value of the inverse and "get" the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
       }

# cacheSolve: calculates the inverse of the matrix created with makeCacheMatrix. It first checks if it has already been calculated. 
#If so, it returns the saved value instead of calculating it again.

cacheSolve <- function(x, ...) {
 
  inv <- x$getinverse()
  
        if(!is.null(inv)) {
        message("getting cached data")
        return(inv) #return the inverse of the matrix if stored in the cache
        }
        
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv # compute and return the value of the inverse of the matrix
}

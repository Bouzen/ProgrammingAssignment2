## This function takes a matrix object and calculates the inverse. 
##If it has already calculated the inverse it pulls it from cache.


## This prepares everything

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y)
  {
    x<<- y
    m<<-NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<- inverse
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## This checks to see if we've already solved for the inverse and returms the inverse from cache.
## If there is no cached inverse for your matrix it calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()
  if(!is.null(m)) 
  { message("getting cached data.") 
    return(m)}
  data <- x$get()
  m<- solve(data)
  x$setinverse(m)
  m
}

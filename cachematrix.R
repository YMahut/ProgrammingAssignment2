makeCacheMatrix <- function( x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(mean) m <<- solve(m)
  getSolve <- function() m
  list( set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


cacheSolve <- function(x){
  m <- x$getSolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSove(m)
  m
}

##input x as a matrix
## set solve value "m" as a null
## change mean to Solve
makeCacheMatrix <- function (x = matrix()){
  m <- NULL
  set <- function(y){
    y<<- y
    m<<- NULL
  }
  get <- function(){
    x
  }
  setINV <- function(solve)
  {
    m<<-solve
  }
  getINV <- function()
  {
    m
  }
  
  list(set = set, get=get,
       setINV=setINV,
       getINV=getINV)
}

cacheSolve <- function(x, ...){
  m <- x$getINV()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setINV(m)
  m
}

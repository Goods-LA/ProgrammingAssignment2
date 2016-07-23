## MakeCacheMatrix creates a matrix that caches the inverse of x
## cacheSolve looks for an existing value of getinv, and returns the value if it exists.  If not, 
## the function inverts the matrix

## utputs the values of input and inverted matrices


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv=function(solve) m <<-solve
  getinv<-function() m
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}


## If getinv ne NULL, returns the inverted matrix with a message saying "getting cached data" 
## If getinv is NULL, returns the inverted matrix and caching the inverted matrix

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setinv(m)
  m
}

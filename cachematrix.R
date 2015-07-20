#This function is the constructor of matrix object and their functions associted
makeCacheMatrix <- function(x = matrix()) {
  #m is the variable cached inicialized with NULL
  m<-NULL
  #function that set the free variable x
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #get the free variable
  get<-function() x
  #cache the inverse matrix
  setmatrix<-function(solve) m<<- solve
  #get the inverse matrix
  getmatrix<-function() m
  list(set=set, get=get,
    setmatrix=setmatrix,
    getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  #get the inverse 
  m<-x$getmatrix()
  if(!is.null(m)){
    #if exist the inverse matrix in cache get her and return
    message("getting cached data")
    return(m)
  }
  #if doesn't exist calculate and cache her
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

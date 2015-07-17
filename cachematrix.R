#This function is the constructor of matrix object and their functions associted
makeVector <- function(x = numeric()) {
  #m is the variable cached inicialized with NULL
  m <- NULL
  #function that set the free variable x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the free variable
  get <- function() x
  #cache the mean
  setmean <- function(mean) m <<- mean
  #get the mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  #get the mean
  m <- x$getmean()  	
  if(!is.null(m)) {
    #if exist the mean in cache get her and return
    message("getting cached data")
    return(m)
  }
  #if doesn't exist calculate and cache her
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

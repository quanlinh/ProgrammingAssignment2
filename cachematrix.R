# the first function `makeCacheMatrix` create create a special matrix that can 
# cached its inverse

makeCacheMatrix <- function( x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  # get the value of the matrix
  get <- function() {
    x
  }
  # stored the value of the matrix: cached the matrix
  setmatrix <- function(matrix) {
    m <<- matrix 
  }
  # get the value of the cached matrix
  getmatrix <- function(matrix) {
    m
  }
  
# create a function list that can stored four function in function 'makeCacheMatrix`
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}




#This function `cacheSolve` will check to see if the the matrix is already cache.
#If yes, it will return the cache. If not,it will get the data from the `makeCacheMatrix
# and compute the inverse matrix then return. 

cacheSolve <- function(x, ...) {
  # get the matrix from 'makeCacheMatrix' function
  m <- x$getmatrix() 
  # check whether matrix is cache
  # if it was cache, return matrix with the message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if the matrix is not cache input the data   
  data <- x$get()
  # compute the inverse matrix of this data by giving function `solve`
  m <- solve(data, ...)
  
  # cache this matrix by call `makeCacehMatrix` function
  x$setmatrix(m)
  m
}
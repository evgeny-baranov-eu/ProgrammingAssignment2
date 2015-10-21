#####################################
## Programming assigment #2        ##
## Caching the Inverse of a Matrix ##
#####################################

## cached matrix object constructor
## x : original matrix
## returns list with getters/setters
##
## USAGE:
## x = makeCacheMatrix()
## x$set(matrix(sample(1:100, 100, replace = T), nrow = 10, ncol = 10))
## x$setSolve(solve(x$get()))
## x$get()
## x$getSolve()

makeCacheMatrix <- function(x = matrix()) {
  # init cache variable
  c <- NULL

  # create list with methods
  list(
    # original matrix setter
    set = function(y) {
      x <<- y
      c <<- NULL
    },
    
    # original matrix getter
    get = function() return(x),
    
    # solve matrix getter
    getSolve = function() return(c),
    
    # solve matrix setter
    setSolve = function (s) c <<- s
  )
}


## test workaround for makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
    message("calculate data")
    m <- solve(x$get(), ...)
    x$setSolve(m)
    return(m)
  }
}
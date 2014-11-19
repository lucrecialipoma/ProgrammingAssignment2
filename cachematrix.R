# Here we create a function call makeCacheMatrix that is list containing 4 functions to create and read an object 
# and to create and apply a function to that object. These function do not perform if I do not tell R to do it, 
# but makeCacheMatrix can keep the information in the environment for future use. 
# The other function is cacheSolve that is supposed to look in the cache the function to apply to the object created. 
# If this function was not perform previously, cacheSolve does it, if the function was already done, it just only return 
# the answer.

# makeCacheMatrix is a function that contains other 4 functions that are keep in the environment or cache for future use.

makeCacheMatrix <- function (x = matrix()){
                   I <- NULL
                   set <- function (y){
                        x <<- y
                        I <<- NULL
                        }
                   get <- function() {x}
                   setinv <- function (solve) {
                          I<<-solve
                           }
                   getinv <- function() {I}
                   list(set=set, get=get, setinv=setinv, getinv=getinv)
    }

# cacheSolve is a function to access the object created with the makeCacheMatrix function and calculate the inverse 
# of the matrix if it was not calculated yet. Instead, if the inverse of the matrix was already perform, it just 
# shows the result and don't calculated it again.

CacheSolve <- function(x,...){
              I <- x$getinv()
              if(!is.null(I)) {
                  message("getting cached data")
                  return(I)
                  }
             data <- x$get()
             I <- solve(data,...)
             x$setinv(I)
             I
    }

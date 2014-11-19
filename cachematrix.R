
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

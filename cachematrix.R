##The assignment is regarding lexical scoping and caching functions that mostly require a long computation time. 
##Here I am using "solve()" to find the inverse of a matrix as also described in the example of the course and cache it using a free floating variable.
## Below , For each function the code is described. And at the end a suitable use with example is also given.

## This particular function,"makeCacheMatrix" creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
  
}


## The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. 
## If yes, then the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
  
}

## To try this code use the following procedure in your 'R' running platform.
## 1.Copy the both function and save it
## 2. use "a<-makeCacheMatrix()"
## 3. use "a$set(matrix(1:4,2,2))"
## 4. use "cacheSolve(a)"
## 5. it will return the matrix      [,1] [,2] 
##                             [1,]   -2  1.5
##                             [2,]    1 -0.5
## which is inverse of 
##                               [,1] [,2]
##                         [1,]    1    3
##                         [2,]    2    4
##******To check the matrix calculation manually , do inverse of matrix manually which is
## transpose the adjoint of the actual matrix*****

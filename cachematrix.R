## create an object associated with the given matrix x,
## which has four function:
## 1. get the value of x
## 2. set the value of x
## 3. get the inverse of x
## 4. set the inverse of x
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  getinv<-function() inv
  get<-function() x
  set<-function(newmat) {
    x<<-newmat
    inv<<-NULL
  }
  setinv<-function(invmat) {
    inv<<-invmat
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Get/compute the inverse of matrix associated with x created by makeCacheMatrix,
## use cached value if matrix is not changed since last computation of inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat)
  x$setinv(inv)
  inv
}

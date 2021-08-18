#makea cache matrix function as implemented in instruction
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
 
  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
}
 
 
#making the Vector creates a special vector by using cachesolve function
cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)) {
    message("retrieving matrix")
    return(inv)
  }
 
  ans<-x$get()
  inv<-solve(ans,...)
  x$setInv(inv)
  inv
 
}
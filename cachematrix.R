## The following functions store a matrix and cache its inverse, so that if the same
##matrix is called twice, it gets its inverse from the cache and does not calculate it twice.


#Create a list which sets the matrix, get the matrix, sets the inverse of the matrix,
##gets the inverse of the matrix.  

makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  set_invmat<-function(inversemat) m<<-inversemat
  get_invmat<-function() m
  list(set=set,get=get,set_invmat=set_invmat,get_invmat=get_invmat)
}


## calculates the inverse of the matrix in the list returned by the above function;
##it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache. Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache via the set_invmat function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$get_invmat()
  if(!is.null(m)){
    message("getting from the cache")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$set_invmat(m)
  return(m)
}


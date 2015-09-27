## This function takes a matrix and computes its inverse trough the use of solve() function

## The first function, makeCacheMatrix creates a special "matrix", which is really
## a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the matrix's inverse
## 4.get the value of the matrix's inverse

makeCacheMatrix <- function(a=c(1,0,0,1),b=2,c=2) {
        x<-matrix(a,b,c)
        inv<-NULL
        set<-function(d,e,f){
                y<-matrix(d,e,f)
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the inverse of the special "matrix" created
## with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv<-x$getinverse()
          if(!is.null(inv)){
                  message("getting cache data")
                  return(inv)
          }
          data<-x$get()
          inv<-solve(data)
          x$setinverse(inv)
          inv
}
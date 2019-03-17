##Caching the Inverse of a matrix

## makeCacheMatrix function creates a special "matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL #make empty object m
    get<-function() x #get retrieves value of x, the input matrix
    setinv<-function(inv) m<<-inv #stores matrix inversion to variable m
    getinv<-function() m #calls value of inverse of matrix
    list(get=get, setinv=setinv, getinv=getinv)
}


##cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. If the inverse has already been calculated, and the matrix has not changed, then cacheSolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
    m<-x$getinv() #calls value of inversion of matrix
    if(!is.null(m)){ #tests whether matrix inversion was performed, and returns inversion of matrix
        message("getting cached data")
        return(m)
    }
    data<-x$get() #data set to input matrix
    m<-solve(data, ...) #performs inversion of input matrix
    x$setinv(m) #stores inversion of matrix into m and prints m
    m
}

## makeCacheMatrix creates list of functions to set the value of matrix, 
## get the value of matrix, set the inverse of the matrix and get the inverse of
## the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    
    set<- function(y)	{
        x<<-y
        m<<-NULL
    }
    
    get<- function()
        x
    
    setinverse<-function(solve)
        m<<-solve
    
    getinverse<-function()
        m
    
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}

## cacheSolve calculates inverse of the matrix the first time and caches it 
## so that next time the inverse is called, it retrieves inverse from cache

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}

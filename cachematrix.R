## This is an example of lexical scoping. These two functions in conjunction, create an
## R object containing a matrix and its inverse and functions to manipulate and retrieve them.
## The purpose of these two functions is to 
## be able to use the cache, so expensive calcultions, like solving a matrix have to be only done once.



## makeCacheMatrix creates an object that contains a complete copy of the environment of makeCacheMatrix,
##containing a matrix and its inverse and getters and setters for them

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL                 ##initiaized for further use
        set<-function(y){      
                x<<-y           ## y assinged to x in parent environment
                m<<-NULL        ## m cleared in cache
        }
        get<-function()x
        setsolve<-function(solve) m<<-solve  ## m placed in cache parent environment.
        getsolve<-function() m
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)  ## assign getters and setters and return list object
}


##  cacheSolve assigns or retrieves the value of the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()   ##set data to x
        m<-solve(data)  ## recalculate m
        x$setsolve(m)   ## recalculate m and place in cache parent environment
        m
}

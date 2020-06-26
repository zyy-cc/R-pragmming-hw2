 #This function is used to set the set(),get(),setinverse(),getinverse() function
makeCacheMatrix<-function(x=matrix()){
        # set the initial value
        m<-NULL
        # set x using y, m reset null
        set<-function(y){
           x<<- y
           m<<- NULL
        }
        # return x
        get<- function(){
                x
        }
        
       setinverse<-function(solve){
                 m<<-solve
       }
       #return y
        getinverse<-function(){
                m
        }
        #return to a list
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
            }

# This function is used to calculate the inverse of the matrix, assuming all the matrix are invertible.
 cacheSolve<-function(x,...){
         m <- x$getinverse()
         if(!is.null(m)){
                 message("getting cached data")
                 return(m)
             }
        data <- x$get()
         m <- solve(data,...)
         x$setinverse(m)
        m
         
}
# test
 na <-matrix(3:6,2,2)
 myinve<-makeCacheMatrix(na)
 cacheSolve(myinve)


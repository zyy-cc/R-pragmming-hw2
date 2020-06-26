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
        
       setinverse<-function(inverse){
                 m<<-inverse
       }
       #return y
        getinverse<-function(){
                m
        }
        #return to a list
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
            }
 cacheSolve<-function(x,...){
         m <- x$getinverse()
         if(!is.null(m)){
                 message("getting cached data")
                 return(m)
             }
        data <- x$get()
         m <- inverse(data,...)
         x$setinverse(m)
        m
         
}

A=matrix(c(2,0,5,-2,1,3,1,2,1),nrow=3,ncol=3)

B = matrix(c(2, 4, 3, 1, 5, 7),nrow=3,ncol=2)
C=t(B)
B
C
Z<-cbind(C,c(6,4))
Z
#a<-rep.int(0,2)
#a
#b<-rep.int(1,3)
#b
#c<-append(a,b)
#c                     gonna need this example for m<n

LU<-function(M){
  dm<-dim(M)
  m<-dm[1]
  n<-dm[2]
  if(m>=n){
    l<-diag(rep(1,m))

    for(i in 1:(n-1)){     # i is for columns(every time we change guide element)
        for(j in (i+1):m){
          m_ij<-M[j,i]/M[i,i] # M[i,i] guide element
          M[j,]<-M[j,]-m_ij*M[i,] # zeroing j,i element(j row - mult*main row(has the guide))
          l[j,i]<- m_ij           # filling l matrix
      }
    }
  }
  else{
    l<-diag(rep(1,m))
    l.new<-l
    for(i in 1:(n-1)){
      if( i>=m){         # case m<n so if T we have to resize matrix
        m=m+1
        a<-rep.int(0,(i-1))
        b<-rep.int(1,(n-i)+1)
        c<-append(a,b)
        M<-rbind(M,c)     # the matrix plus one row,full of 0 before guide element
        l.new<-l          # full of 1 in and after guide element,also resizing L because
        l<-diag(rep(1,m)) # has dimensions mxm
        l[1:(m-1),1:(m-1)]<-l.new[1:(m-1),1:(m-1)] # filling new L with old L's
                                                   #values  
      }
      for(j in (i+1):m){
        
        
        m_ij<-M[j,i]/M[i,i]
        M[j,]<-M[j,]-m_ij*M[i,]
        l[j,i]<- m_ij
      }
    }
    }
  return(list("U"=M,"L"=l))
}

LU(A)
LU(B)
LU(C)
LU(Z)

LU(Z)$L%*%LU(Z)$U
# i want to create a function to remove filthy rows
# from LU if m<n, for m>n we have precise results
TestRes<- function(p){
  MatrixRec<- function(x) LU(x)$L%*%LU(x)$U
  dms<-dim(p)
  nr<-dms[1]
  nc<-dms[2]
  
  if(nr>=nc){        # if #rows>=#cols ola popa
    return(MatrixRec(p))
  }
  else{
    
    matrix.filthy<-MatrixRec(p)
    rdms<-dim(matrix.filthy)
    rnr<-rdms[1]   # dimensions are the same but we will need them both
    rnc<-rdms[2]
    
    #function whithin a function
    ReduceRow<-function(m,r,c){
      for(i in r:1){           
        # want to begin from the end of the matrix
        count=0
        for(j in 1:rnc){
          if(m[i,j]==1 | m[i,j]==0){
            count=count+1
            if(j==rnc){    # check if we are at the end of the row
              if(count==rnc){
                m<- m[-i,]
              }
            }
          }
          
        }
        
      }
      return(m)
    }
    
    return(ReduceRow(matrix.filthy,rnr,rnc))
    
  }
}




       # init count to see if a row is full of 0 and 1
print("Case m==n")
print("Reconstruction:")
res<-TestRes(A)
res
print("Origin Matrix:")
A
print("Case m>n")
print("Reconstruction:")
res<-TestRes(B)
res
print("Origin Matrix:")
B
print("Case m<n")
print("Reconstruction:")
res<-TestRes(C)
res
print("Origin Matrix:")
C


print("Case m<n")
print("Reconstruction:")
res<-TestRes(Z)
res
print("Origin Matrix:")
Z



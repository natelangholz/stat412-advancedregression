

#function example

h=function(x){(cos(50*x)+sin(20*x))^2}
curve(h,xlab="Function",ylab="",lwd=3,col="maroon")
x=runif(1)
hval=hcur=h(x)
temp=1/log(1+1:10000)
#temp=1/log(1+1:10000)^2
#temp = seq(2,.1,by=-1/10000)
diff=iter=1
while(diff>10^(-4)){
  prop=x[iter]+runif(1,-1,1)
  if((prop>1) || (prop <0) ||(log(runif(1))*temp[iter]>h(prop)-hcur))
    prop=x[iter]
  x=c(x,prop)
  hcur=h(prop)
  hval=c(hval,hcur)
  if ((iter>1000)&&(length(unique(x[(iter/2):iter]))>1))
    diff=max(hval)-max(hval[1:(iter/2)])
  iter=iter+1
  points(x,hval,type="l")
}





#Sudoku example

#easily solved
s=matrix(0,ncol=9,nrow=9)
s[1,c(1,2,5)]=c(5,3,7)
s[2,c(1,4:6)]=c(6,1,9,5)
s[3,c(2,3,8)]=c(9,8,6)
s[4,c(1,5,9)]=c(8,6,3)
s[5,c(1,4,6,9)]=c(4,8,3,1)
s[6,c(1,5,9)]=c(7,2,6)
s[7,c(2,7,8)]=c(6,2,8)
s[8,c(4:6,9)]=c(4,1,9,5)
s[9,c(5,8,9)]=c(8,7,9)

#medium difficulty
# s=matrix(0,ncol=9,nrow=9)
# s[1,c(5,7)]=c(7,1)
# s[2,c(1,2,5)]=c(7,9,4)
# s[3,c(1,3,4)]=c(2,4,3)
# s[4,c(3,6,8)]=c(7,8,5)
# s[5,c(1,5,9)]=c(8,9,4)
# s[6,c(2,4,7)]=c(1,7,8)
# s[7,c(6,7,9)]=c(1,5,8)
# s[8,c(5,8,9)]=c(8,2,1)
# s[9,c(3,5)]=c(6,3)


temp<-0.5

#temp<-2

#set.seed(47)
set.seed(21)
cur<-matrix(0,9,9)
sample(1:9)
for(i in 0:2){
  for(j in 0:2){
    sq<-s[(1:3)+3*i,(1:3)+3*j]
    x<-c(1:9)
    sq1<-as.vector(sq)
    sq1[sq1==0]<-sample(x[-c(sq1)],length(x[-c(sq1)]))
    sq<-matrix(sq1,nrow=3)
    cur[(1:3)+3*i,(1:3)+3*j]<-sq
  }
}

scor<-function(matr){
  scr=-1*(sum(apply(matr,1,function(x) length(unique(x))))+sum(apply(matr,2,function(x) length(unique(x)))));return(scr)}




for(i in 1:1000000){
  randbox<-sample(0:2,2,replace=T)
  rbox1<-(1:3)+3*randbox[1]
  rbox2<-(1:3)+3*randbox[2]
  propos<-cur[rbox1,rbox2]
  flips<-sample(which(s[rbox1,rbox2]==0),2)
  tmp<-propos[flips[1]]
  propos[flips[1]]<-propos[flips[2]]
  propos[flips[2]]<-tmp
  proposal<-cur
  proposal[rbox1,rbox2]<-propos
  
  
  y_n<-scor(proposal)
  y<-scor(cur)
  
  
  if(exp((y-y_n)/temp)- runif(1) > 0){cur<-proposal}else{cur<-cur}
  if(i%%1000==0){print('iteration');print(i);print('score');print(y)}
  if(scor(cur)==-162){print('puzzle solved');print(cur);print(i);break}
  temp<-.99999*temp}


  
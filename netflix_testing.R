combined_data_1 <- read.csv2("C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/combined_data_1.txt", header=FALSE, sep="")

library(stringr)
library(lubridate)
MovieDat<-matrix(NA,nrow = 0,ncol = 3)
dat<-combined_data_1$V1
for(i in 1:length(dat)){
  if(str_detect(dat[i],":")){
    curmovie<-as.numeric(substr(dat[i],1,str_length(dat[i])-1))
    print(i)
  } else{
    cur<-str_split(dat[i],",")
    if(year(as.Date(cur[[1]][3]))==2000){
      vec<-c(curmovie, as.numeric(cur[[1]][1]), as.numeric(cur[[1]][2]))
      MovieDat<-rbind(MovieDat,vec)
    }
  }
}


combined_data_1 <- read.csv2("C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/combined_data_2.txt", header=FALSE, sep="")

dat<-combined_data_1$V1
for(i in 1:length(dat)){
  if(str_detect(dat[i],":")){
    curmovie<-as.numeric(substr(dat[i],1,str_length(dat[i])-1))
    print(i)
  } else{
    cur<-str_split(dat[i],",")
    if(year(as.Date(cur[[1]][3]))==2000){
      vec<-c(curmovie, as.numeric(cur[[1]][1]), as.numeric(cur[[1]][2]))
      MovieDat<-rbind(MovieDat,vec)
    }
  }
}


combined_data_1 <- read.csv2("C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/combined_data_3.txt", header=FALSE, sep="")

dat<-combined_data_1$V1
for(i in 1:length(dat)){
  if(str_detect(dat[i],":")){
    curmovie<-as.numeric(substr(dat[i],1,str_length(dat[i])-1))
    print(i)
  } else{
    cur<-str_split(dat[i],",")
    if(year(as.Date(cur[[1]][3]))==2000){
      vec<-c(curmovie, as.numeric(cur[[1]][1]), as.numeric(cur[[1]][2]))
      MovieDat<-rbind(MovieDat,vec)
    }
  }
}



combined_data_1 <- read.csv2("C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/combined_data_4.txt", header=FALSE, sep="")

dat<-combined_data_1$V1
for(i in 1:length(dat)){
  if(str_detect(dat[i],":")){
    curmovie<-as.numeric(substr(dat[i],1,str_length(dat[i])-1))
    print(i)
  } else{
    cur<-str_split(dat[i],",")
    if(year(as.Date(cur[[1]][3]))==2000){
      vec<-c(curmovie, as.numeric(cur[[1]][1]), as.numeric(cur[[1]][2]))
      MovieDat<-rbind(MovieDat,vec)
    }
  }
}

MovieDatFinal<-MovieDat
MovieDatFinal<-as.data.frame(MovieDatFinal)
colnames(MovieDatFinal)<-c("MovieID","UserID","Rating")


netflix_genres <- read.csv("C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/netflix_genres.csv")
MovieDatFinal<-merge(MovieDatFinal,netflix_genres,by.x="MovieID",by.y="movieId")
MovieDatFinal$Action<-str_detect(MovieDatFinal$genres,"Action")
MovieDatFinal$Adventure<-str_detect(MovieDatFinal$genres,"Adventure")
MovieDatFinal$Animation<-str_detect(MovieDatFinal$genres,"Animation")
MovieDatFinal$Biography<-str_detect(MovieDatFinal$genres,"Biography")
MovieDatFinal$Comedy<-str_detect(MovieDatFinal$genres,"Comedy")
MovieDatFinal$Crime<-str_detect(MovieDatFinal$genres,"Crime")
MovieDatFinal$Documentary<-str_detect(MovieDatFinal$genres,"Documentary")
MovieDatFinal$Drama<-str_detect(MovieDatFinal$genres,"Drama")
MovieDatFinal$Family<-str_detect(MovieDatFinal$genres,"Family")
MovieDatFinal$Fantasy<-str_detect(MovieDatFinal$genres,"Fantasy")
MovieDatFinal$History<-str_detect(MovieDatFinal$genres,"History")
MovieDatFinal$Horror<-str_detect(MovieDatFinal$genres,"Horror")
MovieDatFinal$Music<-str_detect(MovieDatFinal$genres,"Music")
MovieDatFinal$Musical<-str_detect(MovieDatFinal$genres,"Musical")
MovieDatFinal$Mystery<-str_detect(MovieDatFinal$genres,"Mystery")
MovieDatFinal$RealityTV<-str_detect(MovieDatFinal$genres,"Reality-TV")
MovieDatFinal$Romance<-str_detect(MovieDatFinal$genres,"Romance")
MovieDatFinal$Sport<-str_detect(MovieDatFinal$genres,"Sport")
MovieDatFinal$SciFi<-str_detect(MovieDatFinal$genres,"Sci-Fi")
MovieDatFinal$TalkShow<-str_detect(MovieDatFinal$genres,"Talk-Show")
MovieDatFinal$Thriller<-str_detect(MovieDatFinal$genres,"Thriller")
MovieDatFinal$War<-str_detect(MovieDatFinal$genres,"War")
MovieDatFinal$Western<-str_detect(MovieDatFinal$genres,"Western")

MovieDatFinal$genres<-NULL

write.csv(MovieDatFinal,"C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/MovieDat.csv",row.names = F)


#test speed
library(tidyr)
library(Matrix)
test<-rnorm(100000)
t<-Sys.time()
for(i in 1:10){
  #power<-powerMethod(MgradF,1e-16)
}
Sys.time()-t



powerMethod<-function(A,epsilon){
  v0<-rep(0,ncol(A))
  v0[1]<-1
  Asparse<-as(A, "dgCMatrix")
  ATA<-crossprod(Asparse)
  
  v<-ATA %*% v0
  v<-v/as.numeric(sqrt(crossprod(v)))
  while(max(v-v0)>epsilon){
    v0<-v
    v<-ATA %*% v0
    v<-v/as.numeric(sqrt(crossprod(v)))
    #print(sig)
  }
  sig=as.numeric(sqrt(crossprod(A%*%v)))
  return(list(v=as.numeric(v),u=as.numeric(A%*%v/sig),sig=sig))
}


#start matrix completion
movie_titles <- read.csv("C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/movie_titles.csv", header=FALSE)
MovieDat <- read.csv("C:/Users/joeja/Desktop/CPSC/CPSC536M/Project/MovieDat.csv")
B=spread(MovieDat,key="UserID",value = "Rating")

assign(paste0(colnames(B)[1],"Vec"),as.numeric(B[,1]))
Genres<-B[,2:24]
B[,1:24]<-NULL
movie_titles<-movie_titles[as.numeric(movie_titles$V1) %in% MovieIDVec,]
B<-t(B)

#get optimal tau
# tauvec<-c(20000,30000,40000,50000,60000)
# maxiter<-50
# errorMat<-matrix(0,nrow = maxiter,ncol = length(tauvec))
# for(tau in 1:length(tauvec)){
#   bigU<-rep(1,nrow(B))/norm(rep(1,nrow(B)),type = "2")
#   bigV<-rep(1,ncol(B))/norm(rep(1,ncol(B)),type = "2")
#   sigvec<-mean(as.numeric(B),na.rm=T)/(bigU[1]*bigV[1])
#   X=bigU%*%diag(sigvec,nrow = length(sigvec))%*%t(bigV)
#   errorvec<-c()
#   for(i in 1:maxiter){
#     MgradF<-B-X
#     MgradF[is.na(MgradF)]<-0
#     MgradF<-data.matrix(MgradF)
#     
#     #gamma<-2/(2+i)
#     power<-powerMethod(MgradF,1e-16)
#     bigU<-cbind(bigU,power$u)
#     bigV<-cbind(bigV,power$v)
#     s=tauvec[tau]*power$u%*%t(power$v)
#     gamma<-min(sum(as.numeric(X-B)*as.numeric(X-s),na.rm = T)/sum((as.numeric(s-X)[!is.na(as.numeric(B))])^2),1)
#     sigvec<-c((1-gamma)*sigvec,gamma*tauvec[tau])
#     X=bigU%*%diag(sigvec,nrow = length(sigvec))%*%t(bigV)
#     print(sum(sigvec))
#     errorvec<-c(errorvec,norm(MgradF,"F"))
#   }
#   errorMat[,tau]<-errorvec
# }
# 
# matplot(errorMat,type = "l",xlab = "iterations",ylab = "error (Frobenius norm)",cex.lab=1.3)
# legend("topright",c("tau=20000","tau=30000","tau=40000","tau=50000","tau=60000"),col = seq_len(5),cex=0.8,fill=seq_len(5))


##choose tau=50000
tau<-50000
maxiter<-100
bigU<-rep(1,nrow(B))/norm(rep(1,nrow(B)),type = "2")
bigV<-rep(1,ncol(B))/norm(rep(1,ncol(B)),type = "2")
sigvec<-mean(as.numeric(B),na.rm=T)/(bigU[1]*bigV[1])
X=bigU%*%diag(sigvec,nrow = length(sigvec))%*%t(bigV)

# bigU<-c()
# bigV<-c()
# sigvec<-c()
# X=0

# bigU<-as.numeric(rowMeans(B,na.rm = T))
# bigV<-as.numeric(colMeans(B,na.rm = T))
# X=bigU%*%t(bigV)
# meanX=mean(as.numeric(X))
# meanB=mean(as.numeric(B),na.rm=T)
# normV<-norm(bigV,type = "2")
# normU<-norm(bigU,type = "2")
# bigU<-bigU/normU
# bigV<-bigV/normV
# sigvec<-(normV*normU*meanB/meanX)
# X=bigU%*%t(bigV)*sigvec


errorvec<-c()
for(i in 1:maxiter){
  MgradF<-B-X
  MgradF[is.na(MgradF)]<-0
  MgradF<-data.matrix(MgradF)
  
  #gamma<-2/(2+i)
  power<-powerMethod(MgradF,1e-10)
  bigU<-cbind(bigU,power$u)
  bigV<-cbind(bigV,power$v)
  NAloc<-!is.na(as.numeric(B))
  sX=as.numeric(tau*tcrossprod(power$u,power$v)-X)[NAloc]
  gamma<-as.numeric(crossprod(as.numeric(MgradF)[NAloc],sX)/crossprod(sX))
  if(gamma>1) gamma<-1
  if(gamma<0) gamma<-0
  sigvec<-c((1-gamma)*sigvec,gamma*tau)
  sigMat<-Diagonal(x=sigvec)
  # if(i %% 20==0){
  #   bigU=bigU[,order(- sigvec)][,1:(length(sigvec)-10)]
  #   bigV=bigV[,order(- sigvec)][,1:(length(sigvec)-10)]
  #   sigMat<-Diagonal(x=sigvec[order(- sigvec)])[1:(length(sigvec)-10),1:(length(sigvec)-10)]
  #   sigvec<-sigvec[order(-sigvec)][1:(length(sigvec)-10)]
  # }
  X=tcrossprod(bigU%*%sigMat,bigV)
  errorvec<-c(errorvec,norm(MgradF,"F"))
  print(i)
}

## correlations in bigV and bigU
library(gplots)
library("RColorBrewer")
heatmap.2(cor(bigU)[2:100,2:100],Colv = NA,Rowv = NA,scale = "none",density.info="none", trace="none",col=brewer.pal(11,"RdBu"))
heatmap.2(cor(bigV)[2:100,2:100],Colv = NA,Rowv = NA,scale = "none",density.info="none", trace="none",col=brewer.pal(11,"RdBu"))


ErrorInitialGuessMean<-errorvec[1:100]
ErrorInitialGuess0<-errorvec
ErrorInitialGuessBest<-errorvec

matplot(cbind(ErrorInitialGuess0,ErrorInitialGuessMean,ErrorInitialGuessBest),type = "l",ylim = c(600,1500),xlab = "iteration",ylab = "error",cex.lab=1.4,lwd = 3)
legend("topright",c("Initial Guess = 0","Initial Guess = mean","Initial Guess = col/row means"),col = seq_len(3),cex=0.8,fill=seq_len(3))

# svdX<-svd(X)
# InterpMat<-t(Genres)%*%svdX$v[,1:20]
# InterpMat<-t(t(InterpMat)/apply(InterpMat, 2, sd))
# InterpMat<-InterpMat/as.numeric(colSums(Genres))
# library(RColorBrewer)
# coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
# heatmap(InterpMat,col=coul,Colv = NA,Rowv = NA,breaks=c(seq(from=min(as.numeric(InterpMat)),to=0,length.out = (length(coul)+1)/2),seq(from=0,to=max(as.numeric(InterpMat)),length.out = (length(coul)+1)/2)),scale = "none")

InterpMat<-t(Genres)%*%bigV[,1:20]
InterpMat<-t(t(InterpMat)/apply(InterpMat, 2, sd))
InterpMat<-InterpMat/as.numeric(colSums(Genres))
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(InterpMat,col=coul,Colv = NA,Rowv = NA,breaks=c(seq(from=min(as.numeric(InterpMat)),to=0,length.out = (length(coul)+1)/2),seq(from=0,to=max(as.numeric(InterpMat)),length.out = (length(coul)+1)/2)),scale = "none")


X1=svdX$u[,1:20]%*%diag(svdX$d[1:20],20)%*%t(svdX$v[,1:20])
test=B-X1








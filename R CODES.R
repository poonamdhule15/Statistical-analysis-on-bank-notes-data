####PCA
#WILL Carry out Principal Component Analysis (PCA) on the basis of covariance matrix.

D=read.table(file.choose(),header=TRUE)
n1= names(D)
n1
d=dim(D)
d

 n=dim(D)[1]
n

 p=dim(D)[2]
p
 M=apply(D,2,mean)
M
 D=as.matrix(D)
 S=(t(D)%*%D)-(n*M%*%t(M))
 V=S/n
V
 eval=eigen(V)$values
evec=eigen(V)$vectors
eval
evec

c=cor(D)
c
summary(c)
#Q1b
#(b) Draw the scree plot.

 plot(1:p,eval,type="b")
#As it can be seen from the scree plot, three PCs should be appropriate.

 #Q1c
#(c) How many PCs are required to explain 90% variation?

 prop=cumsum(eval)/sum(eval)
 prop
 #Just as in the case of scree plot, we can see that three PCs will be able to explain 90% of the variation in the data.
 #Q1d
#(d) Draw the score plot for the first two PCs and mark the observations of the two

#groups (genuine and forged) by two different colors. Are the two groups clearly
#distinguishable?

 score=D%*%evec
 plot(score[,1],score[,2],col=rep(c("red","blue"),each=100),pch=16)

#(e) On the basis of first two PCs, which variables are important according to you?
#Q1e
evec
 
#On the basis of the first PC, the Lower Margin variable is the most important.
 #On the basis of the second PC, the Upper Margin variable is the most important.




dir="/home/juliette/Documents/Philippe_Marco/cleaned_codes"
setwd(dir)
library(glmnet)
library(randomForest)
library(rpart)
library(e1071) 


library(devtools)
install("/home/juliette/Documents/Philippe_Marco/package/ExtremesBinaryClassifier",
        force=TRUE) #install package
library(ExtremesBinaryClassifier)

##################################
##################################

par(mfrow=c(1,1),mar=c(4.3,5.1,2.1,2.1))

#########################
set.seed(123)
nsim <- 1e4
alpha1 = 3
alpha2 = 2
sigma = 1
X1 = sigma*1/(runif(nsim)^(1/alpha1))
P = 1/(runif(nsim)^(1/alpha2))
H = X1+P

X2 = 1/(runif(nsim)^(1/alpha2))
X3 = rexp(nsim,1)
X4 = rexp(nsim,2)
u = quantile(H,probs=0.97) #0.95
sum(H>u)
eps = 0.7 #0.5
eps_u = u*eps

# par(mfrow=c(2,2))
# plot(X1,H,pch=20,col='grey',
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X1",#expression(bar(g)(X)),
#      ylab="H",
#      ylim=range(c(H,X1)),xlim=range(X2))
# points(X1[H>eps_u & X1>eps_u],
#        H[H>eps_u & X1>eps_u],
#        pch=20,cex=2)
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# plot(X2,H,pch=20,col='grey',
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X2", ylab="H",
#      ylim=range(c(H,X1)),xlim=range(X2))
# points(X2[H>eps_u & X2>eps_u],
#        H[H>eps_u & X2>eps_u],
#        pch=20,cex=2)
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# plot(X3,H,pch=20,col='grey',
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X3", ylab="H",
#      ylim=range(c(H,X1)),xlim=range(X2))
# points(X3[H>eps_u & X3>eps_u],
#        H[H>eps_u & X3>eps_u],
#        pch=20,cex=2)
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# plot(X4,H,pch=20,col='grey',
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X4", ylab="H",
#      ylim=range(c(H,X1)),xlim=range(X2))
# points(X4[H>eps_u & X4>eps_u],
#        H[H>eps_u & X4>eps_u],
#        pch=20,cex=2)
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# 
# # ZOOM
# par(mfrow=c(2,2))
# plot(X1[H>eps_u & X1>eps_u],
#      H[H>eps_u & X1>eps_u],
#      pch=20,
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X1",ylab="H",
#      ylim=c(eps_u,28),#range(H[H>eps_u & X1>eps_u]),
#      xlim=c(eps_u,28))#range(X1[H>eps_u & X1>eps_u]))
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# plot(X2[H>eps_u & X2>eps_u],
#      H[H>eps_u & X2>eps_u],
#      pch=20,
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X2",ylab="H",
#      ylim=c(eps_u,28),#range(H[H>eps_u & X2>eps_u]),
#      xlim=c(eps_u,28))#range(X2[H>eps_u & X2>eps_u]))
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# plot(X3[H>eps_u & X3>eps_u],
#      H[H>eps_u & X3>eps_u],
#      pch=20,
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X3",ylab="H",
#      ylim=c(eps_u,28),#range(H[H>eps_u & X1>eps_u]),
#      xlim=c(eps_u,28))#range(X1[H>eps_u & X1>eps_u]))
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# plot(X4[H>eps_u & X4>eps_u],
#      H[H>eps_u & X4>eps_u],
#      pch=20,
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,xlab="X4",ylab="H",
#      ylim=c(eps_u,28),#range(H[H>eps_u & X1>eps_u]),
#      xlim=c(eps_u,28))#range(X1[H>eps_u & X1>eps_u]))
# abline(h=u,col='blue',lwd=2,lty='dotted')
# abline(v=u,col='blue',lwd=2,lty='dotted')
# 
# est.c = rep(NA,4)
# X.big = cbind(X1,X2,X3,X4)
# for (i in 1:4){
#   est.c[i] <- sum(X.big[,i]>u) / sum(X.big[,i]>eps_u) 
# }
# est.c
# true.c = c(eps^c(alpha1,alpha2),0,0)
# 
# N <- c(1000,5000,10000,20000,1e5)
# probs <- c(0.7,0.94,0.97,0.985,0.997)
# #c(0.90,0.98,0.99,0.995,0.999)#
# no.rep <- 1000
# nvar = 4
# mean.est <- matrix(NA,ncol=nvar,nrow=length(N))
# cov.perc <- matrix(NA,ncol=nvar,nrow=length(N))
# nx <- matrix(NA,ncol=no.rep,nrow=length(N))
# for (k in 1:length(N)){
#   n <- N[k]
#   est.c <- matrix(NA, nrow=no.rep, ncol=nvar)
#   var.c <- matrix(NA, nrow=no.rep, ncol=nvar)
#   est.c.inf <- matrix(NA, nrow=no.rep, ncol=nvar)
#   est.c.sup <- matrix(NA, nrow=no.rep, ncol=nvar)
#   for (l in 1:no.rep){
#     
#     # simulation data
#     X.big <- cbind(1/(runif(n)^(1/alpha1)),
#                    1/(runif(n)^(1/alpha2)),
#                    rexp(n),rexp(n,rate=2)) 
#     # X.big <- cbind(X.big,X.big[,2]+X.big[,5])
#     H2 <- X.big[,1] + 1/(runif(n)^(1/alpha2))
#     thresh2 <- quantile(p=probs[k], x=H2, na.rm=T)
#     nx[k,] <- sum(H2>thresh2)
#     eps2 = thresh2*eps
#     
#     # estimation of ci 
#     for (i in 1:nvar){
#       if(sum(X.big[,i]>eps2)==0){
#         est.c[l,i]=0
#         var.c[l,i]=0}
#       else{
#         est.c[l,i] <- sum(X.big[,i]>thresh2) / sum(X.big[,i]>eps2) 
#         var.c[l,i] <- est.c[l,i]*(1-est.c[l,i])/sum(X.big[,i]>eps2)
#       }
#       est.c.inf[l,i] <- est.c[l,i] - 
#         1.96 * sqrt(var.c[l,i])
#       est.c.sup[l,i] <- est.c[l,i] + 
#         1.96 * sqrt(var.c[l,i]) 
#     }
#   }
#   for (j in 1:nvar){
#     cov.perc[k,j] <- 
#       sum((est.c.sup[,j] >= true.c[j]) & 
#             (est.c.inf[,j] <= true.c[j])) / 
#       no.rep
#   }
#   mean.est[k,] <- apply(est.c,MARGIN=2,FUN=mean,na.rm=T)
# }
# 
# print(mean.est) # estimated ci for each sample size
# print(apply(nx,MARGIN=1,FUN=mean)) # number of exceedances
# print(cov.perc) #coverage percentages


X.all = cbind(X1,X2)
B = 100 # number of samples for the cross validation

R.lin = R.lasso = R.rf =R.tree= R.svml = 
  R.lin.eps = R.lasso.eps = R.rf.eps =
  R.tree.eps = R.svml.eps = vector(mode = "list", length = B)

theta.all = theta.all.eps = matrix(NA,nrow=B,ncol=2)

reslin0=lm(H~X1+X2,
           data=data.frame(H=H,X1=X1, X2=X2))
reslm0 = abs(reslin0$coefficients[2:3])

reslin=lm(H~X1+X2,
          data=data.frame(H=H[H>eps_u&X1>eps_u],
                          X1=X1[H>eps_u&X1>eps_u],
                          X2=X2[H>eps_u&X1>eps_u]))
reslm = abs(reslin$coefficients[2:3])

for (b in 1:B){
  ##### Split data training-validation sets #####
  ind.train <- sample.int(length(H),
                          size = 0.7*length(H),
                          replace=F)
  
  X_train <- X.all[ind.train,] 
  X_test <- X.all[-ind.train,] 
  
  H_train <- H[ind.train]
  H_test <- H[-ind.train]
  Y_train <- 2*(H_train > u) - 1 
  Y_test <- 2*(H_test > u) - 1 
  
  Y_train.eps <- 2*(H_train > eps_u) - 1 
  Y_test.eps <- 2*(H_test > eps_u) - 1 
  
  ##### Linear classifier with mass ######
  theta = ExtremesBinaryClassifier:::LinearClassifier(X = X_train, thresh = u, H = H_train, 
                                                      initials = reslm0)$theta
  theta.all[b,] = theta
  glin = 2*(as.vector(theta %*% t(X_test)) > u) -1
  
  R.lin[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, g = glin, epsilon = 0)
  
  ##### Linear classifier without mass ######
  theta.eps = ExtremesBinaryClassifier:::LinearClassifier(X = X_train, thresh = eps_u, H = H_train, 
                                                          initials = reslm)$theta
  theta.all.eps[b,] = theta.eps
  glin = 2*(as.vector(theta.eps %*% t(X_test)) > u) -1
  glineps <- 2*(as.vector(theta.eps %*% t(X_test)) > eps_u) - 1
  
  R.lin.eps[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, Y.eps = Y_test.eps, 
                                                            g = glin, g.eps = glineps,
                                                            epsilon = eps)
  ##### Regression trees with mass ######
  treeclass <- rpart(y~., data=data.frame(x=X_train, y = as.factor(Y_train)), method = "class")
  gtree <- as.numeric(as.character(predict(treeclass, newdata = data.frame(x = X_test, y = Y_test), 
                                           type="class")))
  R.tree[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, g = gtree, epsilon = 0)
  
  ##### Regression trees without mass ######
  treeclass.eps <- rpart(y~., data=data.frame(x=X_train, y = as.factor(Y_train.eps)), method = "class")
  gtree <- as.numeric(as.character(predict(treeclass.eps, newdata = data.frame(x = X_test, y = Y_test), 
                                           type="class")))
  gtreeeps <- as.numeric(as.character(predict(treeclass.eps, newdata = data.frame(x = X_test, y = Y_test.eps), 
                                              type = 'class')))
  R.tree.eps[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, Y.eps = Y_test.eps, 
                                                             g = gtree, g.eps = gtreeeps,
                                                             epsilon = eps)
  
  #### SVM linear kernel with mass #####
  svmclass <- svm(y~., data=data.frame(x=X_train,y=Y_train),
                  type = 'C-classification', kernel = 'linear')
  gsvm <- as.numeric(as.character(predict(svmclass, newdata = data.frame(x = X_test, y = Y_test))))
  R.svml[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, g = gsvm, epsilon = 0)
  
  #### SVM linear kernel without mass #####
  svmclass.eps <- svm(y~., data=data.frame(x=X_train,y=Y_train.eps),
                      type = 'C-classification', kernel = 'linear')
  gsvm <- as.numeric(as.character(predict(svmclass.eps, newdata = data.frame(x = X_test, y = Y_test))))
  gsvmeps <- as.numeric(as.character(predict(svmclass.eps, newdata = data.frame(x = X_test, y = Y_test.eps))))
  R.svml.eps[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, Y.eps = Y_test.eps, 
                                                             g = gsvm, g.eps = gsvmeps,
                                                             epsilon = eps)
  
  #### Random forest with mass #####
  forest = randomForest(x = X_train, y =as.factor(Y_train))
  grf = as.numeric(as.character(predict(forest,newdata = 
                                          cbind(X_test,Y_test))))
  R.rf[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, g = grf, epsilon = 0)
  
  #### Random forest without mass #####
  forest.eps = randomForest(x = X_train, y =as.factor(Y_train.eps))
  grf = as.numeric(as.character(predict(forest.eps,newdata = 
                                          cbind(X_test,Y_test))))
  grfeps = as.numeric(as.character(predict(forest.eps,newdata = 
                                             cbind(X_test,Y_test.eps))))
  R.rf.eps[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, Y.eps = Y_test.eps, 
                                                           g = grf, g.eps = grfeps,
                                                           epsilon = eps)
  
  
  
  #### Logistic regression with mass #####
  w = which(Y_train==1)
  X_train1 = rbind(X_train[w,],X_train[1:200,])
  Y_train1 = c(Y_train[w],Y_train[1:200])
  fraction <- table(Y_train1)/length(Y_train1)
  weights <- 1 - fraction[as.character(Y_train1)]
  m_train1 = apply(X_train1,2,mean)
  sd_train1 = apply(X_train1,2,sd)
  cv.lasso = cv.glmnet(x=scale(as.matrix(X_train1)),
                       y=as.factor(Y_train1),
                       weights=weights,
                       family="binomial")
  glasso = as.numeric(predict(cv.lasso,scale(as.matrix(X_test),
                                             center = m_train1,
                                             scale = sd_train1),
                              type="class"))
  R.lasso[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, g = glasso, epsilon = 0)
  
  #### Logistic regression without mass #####
  w = which(Y_train.eps==1)
  X_train1 = rbind(X_train[w,],X_train[1:200,])
  Y_train1 = c(Y_train.eps[w],Y_train.eps[1:200])
  fraction <- table(Y_train1)/length(Y_train1)
  weights <- 1 - fraction[as.character(Y_train1)]
  m_train1 = apply(X_train1,2,mean)
  sd_train1 = apply(X_train1,2,sd)
  cv.lasso.eps = cv.glmnet(x=scale(as.matrix(X_train1)),
                           y=as.factor(Y_train1),
                           weights=weights,
                           family="binomial")
  glasso.eps = as.numeric(predict(cv.lasso,scale(as.matrix(X_test),
                                                 center = m_train1,
                                                 scale = sd_train1),
                                  type="class"))
  R.lasso.eps[[b]] = ExtremesBinaryClassifier:::EmpiricalRisk(Y = Y_test, Y.eps = Y_test.eps, 
                                                              g = glasso, g.eps = glasso.eps,
                                                              epsilon = eps)
}


par(mfrow=c(1,2))
## separated plot #####
boxplot(cbind(unlist(lapply(R.lin, `[[`, 1)),
              unlist(lapply(R.tree, `[[`, 1)),
              unlist(lapply(R.svml, `[[`, 1)),
              unlist(lapply(R.lasso, `[[`, 1)),
              unlist(lapply(R.rf, `[[`, 1))),
        ylab='Risk',cex.lab=2, cex.axis=2,
        cex.main=2, cex.sub=2,cex=1.5,ylim=c(0,1),
        xaxt='n',main=expression(paste(epsilon,"=0")),
        col="#DC3220")
axis(1, at = c(1,2,3,4,5),
     cex=1.5,lwd=2,
     labels = c('Linear','Tree','SVM','Lasso','RF'),
     cex.axis=2)

boxplot(cbind(unlist(lapply(R.lin.eps, `[[`, 1)),
              unlist(lapply(R.tree.eps, `[[`, 1)),
              unlist(lapply(R.svml.eps, `[[`, 1)),
              unlist(lapply(R.lasso.eps, `[[`, 1)),
              unlist(lapply(R.rf.eps, `[[`, 1))),
        ylab='Risk',cex.lab=2, cex.axis=2,
        cex.main=2, cex.sub=2,cex=1.5,ylim=c(0,1),
        xaxt='n',
        col="#005AB5",main=expression(paste(epsilon,">0")))
axis(1,  at = c(1,2,3,4,5),
     cex=1.5,lwd=2,
     labels = c('Linear','Tree','SVM','Lasso','RF'),
     cex.axis=2)

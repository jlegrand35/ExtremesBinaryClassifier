library(evd)
n<-300
b<-10
xiX<-0.1
X<-rgpd(n,0,1,xiX)
Z1 <- rgpd(n,0,1,xiX/b)*(1-pgpd(X,0,1,xiX))
Y<- Z1*(X)/(3+X^2) 

plot(X,Y)
xx<-seq(from=0, to=max(X), length.out = n)
lines(xx,(xx)/(3+xx^2),type="l", lwd=2, col=3)
lines(xx,(1-pgpd(xx,0,1,xiX)),type="l", lwd=2, col=2)

H = 10*Y


thresh = quantile(H,probs=0.7) #0.8
thresh
eps = 0.7
eps_u = eps*thresh #0.6

plot(X,H,pch=20,col='grey',
     cex.lab=2, cex.axis=2, cex.main=2, 
     cex.sub=2,cex=2,
     ylab='Y',xlab='X')
points(X[H>eps_u & X>eps_u],
       H[H>eps_u & X>eps_u],
       pch=20,cex=2)
abline(h=thresh,col='blue',lwd=2,lty='dotted')
abline(v=thresh,col='blue',lwd=2,lty='dotted')

B = 100 # number of samples for the cross validation

R.lin = R.lin.eps = R.tree = R.tree.eps = R.svml = 
  R.svml.eps = R.lasso = R.lasso.eps = R.rf =
  R.rf.eps = vector(mode = "list", length = B)

theta.all = theta.all.eps = matrix(NA,nrow=B,ncol=1)

X.all = cbind(X)

reslin0=lm(H~X,
           data=data.frame(H=H,X=X))
reslm0 = reslin0$coefficients[2]

reslin=lm(H~X,
          data=data.frame(
            H=H[H>eps_u & X>eps_u],
            X=X[H>eps_u&  X>eps_u ]))
reslm = reslin$coefficients[2]

u = thresh
for (b in 1:B){
  ##### Split data training-validation sets #####
  ind.train <- sample.int(length(H),
                          size = 0.7*length(H),
                          replace=F)
  
  X_train <- X[ind.train] 
  X_test <- X[-ind.train] 
  
  H_train <- H[ind.train]
  H_test <- H[-ind.train]
  Y_train <- 2*(H_train > u) - 1 
  Y_train.eps <- 2*(H_train > eps_u) - 1 
  Y_test <- 2*(H_test > u) - 1 
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
  
}

par(mfrow=c(1,2))
## separated plot #####
boxplot(cbind(unlist(lapply(R.lin, `[[`, 1)),
              unlist(lapply(R.tree, `[[`, 1)),
              unlist(lapply(R.svml, `[[`, 1)),
              unlist(lapply(R.rf, `[[`, 1))),
        ylab='Risk',cex.lab=2, cex.axis=2,
        cex.main=2, cex.sub=2,cex=1.5,ylim=c(0,1),
        xaxt='n',main=expression(paste(epsilon,"=0")),
        col="#DC3220")
axis(1, at = c(1,2,3),
     cex=1.5,lwd=2,
     labels = c('Linear','Tree','SVM'),
     cex.axis=2)

boxplot(cbind(unlist(lapply(R.lin.eps, `[[`, 1)),
              unlist(lapply(R.tree.eps, `[[`, 1)),
              unlist(lapply(R.svml.eps, `[[`, 1))),
        ylab='Risk',cex.lab=2, cex.axis=2,
        cex.main=2, cex.sub=2,cex=1.5,ylim=c(0,1),
        xaxt='n',
        col="#005AB5",main=expression(paste(epsilon,">0")))
axis(1, at = c(1,2,3),
     cex=1.5,lwd=2,
     labels = c('Linear','Tree','SVM'),
     cex.axis=2)


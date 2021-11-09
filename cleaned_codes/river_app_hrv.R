dir="/home/juliette/Documents/Philippe_Marco/codes_R_rev"
setwd(dir)

library(rpart)
library(e1071) 
library(glmnet)
library(randomForest)

# source('functions_risk.R')

library(devtools)
install("/home/juliette/Documents/Philippe_Marco/package/ExtremesBinaryClassifier",
        force=TRUE) #install package
library(ExtremesBinaryClassifier)

##################################
##################################

Data = graphicalExtremes::danube$data

Data <- apply(Data, MARGIN=2,
           FUN=function(x) x-min(x) )

##################################
##################################

par(mfrow=c(1,1),mar=c(4.3,5.1,2.1,2.1))

#########################

thresh = quantile(Data[,1],probs=0.8) #0.8
thresh
eps = 0.6
eps_u = eps*thresh #0.6
alpha = 20
X23 = alpha*Data[,23]
X24 = alpha*Data[,24]
H = Data[,1]
plot(X23,H,pch=20,col='grey',
     cex.lab=2, cex.axis=2, cex.main=2,
     cex.sub=2,cex=2,
     ylab='Station 1',xlab='Station 23')#,
     # xlim=range(c(Data[,1],Data[,23])),
     # ylim=range(c(Data[,1],Data[,23])))
points(X23[H>eps_u & (X24>eps_u | X23>eps_u)],
       H[H>eps_u & (X24>eps_u | X23>eps_u)],
       pch=20,cex=2)
abline(h=thresh,col='blue',lwd=2,lty='dotted')
abline(v=thresh,col='blue',lwd=2,lty='dotted')
plot(X24,H,pch=20,col='grey',
     cex.lab=2, cex.axis=2, cex.main=2,
     cex.sub=2,cex=2,
     ylab='Station 1',xlab='Station 24',
     xlim=range(c(Data[,1],Data[,24])),
     ylim=range(c(Data[,1],Data[,24])))
points(X24[H>eps_u & (X24>eps_u | X23>eps_u)],
       H[H>eps_u & (X24>eps_u | X23>eps_u)],
       pch=20,cex=2)
abline(h=thresh,col='blue',lwd=2,lty='dotted')
abline(v=thresh,col='blue',lwd=2,lty='dotted')

# 
# Hfrechet = ExtremesBinaryClassifier:::FrechetMargin(H)
# X23frechet = ExtremesBinaryClassifier:::FrechetMargin(Data[,23])
# X24frechet = ExtremesBinaryClassifier:::FrechetMargin(Data[,24])
# 
# 
# plot(X23frechet,Hfrechet,#pch=20,
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,#cex=2,
#      ylab='Station 1',xlab='Station 23')
# for (i in 1:7){
#   points(X23frechet[order(X23,decreasing = T)[i]],
#          Hfrechet[order(X23,decreasing = T)[i]],
#          col=i,pch=20)
#   text(x=X23frechet[order(X23,decreasing = T)[i]],
#        y=Hfrechet[order(X23,decreasing = T)[i]]+10,
#        labels = i)
# }
# plot(X23,H,#pch=20,col='grey',
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,#cex=2,
#      ylab='Station 1',xlab='Station 23')
# for (i in 1:7){
# points(X23[order(X23,decreasing = T)[i]],
#        H[order(X23,decreasing = T)[i]],
#        col=i,pch=20)
#   text(x=X23[order(X23,decreasing = T)[i]],
#        y=H[order(X23,decreasing = T)[i]]+100,
#        labels = i)
# }
# 
# plot(X24frechet,Hfrechet,pch=20,
#      cex.lab=2, cex.axis=2, cex.main=2, 
#      cex.sub=2,cex=2,
#      ylab='Station 1',xlab='Station 24')

B = 100 # number of samples for the cross validation

R.lin = R.lin.eps = R.tree = R.tree.eps = R.svml = 
  R.svml.eps = R.lasso = R.lasso.eps = R.rf =
  R.rf.eps = vector(mode = "list", length = B)

theta.all = theta.all.eps = matrix(NA,nrow=B,ncol=2)

X.all = cbind(X24,X23)

reslin0=lm(H~X23+X24,
           data=data.frame(H=H,X23=X23,X24=X24))
reslm0 = reslin0$coefficients[2:3]

reslin=lm(H~X23+X24,
          data=data.frame(
            H=H[H>eps_u & (X23>eps_u | X24>eps_u)],
            X23=X23[H>eps_u&  (X23>eps_u | X24>eps_u)],
            X24 = X24[H>eps_u & (X23>eps_u | X24>eps_u)]))
reslm = reslin$coefficients[2:3]

u = thresh
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
              unlist(lapply(R.svml, `[[`, 1))),#,
              # unlist(lapply(R.lasso, `[[`, 1)),
              # unlist(lapply(R.rf, `[[`, 1))),
        ylab='Risk',cex.lab=2, cex.axis=2,
        cex.main=2, cex.sub=2,cex=1.5,ylim=c(0,1),
        xaxt='n',main=expression(paste(epsilon,"=0")),
        col="#DC3220")
axis(1, at = c(1,2,3),#,4,5),
     cex=1.5,lwd=2,
     labels = c('Linear','Tree','SVM'),#,'Lasso','RF'),
     cex.axis=2)

boxplot(cbind(unlist(lapply(R.lin.eps, `[[`, 1)),
              unlist(lapply(R.tree.eps, `[[`, 1)),
              unlist(lapply(R.svml.eps, `[[`, 1))),#,
              # unlist(lapply(R.lasso.eps, `[[`, 1)),
              # unlist(lapply(R.rf.eps, `[[`, 1))),
        ylab='Risk',cex.lab=2, cex.axis=2,
        cex.main=2, cex.sub=2,cex=1.5,ylim=c(0,1),
        xaxt='n',
        col="#005AB5",main=expression(paste(epsilon,">0")))
axis(1,  at = c(1,2,3),#,4,5),
     cex=1.5,lwd=2,
     labels = c('Linear','Tree','SVM'),#,'Lasso','RF'),
     cex.axis=2)

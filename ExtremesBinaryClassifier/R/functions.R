rm(list = ls()) #Delete objects from the memory


#' A Risk estimation function
#'
#' Compute the empirical risk defined ???? given the output of a classifier g and the true binary outcomes Y. If epsilon>0, supply the values of the trained classifier on the thresholded data g.eps and the true binary outcomes Y.eps := +1 if H > eps_u and -1 otherwise.
#' @param Y vector of the true binary outcomes
#' @param Y.eps vector of the true binary outcomes in the extreme region
#' @param g vector of the predicted binary outcomes from a given classifier
#' @param g.eps vector of the predicted binary outcomes in the extreme region from the same classifier
#' @param epsilon logical value indicating whether the classic risk function should be used or the extended version
#' @references Legrand et al.
#' @details to add
#' @seealso \link{LinearClassifier}
#' @examples
#' require(rpart)
#'
#' set.seed(123)
#' ## Reproduce the simulation example from Legrand et al.
#' nsim <- 1e4
#' X1 <- 1/(runif(nsim)^(1/3))
#' X2 <- 1/(runif(nsim)^(1/2))
#' P <- 1/(runif(nsim)^(1/2))
#' H <- X1 + P
#' ## Compute the two thresholds u and epsilon_u
#' u <- quantile(H,probs=0.97)
#' eps <- 0.7
#' eps_u <- u*eps
#' ## Split data between training and testing sets
#' ii <- sample.int(length(H), size = 0.7*length(H), replace=F)
#' Xtrain <- cbind(X1[ii], X2[ii])
#' Xtest <- cbind(X1[-ii], X2[-ii])
#' Htrain <- H[ii]
#' Htest <- H[-ii]
#'
#' ## Linear classifier
#' #~~~~~~~~~~~~~~~~~~~
#' ## Train the linear classifier with mass
#' init0 <- lm(H ~ X1 + X2, data = data.frame(H = H, X1 = X1, X2 = X2))$coefficients[2:3]
#' linclass <- LinearClassifier(X = Xtrain, thresh = u, H = Htrain, initials = init0, epsilon=0)$theta
#' ## Compute the predicted binary outcome on the test set from all the data
#' glin <- 2*(as.vector(linclass %*% t(Xtest)) > u) -1
#' #~~~~~~~~~~~~~~~~~~~
#' ## Train the linear classifier without mass
#' glin0 <- init0 %*% t(cbind(X1,X2))
#' init <- lm(H ~ X1 + X2, data=data.frame(H = H[H>eps_u & glin0>eps_u], X1 = X1[H>eps_u & glin0>eps_u],
#'           X2 = X2[H>eps_u & glin0>eps_u]))$coefficients[2:3]
#' linclass.eps <- LinearClassifier(X = Xtrain[,c(1,2)], thresh = u, H = Htrain, initials = init,
#'                   epsilon = eps)$theta
#' ## Compute the predicted binary outcome on the test set from the extreme region data
#' glineps <- 2*(as.vector(linclass.eps %*% t(Xtest)) > eps_u) - 1
#' #~~~~~~~~~~~~~~~~~~~
#' ## Compute the true binary outcome on the test set from all the data and only with the extreme region data
#' Ytesteps <- 2*(Htest > eps_u) - 1
#' Ytest <- 2*(Htest > u) - 1
#' #~~~~~~~~~~~~~~~~~~~
#' ## Compute the associated risk
#' EmpiricalRisk(Y = Ytest, Y.eps = Ytesteps, g = glin, g.eps = glineps, epsilon = TRUE)
#'
#' ## Comparison with regression tree
#' #~~~~~~~~~~~~~~~~~~~
#' ## Train the tree classifier with mass
#' Ytrain <- 2*(Htrain > u) - 1
#' treeclass <- rpart(y~., data=data.frame(x = Xtrain, y = as.factor(Ytrain)), method = "class")
#' ## Compute the predicted binary outcome on the test set from all the data
#' gtree <- as.numeric(as.character(predict(treeclass, newdata = data.frame(x = Xtest, y = Ytest),
#'            type="class")))
#' #~~~~~~~~~~~~~~~~~~~
#' ## Train the tree classifier without mass
#' Ytraineps <- 2*(Htrain > eps_u) - 1
#' treeclasseps <- rpart(y~., data=data.frame(x = Xtrain, y = as.factor(Ytraineps)), method = "class")
#' ## Compute the predicted binary outcome on the test set from the extreme region data
#' gtreeeps <- as.numeric(as.character(predict(treeclasseps, newdata = data.frame(x = Xtest, y = Ytesteps),
#'               type = 'class')))
#' #~~~~~~~~~~~~~~~~~~~
#' ## Compute the associated risk
#' EmpiricalRisk(Y = Ytest, Y.eps = Ytesteps, g = gtree, g.eps = gtreeeps, epsilon = TRUE)
EmpiricalRisk = function(Y, Y.eps = NULL, g, g.eps = NULL, epsilon = TRUE){
  stopifnot(length(Y) == length(g))
  stopifnot(length(Y.eps) == length(g.eps))
  stopifnot(is.logical(epsilon))
  if (epsilon == TRUE){
    Risk <- sum((g != Y) & (Y.eps == 1) & (g.eps == 1))/
      sum((g == 1 | Y == 1) &
            (Y.eps == 1) & (g.eps == 1))
  }
  else{
    Risk <- sum((g != Y)) / sum(((g == 1) | (Y == 1)))
  }
  return(Risk)
}

#' Optimal linear classifier
#'
#' Compute the optimal linear classifier as defined in section ??? by minimizing the empirical risk (defined by emp.risk.lin).
#' Initial values must be provided which can be estimated by performing a classical linear regression (lm) for example.
#' @param X numeric matrix corresponding to the input data we want to classify
#' @param thresh single numeric giving the threshold over which an extreme event is defined
#' @param H numeric vector corresponding to the latent variable that we wish to predict
#' @param initials initial values for the parameters of the linear classifier to be optimized over
#' @param epsilon single numeric giving the amount of data to remove
#' @references Legrand et al.
#' @return theta value of the linear classifier
#' \item{theta}{the optimal parameters for the linear classifier}
#' \item{Risk}{the value of the risk corresponding theta}
#' @examples
#' set.seed(123)
#' ## Reproduce the simulation example from Legrand et al.
#' nsim <- 1e4
#' X1 <- 1/(runif(nsim)^(1/3))
#' X2 <- 1/(runif(nsim)^(1/2))
#' P <- 1/(runif(nsim)^(1/2))
#' H <- X1 + P
#' u <- quantile(H,probs=0.97)
#' init <- lm(H ~ X1 + X2, data=data.frame(H = H, X1 = X1, X2 = X2))$coefficients[2:3]
#' LinearClassifier(X = cbind(X1, X2), thresh = u, H = H, initials = init, epsilon = 0)
LinearClassifier = function(X, thresh, H, initials, epsilon){
  stopifnot(length(initials)==ncol(as.matrix(X)))
  stopifnot(length(H)==nrow(as.matrix(X)))
  stopifnot(epsilon >= 0)
  stopifnot(thresh >= 0)
  nvar <- ncol(as.matrix(X))
  init <- log(abs(initials))
  res <- optim(par = init, fn = EmpRiskLin, X = X,
               thresh = thresh, H = H, epsilon = epsilon,
               control = list(parscale = rep(1e-2,nvar)),
               method = 'SANN')
  return(list("theta" = exp(res$par), "Risk" = res$value))
}


#' A risk estimation function for linear classifiers
#'
#' Empirical risk estimate specific to the linear classifier
#' @note This function should not really be used by the user, it is only called by the LinearClassifier function
#' @references Legrand et al.
#' @param X numeric matrix corresponding to the input data we want to classify
#' @param thresh single numeric giving the threshold over which an extreme event is defined
#' @param H numeric vector corresponding to the latent variable that we wish to predict
#' @param theta parameters for the linear classifier
#' @param epsilon single numeric giving the amount of data to remove
#' @return The empirical risk of the linear classifier with theta coefficients is computed
#' @noRd
EmpRiskLin = function(theta, X, thresh, H, epsilon){
  if (length(theta)!=ncol(as.matrix(X)) |
      length(H)!=nrow(as.matrix(X))){
    stop("H, X and theta must have same length.")}
  stopifnot(epsilon >= 0)
  exp.theta <- exp(theta)
  g <- as.vector(t(exp.theta) %*% t(X))
  g <- 2*(g > thresh) - 1
  Y <- 2*(H > thresh) - 1
  if (epsilon > 0){
    g.eps <- 2*(g > thresh*epsilon) - 1
    Y.eps <- 2*(H > thresh*epsilon) - 1
    Risk <-  EmpiricalRisk(Y = Y, g = g, g.eps = g.eps, Y.eps = Y.eps,
                           epsilon = TRUE)
  }
  else{
    Risk <-  EmpiricalRisk(Y = Y, g = g, epsilon = FALSE)
  }
  return(Risk)
}


#' Danube river discharges
#'
#' Daily river discharges, measured in \eqn{m^3/s}, at 31 stations spread over the upper Danube basin. The data set covers the period from 1960 to 2010 but only the months of June, July, and August are retained. These data are already declustered following  Mhalla et al.(2020) methodology.
#' @docType data
#' @name dataDanube
#' @usage graphicalExtremes::danube
#' @references Asadi, P., Davison, A.C.,  and Engelke, S. (2015). Extremes on river networks. The Annals of Applied Statistics, 9(4), 2023-2050.
#' @source Bavarian Environmental Agency (\url{http://www.gkd.bayern.de})
#'
#' @examples
#' graphicalExtremes::danube
NULL


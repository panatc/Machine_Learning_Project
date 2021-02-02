### Pan Charoensuk
### V00827791
### Stat 454 Course Project
### Parts of the code were written by Dr. Zhang


library(caret)
library(glmnet)

# loading in data
df <- read.csv("C:/Users/Admin/Desktop/math stuff/stat454/Project/winequality-red.csv")

# assign the quality of red wine to the y column
ymat <- as.matrix(df$quality)
xmat <- as.matrix(df[,-12])
set.seed(1)

idx.cv <- createFolds(rowMeans(df), k=10, list=F)


# fit cv.glmnet, also choose best alpha

cv.glmnet2 <- function(xx, yy, foldid=NULL, nfold=10, alphas=0:10/10, ...)
  
{
  
  if(is.null(foldid)) foldid <- createFolds(yy, k=nfold, list=F)
  
  fits <- vector("list",length(alphas))
  
  names(fits) <- alphas
  
  
  
  for(ii in 1:length(alphas))
    
  {
    
    fits[[ii]] <- cv.glmnet(x=xx, y=yy, foldid=foldid, alpha=alphas[ii],...)
    
  }
  
  idx <- which.min(sapply(fits, function(xx) xx$cvm[which(xx$lambda==xx$lambda.1se)]))
  
  fits[[idx]]$alpha <- alphas[idx]
  
  return(fits[[idx]])
  
}

missclass <- matrix(nrow = 10, ncol = 5)

# compare each method on 10 folds
for (ii in 1:10){
    
  y.train <- ymat[idx.cv!=ii, ]
    
  y.test <-  ymat[idx.cv==ii, ]
    
  x.train <- xmat[idx.cv!=ii, ]
    
  x.test <-  xmat[idx.cv==ii, ]
    
    
    
  y.pred1 <- ymat
    
  y.pred1[!is.na(y.pred1)] <- NA	
    
  y.pred2 <- y.pred1
    
    
  
  #applying the cutoff, >5 having good wine quality
  y01 <- (y.train > 5)
    
  y02 <- (y.test > 5)
    
  df1 = data.frame(y01, x.train)
  df2 = data.frame(yy=y.train,x.train)
    
    
  set.seed(ii)
  print(ii)
    
    
  # Penalized linear model
    
  # cv.glmnet for continous variables	
  fit1 <-  cv.glmnet2(x.train, y.train, nfold=10)
  y.pred1 <- predict(fit1, x.test)
  y.pred1 <- y.pred1[!is.na(y.pred1)]
  c.glm=rep("FALSE",length(y.pred1))
  c.glm[y.pred1 > 5] = "TRUE"
  #  table(c.glm,y02)
  #  mean(c.glm==y02)
  missclass[ii,1] <- mean(c.glm!=y02)
    
    
  # cv.glmnet after the cutoff
  fit2 <-  cv.glmnet2(x.train, y01, nfold=10, family="binomial")
  
  y.pred2 <- predict(fit2, x.test, type="response")  
  y.pred2 <- y.pred2[!is.na(y.pred2)]
  p.glm=rep("FALSE",length(y.pred2))
  p.glm[y.pred2 >.5] = "TRUE"
  #  table(p.glm,y02)
  #  mean(p.glm==y02)
  missclass[ii,2] <- mean(p.glm!=y02)
    
    
  # Logistic Regression
  glm.fits=glm(y01~.,family=binomial, data=df1)
  glm.probs=predict(glm.fits,newdata=data.frame(x.test),type="response")
  glm.pred=rep("FALSE",length(glm.probs))
  glm.pred[glm.probs>.5]="TRUE"
  #  table(glm.pred,y02)
  #  mean(glm.pred==y02)
  missclass[ii,3] <- mean(glm.pred!=y02)
    
    
  # Linear Discriminant Analysis
  library(MASS)
  lda.fit=lda(y01~.,data=df1)
  lda.pred=predict(lda.fit, data.frame(x.test))
  lda.class=lda.pred$class
  #  table(lda.class,y02)
  #  mean(lda.class==y02)
  missclass[ii,4] <- mean(lda.class!=y02)
    
    
  # Linear model
  lmfit <- lm(yy~., data=df2)
  lm.predict <- predict.lm(lmfit,newdata = data.frame(x.test))
  linear.predict=rep("FALSE",length(lm.predict))
  linear.predict[lm.predict > 5]="TRUE"
  #  table(linear.predict,y02)
  #  mean(linear.predict==y02)
  missclass[ii,5] <- mean(linear.predict!=y02)
    
    
}  
      
  
  
`colnames<-`(missclass, c("Con.glmnet", "bin.glmnet", "Logis", "LDA", "LM"))

# average of each method

avg <- colSums(missclass)/10
avg


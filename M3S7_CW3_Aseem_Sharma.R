require(ggplot2)

# Load libraries


# Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7\\')
library(MASS)
library(corrplot)
library(FSelector)
library(class)
library(rpart)
library(nnet)
library(rpart.plot)


set.seed(1)
data <- read.table("http://www2.imperial.ac.uk/~eakc07/S7/data4.dat",quote="\"'")
data.r <- sort(sample(dim(data)[1],100,FALSE))
data.te <- as.data.frame(data[data.r,])

data.df <- as.data.frame(data[-data.r,])
colnames(data.df) <- c(0:28)
par(mfrow=c(4,7))



########
par(mfrow=c(4,7))
par(cex = 0.6)
par(mar = c(0, 0, 0,0), oma = c(4, 4, 4, 0.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))
for (i in 2:29){
  plot(c(1:1027),data.df3[,i],col = (ifelse(data.df3[,1]==0,"blue","dark orange")),xlab=i-1,ylim=range(-4,4))
  title(sub = bquote(paste("Dimension ", .(i-1))), line = -2)
}
mtext("Observation Number", side = 1, outer = TRUE, cex = 1, line = 2.2,
      col = "grey20")
mtext("Standard Deviations from Mean", side = 2, outer = TRUE, cex = 1, line = 2.2,
      col = "grey20")
mtext("Plot of Scaled Data", side = 3, outer = TRUE, cex = 1, line = 2.2,
      col = "grey20")

########

#  }

##### table
data.df2 <- data.df
t(as.table(summary(data.df[,2:29])))
(summary(data.df2[,2:29]))
apply(data.df2, 2, sd)


#####



#####
data.df[532:533,] # Check where the split between 0 and 1 is

data.df2.0 <- data.df[1:532,] # split data into classes
data.df2.1 <- data.df[533:nrow(data.df),] # split data into classes
for (i in 1:29)
{
data.df2.0[is.na(data.df2.0[,i]), i] <- mean(data.df2.0[,i], na.rm = TRUE) # replace NAs with class mean
}
for (i in 1:29)
{
data.df2.1[is.na(data.df2.1[,i]), i] <- mean(data.df2.1[,i], na.rm = TRUE) # replace NAs with class mean
}

data.df2 <- rbind(data.df2.0,data.df2.1) # rebuild training with NAs replaced, used for QDA, KNN
data.df3 <- cbind(data.df2[,1],scale(data.df2[,-1])) # normalised data, used for KNN
data.df4 <- cbind(data.df2[,1],apply(data.df2[,2:29],2,function(a){(a-min(a))/max(a-min(a))})) 
# data scaled between 0 and 1, used for MLP


###### correlation plot

par(mfrow=c(1,1))
correlation_matrix <- cor(as.matrix(data.df2[2:29]))
correlation_matrix.selected <- cor(as.matrix(data.df2[2:19]))
corrplot(correlation_matrix.selected,  tl.pos = "d")
title(main="Correlation between selected Data Sets", outer = TRUE, line = 0)



######## Box Plot
class.data.list <- list()
kcount <- 2
for (i in seq(1,56,2))
{
  class.data.list[[i]] <- data.df2.0[,kcount]
  class.data.list[[i+1]] <- data.df2.1[,kcount]
  kcount <- kcount + 1
}

par(mfrow=c(1,1))
boxplot(class.data.list, ylab = "Values",xlab = "Dimension Number", notch = TRUE, col = c("dark orange","blue"), xaxt='n')
axis(side=1, at=seq(1,56,2),labels=c(1:28))
legend(x=1,y=600, ncol=1,  cex=1, c("Class 1","Class 2"),text.width=5,lty=c(1,1),lwd=c(6,6),col=c("dark orange","blue"))
#boxplot(data.df2[,2:29], ylab = "Values", notch=TRUE)
title(main="All Box Plots", line = 2,xlab="Dimension Number",  ylab="Values", outer=TRUE)



########
# Functions for separability Plot

boundary.select <- function(class.data, sensitivity)
{
  c <- cbind(1:(sensitivity+1),(1:(sensitivity+1)))
  k = 1
  for (i in seq(as.integer(min(class.data[,2])), as.integer(max(class.data[,2])),(abs(as.integer(min(class.data[,2])))+ abs(as.integer(max(class.data[,2]))))/sensitivity))
{
    class.data2 = class.data
   class.data2[,2] <- replace(class.data2[,2], class.data2[,2] > i, 1 )
   class.data2[,2] <- replace(class.data2[,2], class.data2[,2] != 1, 0)
   
   c[k,2]= 0.5-abs(0.5 - (sum(class.data2[,1]==class.data2[,2])/nrow(class.data)))
   c[k,1]= i
   k=k+1
  }
  print(min(c[,2]))
  min(c[,2])
}
boundary.select.all <- function(sensitivity)
{
  error.rate <- c(1:28)
  for (i in 2:29)
  {
    error.rate[i-1] <- boundary.select(as.matrix(data.df2[,c(1,i)]),sensitivity)
  }
  error.rate
}
error.rate.aseem <- boundary.select.all(100)
c.e<-cbind(c(1:28),error.rate.aseem[c(1:28)])

class.error <- c.e[order(c.e[,2]),]


par(mfrow=c(1,1))
plot(c(1:28),class.error[,2],  cex = 4,xlab="Dimension Number", ylab="Error Rate")
title(main="Separability of Classes based on one dimensional classification", outer=TRUE, xlab="Dimension Rank", ylab="Error Rate")
text(c(1:28), class.error[,2], class.error[,1], cex=1,  col=(ifelse(class.error[,2]<0.3,"dark green","red")))

##########

##################################
# Cross Validation Funtion
##########################
my.ten.cv <- function( Hwd=c(3,0.001),x, kay, folds=10, classifier)
{
  set.seed(1)
  a <- rep(NA, folds)
  x.r <- x[sample(nrow(as.matrix(x))),]
  folds2 <- cut(seq(1,nrow(x.r)),breaks=folds,labels=FALSE)

  if (classifier == "KNN")
  {
    for (i in 1:folds)
    {
      testIndices <- which(folds2==i,arr.ind=TRUE)
      x.te <- x.r[testIndices,]
      x.tr <- x.r[-testIndices,]
      cl <- as.numeric(knn(as.matrix(x.tr[,-1]),as.matrix(x.te[,-1]),x.tr[,1], k=kay)) -1
      a[i] <- sum(cl != x.r[testIndices,1])/nrow(x.te)
    }
  }
  else if (classifier == "QDA")
  {
    for (i in 1:folds)
    {
      testIndices <- which(folds2==i,arr.ind=TRUE)
      x.te <- x.r[testIndices,]
      x.tr <- x.r[-testIndices,]
      qda.l <- qda(as.matrix(x.tr[,-1]),as.matrix(x.tr[,1]))
      qda.pred <- predict(qda.l,as.matrix(x.te[,-1]))
      a[i] <- sum(qda.pred$class != x.r[testIndices,1])/nrow(x.te)
    }
    
  }
  else if (classifier == "LD")
  {
    for (i in 1:folds)
    {
      testIndices <- which(folds2==i,arr.ind=TRUE)
      x.te <- as.data.frame(x.r[testIndices,])
      x.tr <- as.data.frame(x.r[-testIndices,])
      names(x.te)[1]<-"c"
      names(x.tr)[1]<-"c"
      ld.1 <- glm(c~.,data=x.tr,family="binomial")
      ld.pred <- as.numeric(predict(ld.1, x.te, type="response") >0.5)
      a[i] <- sum(ld.pred != x.r[testIndices,1])/nrow(x.te)
    }
  }
  else if (classifier == "CART")
  {
    for (i in 1:folds)
    {
      testIndices <- which(folds2==i,arr.ind=TRUE)
      x.te <- x.r[testIndices,]
      x.tr <- x.r[-testIndices,]
      gini.tree <- rpart(as.factor(x.tr[,1])~., data = x.tr[,-1])
      prune.gini.tree<- prune(gini.tree, 0.017 ) # 0.017 determined from plot
      gini.class <- predict(prune.gini.tree, x.te[,-1], type='vector')-1
      a[i] <- sum(gini.class != x.r[testIndices,1])/nrow(x.te)
    }
  }
  else if (classifier == "MLP")
  {
    for (i in 1: folds)
    {
      H <- Hwd[1]
      wd <- Hwd[2]
      testIndices <- which(folds2==i,arr.ind=TRUE)
      x.te <- as.data.frame(x.r[testIndices,])
      x.tr <- as.data.frame(x.r[-testIndices,])
      error.rate <- rep(NA, kay)
      names(x.te)[1]<-"c"
      names(x.tr)[1]<-"c"
        count <- 0
      iteration <- 0

      while(count < kay) 
      {
        iteration = iteration + 1
        set.seed(iteration)

        mlp <- nnet( as.factor(c)~. , data = x.tr, size = H,decay = wd, maxit = 200, trace=FALSE)
        evals <- eigen( nnetHess(mlp, x.tr[, -1], x.tr[, 1]), T)$values
        if(min(evals) > 0.00001) 
          # ensure a minimum is found
        {
          
          if (ncol(as.matrix(x.tr[,-1]))==1)
            # avoid Data Frame naming issues
          {
            x.te.1 <-as.data.frame(x.te[,-1])
            names(x.te.1)[1] <- "V2"
            pred.mlp <- predict(mlp,x.te.1)
            count <- count + 1
          }
          else
          {
          pred.mlp <- predict(mlp, x.te[,-1])
          count <- count + 1 
          }
          error.rate[count] = sum((pred.mlp > 0.5) != x.r[testIndices,1])/nrow(x.te)
        }     
      }
      a[i] <- sum(error.rate)/kay
      # average error rates
    }
    
        }

  sum(a)/folds # error rate
}


##############


wrapper <- function (x, k = 7, size = 10, classifier)
# wrapper function
  
{
  set.seed(1)

  sub.wrapper <- function(x, a, k,classifier)
  # sub wrapper
    
  {
    
    c <- c(1:(ncol(x)-1))
    c <- cbind(c,c)
    # prepares a matrix to store dimension number and associated error
    
    for (i in 1:(ncol(x)-1))
    # loops across all dimensions
    {
      
      # binds the remaining data with features selected so far
      c[i,2] <- my.ten.cv(x=cbind(as.matrix(x[-1,1]),as.matrix(x[-1,i+1]),a),kay=k,classifier=classifier)
    }
    
    c[,1] <- as.matrix(x[1,2:ncol(x)])
    print(which.min(c[,2]))
    
    # returns dimension which had lowest error
    c[which.min(c[,2]),]    
  }
########  
  # creates a matrix of data with dimensions labelled
  y <- rbind(c(0:28),x)
  
  print(y[1,])
  
  # prepares vector storage
  a <- cbind(c(1:size),rep(NA,size))
  b <- vector()
  #b <- rep(0,nrow(x))
  print(length(b))
  print(nrow(as.matrix(x)))
  for (i in 1:size)
  {
    
  # calls the sub wrapper
  z <- sub.wrapper(y[,], k=k, b, classifier)
  
  # removes dimension chosen by sub wrapper
  y <- y[,y[1,]!=z[1]]
  
  
  # stores best dimensions in a vector
  a[i,] <- z
  
  # adds dimension chosen to current matrix of chosen dimensions
  b <- cbind(b,x[,(z+1)[1]])
  }
  a 
}


find.k <- function(x) 
{
  c <- c(1:51)
  c <- cbind(c(1:51),seq(1,101,2))
  for (i in seq(1,101,2))
  {
    knncv <- my.ten.cv(x=cbind(as.matrix(data.df3[,1]),as.matrix(data.df3[,x + 1])),k=i,classifier="KNN")
    
    c[(i+1)/2,1]= knncv
  }
  c
}


# Run wrapper for different values of k. Establish that k = 21 produces the lowest error rates
wrapper.select <- matrix(NA,20,22)
for (i in seq(1,21,2))
{
a <- wrapper(data.df3, size = 20, k=i,classifier="KNN")
wrapper.select[,i] <- a[,1]
wrapper.select[,i+1] <- a[,2]
}
find.k(wrapper.select[1:16,21])
wrapper.KNN <- wrapper.select[,21:22]
plot(c(1:20),wrapper.KNN[,2],ylim=range(0,0.2))
title(xlab="Number of Dimensions used", 
     ylab ="Error Rate", 
     main="KNN error rate against number of dimensions used" , outer=TRUE)

wrapper.qda <- wrapper(data.df2, size = 20, classifier="QDA")
plot(c(1:20),wrapper.qda[,2],ylim=range(0,0.2))
title(xlab="Number of Dimensions used", 
      ylab ="Error Rate", 
      main="QDA error rate against number of dimensions used" , outer=TRUE)

wrapper.ld <- wrapper(data.df2, size =28, classifier="LD")
plot(c(1:28),wrapper.ld[,2],ylim=range(0,0.2))
title(xlab="Number of Dimensions used", 
      ylab ="Error Rate", 
      main="LD error rate against number of dimensions used" , outer=TRUE)

mlp.check <- wrapper(data.df4, size = 20, classifier ="MLP", k=1)
mlp.check
H <- c(1,3,5)
wd <- c(0.01,0.001,0.0001)
gr <- as.matrix(expand.grid(H,wd))


wrapper.MLP <- apply(gr,1,my.ten.cv, kay=3,classifier="MLP",x=cbind(data.df4[,1],data.df4[,mlp.check[1:9,1]+1]),folds=10)
plot(c(1:20),mlp.check[,2],ylim=range(0,0.2))
title(xlab="Number of Dimensions used", 
      ylab ="Error Rate", 
      main="MLP error rate against number of dimensions used" , outer=TRUE)

###### Cart

# Gini Tree
set.seed(1)
gini.tree <- rpart(as.factor(data.df[,1])~., data = data.df[,-1])
par(mfrow=c(1,1))
prp(gini.tree,extra=2, main="Gini Tree without pruning")
plotcp(gini.tree)
gini.tree$cptable
prune.gini.tree<- prune(gini.tree, 0.017 )
prp(prune.gini.tree,extra=2, main="Pruned Gini Tree")

# Information Tree
set.seed(1)
information.tree <- rpart(as.factor(data.df[,1])~., data = data.df[,-1],parms=list(split="information"))
prp(information.tree,extra=2, main="Entropy Tree without pruning")
plotcp(information.tree)
gini.tree$cptable
prune.information.tree<- prune(gini.tree, 0.039 )
prp(prune.information.tree,extra=2, main ="Pruned Entropy Tree")

# Establish Gini Tree is better than Information Tree

wrapper.CART <- my.ten.cv(x=data.df, kay=1, classifier="CART")

# ALL ERROR RATES
min(wrapper.ld[,2])
min(wrapper.MLP)
min(wrapper.KNN[,2])
min(wrapper.qda[,2])
wrapper.CART




################## McNemar Test 1% significance

"McNemar"<-function(x,y, true, sig = 0.01)
{
  x.wrong <- x != true
  y.wrong <- y != true
  n.1 <- sum((x.wrong == T) & (y.wrong==F))
  n.2 <- sum((y.wrong == T) & (x.wrong==F))
  z <- (abs(n.1 - n.2) - 1)/(sqrt(n.1+n.2))
  cr <- qnorm(1 - sig/2, 0, 1)
  list(z=z, cr = cr)
}
data.te.na <- data.te
for (i in 1:29)
{
  data.te.na[is.na(data.te.na[,i]), i] <- mean(data.df2[,i], na.rm = TRUE) # replace NAs with class mean
}

data.te.KNN <- cbind(data.te.na[,1],scale(data.te.na[,-1]))
data.te.MLP <- cbind(data.te.na[,1],apply(data.te.na[,2:29],2,function(a){(a-min(a))/max(a-min(a))}))

#KNN
knn.pred<- as.numeric( knn( data.df3[,wrapper.KNN[1:16,1]+1], data.te.KNN[,wrapper.KNN[1:16,1]+1], data.df3[,1],k=11 ))-1
#MLP
  data.df.MLP.2 <- as.data.frame(cbind(data.df4[,1],data.df4[,mlp.check[1:9,1]+1]))
  data.te.MLP.2 <- as.data.frame(cbind(data.te.KNN[,1],data.te.KNN[,mlp.check[1:9,1]+1]))
  names(data.te.MLP.2)[2] <- "1"
  names(data.te.MLP.2)[3] <- "2"
  names(data.te.MLP.2)[4] <- "3"
  names(data.te.MLP.2)[5] <- "4"
  names(data.te.MLP.2)[6] <- "5"
  names(data.te.MLP.2)[7] <- "6"
  names(data.te.MLP.2)[8] <- "7"
  names(data.te.MLP.2)[9] <- "8"
  names(data.te.MLP.2)[10] <- "9"

  names(data.df.MLP.2)[2] <- "1"
  names(data.df.MLP.2)[3] <- "2"
  names(data.df.MLP.2)[4] <- "3"
  names(data.df.MLP.2)[5] <- "4"
  names(data.df.MLP.2)[6] <- "5"
  names(data.df.MLP.2)[7] <- "6"
  names(data.df.MLP.2)[8] <- "7"
  names(data.df.MLP.2)[9] <- "8"
  names(data.df.MLP.2)[10] <- "9"

mlp <- nnet( as.factor(V1)~. , data = data.df.MLP.2, size = 3,decay = 0.001, maxit = 1000, trace=FALSE)
evals <- eigen( nnetHess(mlp, data.df.MLP.2[,-1], data.df.MLP.2[,1]), T)$values
print(min(evals))
pred.mlp <- round(predict(mlp, data.te.MLP.2[,-1]))

McNemar(knn.pred, pred.mlp, data.te[,1])











labels <- read.table("batches.meta.txt")
images.rgb <- list()
images.lab <- list()
num.images = 10000 # Set to 10000 to retrieve all images per file to memory

# Cycle through all 5 binary files
for (f in 1:5) {
  to.read <- file(paste("data_batch_", f, ".bin", sep=""), "rb")
  for(i in 1:num.images) {
    l <- readBin(to.read, integer(), size=1, n=1, endian="big")
    r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    index <- num.images * (f-1) + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.lab[[index]] = l+1
  }
  close(to.read)
  remove(l,r,g,b,f,i,index, to.read)
}

# function to run sanity check on photos & labels import
drawImage <- function(index) {
  # Testing the parsing: Convert each color layer into a matrix,
  # combine into an rgb object, and display as a plot
  img <- images.rgb[[index]]
  img.r.mat <- matrix(img$r, ncol=32, byrow = TRUE)
  img.g.mat <- matrix(img$g, ncol=32, byrow = TRUE)
  img.b.mat <- matrix(img$b, ncol=32, byrow = TRUE)
  img.col.mat <- rgb(img.r.mat, img.g.mat, img.b.mat, maxColorValue = 255)
  dim(img.col.mat) <- dim(img.r.mat)
  
  # Plot and output label
  library(grid)
  grid.raster(img.col.mat, interpolate=FALSE)
  
  # clean up
  remove(img, img.r.mat, img.g.mat, img.b.mat, img.col.mat)
  
  labels[[1]][images.lab[[index]]]
}

drawImage(sample(1:(num.images*5), size=1))


###### preparing labels########
labels<-as.data.frame(images.lab)
labels<-t(labels)

######seperating RGB######
r=matrix(nrow=50000,ncol=1024)
g=matrix(nrow=50000,ncol=1024)
b=matrix(nrow=50000,ncol=1024)
for (k in 1:50000){
  r[k,]=t(images.rgb[[k]][,1])
  g[k,]=t(images.rgb[[k]][,2])
  b[k,]=t(images.rgb[[k]][,3])
}

r<-as.data.frame(r)
g<-as.data.frame(g)
b<-as.data.frame(b)


#####PCA####

pca_r<-prcomp(r,scale=TRUE)
pca_g<-prcomp(g,scale=TRUE)
pca_b<-prcomp(b,scale=TRUE)

####SCREE Plots######

######variance of PCA######
sd_r <- pca_r$sdev
sd_g <- pca_g$sdev
sd_b <- pca_b$sdev
var_r <- sd_r^2
var_g <- sd_g^2
var_b <- sd_b^2

#proportion of variance explained
pvarex_r <- var_r/sum(var_b)
pvarex_g <- var_g/sum(var_g)
pvarex_b <- var_b/sum(var_b)


#cumulative scree plot
par(mfrow=c(1,3))
plot(cumsum(pvarex_r), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained (red)",
     type = "b", xlim=c(0,500))
plot(cumsum(pvarex_g), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained (green)",
     type = "b", xlim=c(0,500))
plot(cumsum(pvarex_b), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained (blue)",
     type = "b", xlim=c(0,500))


#####Tuning DA#######
err_lda<-list()
library(MASS)
for(k in 15:100){
  set.seed(10)
  da_r<-pca_r$x[,c(1:k)]
  da_g<-pca_g$x[,c(1:k)]
  da_b<-pca_b$x[,c(1:k)]
  for (i in 1:k){colnames(da_r)[i]<-paste('RPC',i,sep="")}
  for (i in 1:k){colnames(da_g)[i]<-paste('GPC',i,sep="")}
  for (i in 1:k){colnames(da_b)[i]<-paste('BPC',i,sep="")}

  da_size<-sample(50000,40000)
  train_size<-sample(40000,36000)
  da_tune<-as.data.frame(cbind(da_r[da_size,],da_g[da_size,],da_b[da_size,]))
  l_tune<-labels[da_size]
  ttrain_x<-da_tune[train_size,]
  ttrain_y<-l_tune[train_size]
  ttest_x<-da_tune[-train_size,]
  ttest_y<-l_tune[-train_size]

  lda_mod<-lda(ttrain_y~., data=ttrain_x, method="t",nu=100)
  pred_lda<-predict(lda_mod, newdata=ttest_x)
  err_lda[k]<-mean(pred_lda$class!=ttest_y)
}

#finalizing LDA Model for 46000 data with 64 pca####

set.seed(10)
da_rf<-pca_r$x[,c(1:64)]
da_gf<-pca_g$x[,c(1:64)]
da_bf<-pca_b$x[,c(1:64)]
for (i in 1:64){colnames(da_rf)[i]<-paste('RPC',i,sep="")}
for (i in 1:64){colnames(da_gf)[i]<-paste('GPC',i,sep="")}
for (i in 1:64){colnames(da_bf)[i]<-paste('BPC',i,sep="")}

set.seed(10)
da_sizef<-sample(50000,46000)
train_ldax<-as.data.frame(cbind(da_rf[da_sizef,],da_gf[da_sizef,],da_bf[da_sizef,]))
train_lday<-labels[da_sizef]
test_ldax<-as.data.frame(cbind(da_rf[-da_sizef,],da_gf[-da_sizef,],da_bf[-da_sizef,]))
test_lday<-labels[-da_sizef]


library(MASS)
Sys.time()
lda_f<- lda(train_lday~., data=train_ldax, method="t",nu=100)
Sys.time()
pred_ldaf<-predict(lda_f, newdata=test_ldax)
err_ldaf<-mean(pred_ldaf$class!=test_lday)

####finalizing QDA###
library(MASS)
Sys.time()
qda_f<- qda(train_lday~., data=train_ldax, method="t",nu=10)
Sys.time()
pred_qdaf<-predict(qda_f, newdata=test_ldax)
err_qdaf<-mean(pred_qdaf$class!=test_lday)

####Tuning SVM#####
set.seed(10)
sv_r<-pca_r$x[,c(1:75)]
sv_g<-pca_g$x[,c(1:75)]
sv_b<-pca_b$x[,c(1:75)]
for (i in 1:75){colnames(sv_r)[i]<-paste('RPC',i,sep="")}
for (i in 1:75){colnames(sv_g)[i]<-paste('GPC',i,sep="")}
for (i in 1:75){colnames(sv_b)[i]<-paste('BPC',i,sep="")}

sv_size<-sample(50000,10000)
svtrain_size<-sample(10000,9000)
sv_tune<-cbind(sv_r[sv_size,],sv_g[sv_size,],sv_b[sv_size,])
svl_tune<-labels[sv_size]
svttrain_x<-sv_tune[svtrain_size,]
svttrain_y<-svl_tune[svtrain_size]
svttest_x<-sv_tune[-svtrain_size,]
svttest_y<-svl_tune[-svtrain_size]
#### selection of kernel######
library(e1071)

##linear
svm_l<-svm(factor(svttrain_y)~svttrain_x,  kernel="linear",,cost=1000,gamma=1, type='C-classification',scale=TRUE)

pred_svm_l<-predict(svm_l, newdata=svttest_x)

err_rt_svm_l<-mean(pred_svm_l!=svttest_y)

###radial
svm_r<-svm(factor(svttrain_y)~svttrain_x,  kernel="radial",,cost=1000,gamma=1, type='C-classification',scale=TRUE)

pred_svm_r<-predict(svm_r, newdata=svttest_x)

err_rt_svm_r<-mean(pred_svm_r!=svttest_y)

####poly
svm_p<-svm(factor(svttrain_y)~svttrain_x,  kernel="polynomial",degree=3,cost=1000,gamma=1, type='C-classification',scale=TRUE)

pred_svm_p<-predict(svm_p, newdata=svttest_x)

err_rt_svm_p<-mean(pred_svm_p!=svttest_y)


#####Tuning RF######
set.seed(10)
rf_r<-pca_r$x[,c(1:75)]
rf_g<-pca_g$x[,c(1:75)]
rf_b<-pca_b$x[,c(1:75)]
for (i in 1:75){colnames(rf_r)[i]<-paste('RPC',i,sep="")}
for (i in 1:75){colnames(rf_g)[i]<-paste('GPC',i,sep="")}
for (i in 1:75){colnames(rf_b)[i]<-paste('BPC',i,sep="")}

rf_size<-sample(50000,2000)
rftrain_size<-sample(2000,1800)
rf_tune<-cbind(rf_r[rf_size,],rf_g[rf_size,],rf_b[rf_size,])
rfl_tune<-labels[rf_size]
rfttrain_x<-rf_tune[rftrain_size,]
rfttrain_y<-rfl_tune[rftrain_size]
rfttest_x<-rf_tune[-rftrain_size,]
rfttest_y<-rfl_tune[-rftrain_size]

####tuning mtry values#####
library(randomForest)
rf_err<-list()

for (i in 1:30){
  set.seed(1)
  rf_tune<-randomForest(factor(rfttrain_y)~., data=rfttrain_x, mtry=i, ntree=50)
  rf_tune_pred<-predict(rf_tune,newdata=rfttest_x)
  rf_err[i]<-mean(rf_tune_pred!=rfttest_y)
}



### tuning ntree value#####
rf_err2<-list()
k=1
for (i in c(50,100,150,200,250,300,350,400,450,500,550,600)){
  set.seed(1)
  rf_tune2<-randomForest(factor(rfttrain_y)~., data=rfttrain_x, mtry=18, ntree=i)
  rf_tune_pred2<-predict(rf_tune2,newdata=rfttest_x)
  rf_err2[k]<-mean(rf_tune_pred2!=rfttest_y)
  k=k+1
}

####building RF For ntree=300 and mtry=18 for 46000 data####

set.seed(10)
rf_rf<-pca_r$x[,c(1:75)]
rf_gf<-pca_g$x[,c(1:75)]
rf_bf<-pca_b$x[,c(1:75)]
for (i in 1:75){colnames(rf_rf)[i]<-paste('RPC',i,sep="")}
for (i in 1:75){colnames(rf_gf)[i]<-paste('GPC',i,sep="")}
for (i in 1:75){colnames(rf_bf)[i]<-paste('BPC',i,sep="")}
set.seed(10)
rf_sizef<-sample(50000,46000)
train_rfx<-as.data.frame(cbind(rf_rf[rf_sizef,],rf_gf[rf_sizef,],rf_bf[rf_sizef,]))
train_rfy<-labels[rf_sizef]
test_rfx<-as.data.frame(cbind(rf_rf[-rf_sizef,],rf_gf[-rf_sizef,],rf_bf[-rf_sizef,]))
test_rfy<-labels[-rf_sizef]

set.seed(10)
rf_final<-randomForest(factor(train_rfy)~., data=train_rfx, mtry=18, ntree=300)
rf_pred_f<-predict(rf_final,newdata=test_rfx)
rf_err_f<-mean(rf_pred_f!=test_rfy)

####cross Validation on RF######
library(randomForest)
library(rfUtilities)
train_x_full<-as.data.frame(cbind(rf_rf[,],rf_gf[,],rf_bf[,]))
Sys.time()
set.seed(10)
rf_mod<-randomForest(factor(labels)~., data=train_x_full, mtry=18, ntree=300)
Sys.time()
Sys.time()
rf.cv <- rf.crossValidation(rf_mod, train_x_full, p=0.10, n=10, ntree=300, seed=10)
Sys.time()

#####test batch on rf###


######test Batch######
timages.rgb <- list()
timages.lab <- list()
to.read <- file("test_batch.bin", "rb")
for(i in 1:num.images) {
  l <- readBin(to.read, integer(), size=1, n=1, endian="big")
  r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
  g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
  b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
  index <- i
  timages.rgb[[index]] = data.frame(r, g, b)
  timages.lab[[index]] = l+1
}
close(to.read)
remove(l,r,g,b,i,index, to.read)


tlabels<-as.data.frame(timages.lab)
tlabels<-t(tlabels)
#####test batch data prep#####
tdf_r=matrix(nrow=10000,ncol=1024)
tdf_g=matrix(nrow=10000,ncol=1024)
tdf_b=matrix(nrow=10000,ncol=1024)
for (k in 1:10000){
  tdf_r[k,]=t(timages.rgb[[k]][,1])
  tdf_g[k,]=t(timages.rgb[[k]][,2])
  tdf_b[k,]=t(timages.rgb[[k]][,3])
}

tdf_r<-as.data.frame(tdf_r)
tdf_g<-as.data.frame(tdf_g)
tdf_b<-as.data.frame(tdf_b)

####Transfroming test data for prediction#####
ttpca_r<-predict(pca_r, newdata=tdf_r)
ttpca_g<-predict(pca_g, newdata=tdf_g)
ttpca_b<-predict(pca_b, newdata=tdf_b)

set.seed(10)
trf_r<-ttpca_r[,c(1:30)]
trf_g<-ttpca_g[,c(1:30)]
trf_b<-ttpca_b[,c(1:30)]
for (i in 1:30){colnames(trf_r)[i]<-paste('RPC',i,sep="")}
for (i in 1:30){colnames(trf_g)[i]<-paste('GPC',i,sep="")}
for (i in 1:30){colnames(trf_b)[i]<-paste('BPC',i,sep="")}

test_batch<-cbind(trf_r,trf_g,trf_b)

test_batch_pred<-predict(rf_mod, newdata=test_batch)

acc_rf=mean(test_batch_pred==tlabels)




######CV for LDA#####
library(caret)

train_x_lda<-cbind(train_x_full,labels)
test_batch_lda<-cbind(test_batch, tlabels)
train_control <- trainControl(method="cv", number=10)
lda.fit = train(factor(labels) ~ ., data=train_x_lda, method="lda",
                trControl = trainControl(method = "cv"))
lda.pred<-predict(lda.fit,newdata=test_batch_lda)
lda.acc=mean(lda.pred==tlabels)

####tuning QDA for no. of PCA#####
qda.fit.acc<-list()
for (f in c(3,4,5,6,7,8,9,10)){
  rpc<-pca_r$x[,c(1:f)]
  gpc<-pca_g$x[,c(1:f)]
  bpc<-pca_b$x[,c(1:f)]
  for (i in 1:f){colnames(rpc)[i]<-paste('RPC',i,sep="")}
  for (i in 1:f){colnames(gpc)[i]<-paste('GPC',i,sep="")}
  for (i in 1:f){colnames(bpc)[i]<-paste('BPC',i,sep="")}

  t_x<-as.data.frame(cbind(rpc[,],gpc[,],bpc[,]))
  t_x<-cbind(t_x,labels)
  qda.fit = train(factor(labels) ~ ., data=t_x, method="qda",
                trControl = trainControl(method = "cv"))
  qda.fit.acc[f]<-qda.fit$results$Accuracy
}


rpc<-pca_r$x[,c(1:30)]
gpc<-pca_g$x[,c(1:30)]
bpc<-pca_b$x[,c(1:30)]
for (i in 1:30){colnames(rpc)[i]<-paste('RPC',i,sep="")}
for (i in 1:30){colnames(gpc)[i]<-paste('GPC',i,sep="")}
for (i in 1:30){colnames(bpc)[i]<-paste('BPC',i,sep="")}
t_x<-as.data.frame(cbind(rpc[,],gpc[,],bpc[,]))
t_x<-cbind(t_x,labels)

Sys.time()
qda.fit.final = train(factor(labels) ~ .-labels, data=t_x, method="qda",
                trControl = trainControl(method = "cv"))
Sys.time()

qda_final_pred<-predict(qda.fit.final,newdata=test_batch_lda)
err_final_qda<-mean(qda_final_pred!=tlabels)

library(pROC)
qqq<-cbind(test_batch,tlabels)
p<- predict(qda.fit.final,newdata=qqq)


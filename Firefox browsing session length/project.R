setwd("C:/users/bllguo/dropbox/penn_fall_15-16/stat431/final")
data<-read.csv("book.csv",header=TRUE);
data<-data[-30,];
require(arm)
names(data)<-c("id","btime","windows","tabs","bookmarks","npriv","ndls","ext","acts")
# bound <- floor(nrow(data)/2)
# data.train<-data[1:bound,]
# data.test<-data[(bound+1):nrow(data), ]
# 
# set.seed(123)
# data_ind <- sample(seq_len(nrow(data)), size = bound)
# 
# data.train <- data[data_ind, ]
# data.test <- data[-data_ind, ]
data.train<-data
data.test<-data[,2:ncol(data)]
data.test[,8]<-log(.1+data.test[,8])
names(data.test)<-c("btime","n_windows","n_tabs","n_bookmarks","priv","dls","ext","log(acts+.1)")
pairs(data.test)

names(data.train)<-c("id","btime","n_windows","n_tabs","n_bookmarks","priv","dls","ext","acts")


btime<-data.train[,2];
windows<-data.train[,3];
tabs<-data.train[,4];
bookmarks<-data.train[,5];
priv<-data.train[,6];
dls<-data.train[,7];
ext<-data.train[,8];
acts<-data.train[,9];

# check distribution of response
hist(btime)
# negative skew - may want to transform
hist(windows)
# some negative skew
hist(tabs)
# some negative skew
hist(bookmarks)
# some negative skew
hist(priv)
# extreme positive outliers present
hist(dls)
# extreme positive outliers present
hist(ext)
# some negative skew
hist(acts)

plot(windows,log(btime),main="n_windows v. log(btime)",xlab="n_windows")
plot(tabs,log(btime),main="n_tabs v. log(btime)",xlab="n_tabs")
plot(bookmarks,log(btime),main="n_bookmarks v. log(btime)",xlab="n_bookmarks")
plot(log(acts+.1),log(btime),main="acts v. log(btime)",xlab="acts")
plot(log(dls+.1),log(btime),main="dls v. log(btime)",xlab="dls")


require(geoR)
bcox <- function(x,l1,l2) {
  y <- vector(mode="numeric",length=length(x))
  for(i in 1:length(x)) {
    if(x[i]!=0) {
      y[i]<-((x[i]+l2)^l1 - 1)/l1
    }
    else {
      y[i]<-log(l2)
    }
  }
  return(y)
}

cube <- function(x) {
  y <- vector(mode="numeric",length=length(x))
  for(i in 1:length(x)) {
    y[i]<-x[i]^(1/3.5)
  }
  return(y)
}

l1 <- as.numeric(boxcoxfit(acts,lambda2=TRUE)$lambda[1])
l2 <- as.numeric(boxcoxfit(acts,lambda2=TRUE)$lambda[2])

x<-3
y<-3
z<-3

fit0 <- lm(log(btime) ~ 1,data=data.train)
fit1 <- lm(sqrt(btime) ~ 1,data=data.train)
fit2 <- lm((btime)^(1/3) ~ 1, data=data.train)
fitall <- lm(log(btime) ~ poly((n_windows),degree=x,raw=TRUE) + 
               poly((n_tabs),degree=y,raw=TRUE) + 
               poly((n_bookmarks),degree=z,raw=TRUE) + 
               priv + dls + ext + log(acts+.1), data=data.train)
fittest <- lm(log(btime) ~ n_windows + n_tabs + n_bookmarks + 
               priv + dls + ext + log(acts+.1), data=data.train)

# # forward selection
# data.fs <- step(fit0, scope = list(lower = ~ 1,
#                                    upper = ~ poly((windows),degree=x,raw=TRUE) + 
#                                      poly((tabs),degree=y,raw=TRUE) + 
#                                      poly((bookmarks),degree=z,raw=TRUE) + 
#                                      priv + dls + ext + sqrt(acts)),
#                 direction = "forward", data = data)
# 
# # backward elimination
# data.be <- step(fitall, scope = list(lower = ~ 1,
#                                      upper = ~ poly((windows),degree=x,raw=TRUE) + 
#                                        poly((tabs),degree=y,raw=TRUE) + 
#                                        poly((bookmarks),degree=z,raw=TRUE) + 
#                                        priv + dls + ext + sqrt(acts)),
#                 direction = "backward", data = data)
# 
# stepwise regression
data.st <- step(fit0, scope = list(lower = ~ 1,
                                   upper = ~ poly((n_windows),degree=x,raw=TRUE) + 
                                     poly((n_tabs),degree=y,raw=TRUE) + 
                                     poly((n_bookmarks),degree=z,raw=TRUE) + 
                                     priv + dls + ext + log(acts+.1)),
                direction = "both", data = data.train)

data.st1 <- step(fit1, scope = list(lower = ~ 1,
                                    upper = ~ poly((n_windows),degree=x,raw=TRUE) + 
                                      poly((n_tabs),degree=y,raw=TRUE) + 
                                      poly((n_bookmarks),degree=z,raw=TRUE) + 
                                      priv + dls + ext + log(acts+.1)),
                direction = "both", data = data.train)

data.st2 <- step(fit2, scope = list(lower = ~ 1,
                                    upper = ~ poly((n_windows),degree=x,raw=TRUE) + 
                                      poly((n_tabs),degree=y,raw=TRUE) + 
                                      poly((n_bookmarks),degree=z,raw=TRUE) + 
                                      priv + dls + ext + log(acts+.1)),
                 direction = "both", data = data.train)

data.sttest <- step(fittest, scope = list(lower = ~ 1,
                                    upper = ~ n_windows + n_tabs + n_bookmarks + 
                                      priv + dls + ext + log(acts+.1)),
                 direction = "both", data = data.train)

require(leaps)

data.subset <- regsubsets(log(btime) ~ poly((n_windows),degree=x,raw=TRUE) + 
                            poly((n_tabs),degree=y,raw=TRUE) + 
                            poly((n_bookmarks),degree=z,raw=TRUE) + 
                            priv + dls + ext + log(acts+.1), data=data.train)
# check the best model for each given subset size
datab <- summary(data.subset)
datab
# select the final model based on Cp
datab$cp
# select the final model based on BIC
datab$bic

data.subsettest <- regsubsets(log(btime) ~ n_windows + n_tabs + n_bookmarks + 
                                priv + dls + ext + log(acts+.1), data=data.train)

#best model
fin <- lm(log(btime) ~ 1 + log(acts+.1) + log(dls+.1) + poly(n_windows,degree=x,raw=TRUE),data = data.train)
# finmodel1 <- lm(btime^(1/2) ~ 1 + log1p(acts) + poly(windows,degree=x,raw=TRUE),data = data)
# finmodel2 <- lm(btime^(1/3) ~ 1 + sqrt(acts) + poly(windows,degree=x,raw=TRUE),data = data)

finmodel <- fin
finmodel <- lm(log(btime) ~ poly(n_windows,degree = 2, raw=TRUE) + log(n_tabs) + log(acts+.1), data=data.train)
summary(finmodel)
# finmodel <- lm(log(btime) ~ n_tabs + log(acts+.1), data=data.train)
# summary(finmodel)
# finmodel <- lm(log(btime) ~ n_windows + n_tabs + n_bookmarks + log(acts+.1), data=data.train)
# summary(finmodel)

#check heteroscedasticity
plot(fitted(finmodel), residuals(finmodel), xlab = "Model 1 Fitted values", ylab = "Model 1 Residuals")
abline(h=0, lwd=2)
# plot(fitted(finmodel1), residuals(finmodel1))
# abline(h=0, lwd=2)
# plot(fitted(finmodel2), residuals(finmodel2))
# abline(h=0, lwd=2)
#checks out well enough

require(car)
residualPlots(finmodel)
# residualPlots(finmodel1)
# residualPlots(finmodel2)

qqnorm(scale(residuals(finmodel)))
abline(0,1)
# qqnorm(scale(residuals(finmodel1)))
# abline(0,1)
# qqnorm(scale(residuals(finmodel2)))
# abline(0,1)

plot(hatvalues(finmodel), cex=.5, main="Model 1.1 Hat Values")
abline(h=2*length(coef(finmodel))/nrow(data.train), col=2)

plot(cooks.distance(finmodel), cex=.5, main="Model 1.1 Cook's Distance")

vif(finmodel)

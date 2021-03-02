#Exercise 1 Missing data
library(readr)
datstu <- read.table("~/Econ613-master/Econ613-master/Assignments/A1/dat/datstu.csv", head= TRUE, sep=",", na.strings=c("","NA","99"))
nrow(datstu)
# There are 340823 students

#Number of schools
code1 <- unique(datstu$schoolcode1)
code2 <- unique(datstu$schoolcode2)
code3 <- unique(datstu$schoolcode3)
code4 <- unique(datstu$schoolcode4)
code5 <- unique(datstu$schoolcode5)
code6 <- unique(datstu$schoolcode6)
code_combined <- c(code1,code2,code3,code4,code5,code6)
length(unique(code_combined))
#641 unique schools

#Number of programs
code1 <- unique(datstu$choicepgm1)
code2 <- unique(datstu$choicepgm2)
code3 <- unique(datstu$choicepgm3)
code4 <- unique(datstu$choicepgm4)
code5 <- unique(datstu$choicepgm5)
code6 <- unique(datstu$choicepgm6)
code_combined <- c(code1,code2,code3,code4,code5,code6)
length(unique(code_combined))
#33 unique programs

#Number of choices (school,program)
library("data.table", lib.loc="~/R/win-library/4.0")
datstu2 <- subset(datstu, select = c(choicepgm1, schoolcode1))
setDT(datstu2)[,list(Count=.N) , names(datstu2)]

#there are 2545 number of choices


#Missing test score
sum(is.na(datstu$score))
#According to R, there are 179887 observations missing


#Apply to the same school (different programs)
datstu$same <- NA
datstu$same <- ifelse(datstu$schoolcode1 == datstu$schoolcode2,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode1 == datstu$schoolcode3,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode1 == datstu$schoolcode4,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode1 == datstu$schoolcode5,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode1 == datstu$schoolcode6,1, datstu$same)

datstu$same <- ifelse(datstu$schoolcode2 == datstu$schoolcode1,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode2 == datstu$schoolcode3,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode2 == datstu$schoolcode4,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode2 == datstu$schoolcode5,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode2 == datstu$schoolcode6,1, datstu$same)

datstu$same <- ifelse(datstu$schoolcode3 == datstu$schoolcode2,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode3 == datstu$schoolcode1,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode3 == datstu$schoolcode4,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode3 == datstu$schoolcode5,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode3 == datstu$schoolcode6,1, datstu$same)

datstu$same <- ifelse(datstu$schoolcode4 == datstu$schoolcode2,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode4 == datstu$schoolcode3,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode4 == datstu$schoolcode1,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode4 == datstu$schoolcode5,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode4 == datstu$schoolcode6,1, datstu$same)

datstu$same <- ifelse(datstu$schoolcode5 == datstu$schoolcode2,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode5 == datstu$schoolcode3,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode5 == datstu$schoolcode4,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode5 == datstu$schoolcode1,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode5 == datstu$schoolcode6,1, datstu$same)

datstu$same <- ifelse(datstu$schoolcode6 == datstu$schoolcode2,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode6 == datstu$schoolcode3,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode6 == datstu$schoolcode4,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode6 == datstu$schoolcode5,1, datstu$same)
datstu$same <- ifelse(datstu$schoolcode6 == datstu$schoolcode1,1, datstu$same)
table(datstu$same)
#116430 apply to the same school
#Apply to less than 6 choices

datstu2 <- subset(datstu, is.na(datstu$choicepgm6))
#18954 students applied to less than 6 schools

#Exercise 2
library(readr)
library("reshape", lib.loc="~/R/win-library/4.0")
library("reshape2", lib.loc="~/R/win-library/4.0")
datsss <- read_csv("~/Econ613-master/Econ613-master/Assignments/A1/dat/datsss.csv")
#Create a school level dataset, where each row corresponds to a (school,program) with the following variables:

# the district where the school is located
# the latitude of the district
# the longitude of the district
# cutoff (the lowest score to be admitted)
# quality (the average score of the students admitted)
# size (number of students admitted)

#creating dataset by joining ranked choices
datstu_f <- na.omit(datstu)
first_choice <- datstu_f[which(datstu_f$rankplace == 1), ]
first_choice <- first_choice[ , c(-6,-7,-8,-9,-10,-12,-13,-14,-15,-16)]
names(first_choice) [5:6] <- c("schoolcode","choicepgm")
Second_choice <- datstu_f[which(datstu_f$rankplace == 2), ]
Second_choice <- Second_choice[ , c(-5,-7,-8,-9,-10,-11,-13,-14,-15,-16)]
names(Second_choice) [5:6]<-c("schoolcode","choicepgm")
Third_choice <- datstu_f[which(datstu_f$rankplace == 3), ]
Third_choice <- Third_choice[ , c(-5,-6,-8,-9,-10,-11,-12,-14,-15,-16)]
names(Third_choice) [5:6]<-c("schoolcode","choicepgm")
Fourth_choice <- datstu_f[which(datstu_f$rankplace == 4), ]
Fourth_choice <- Fourth_choice[ , c(-5,-6,-7,-9,-10,-11,-12,-13,-15,-16)]
names(Fourth_choice) [5:6] <- c("schoolcode","choicepgm")
Fifth_choice <- datstu_f[which(datstu_f$rankplace == 5), ]
Fifth_choice <- Fifth_choice[ , c(-5,-6,-7,-8,-10,-11,-12,-13,-14,-16)]
names(Fifth_choice) [5:6]<-c("schoolcode","choicepgm")
Sixth_choice <- datstu_f[which(datstu_f$rankplace == 6), ]
Sixth_choice <- Sixth_choice[ , c(-5,-6,-7,-8,-9,-11,-12,-13,-14,-15)]
names(Sixth_choice) [5:6]<-c("schoolcode","choicepgm")
Combined_choices <- rbind(first_choice,Second_choice,Third_choice,Fourth_choice, Fifth_choice,Sixth_choice)
Final_choices <- Combined_choices[order(Combined_choices$schoolcode,Combined_choices$choicepgm, Combined_choices$score),]

#get final entrance ranking
Final_choices$choice<- paste0(Final_choices$schoolcode,Final_choices$choicepgm)
entrance_ranking<- Final_choices[ , c(2,9)]
entrance_bottom <- melt(entrance_ranking, id="choice")
head(entrance_bottom)
cutoff_bot<- cast(entrance_bottom, choice~variable, min)
cutoff_mid<- cast(entrance_bottom, choice~variable, mean)
cutoff_top<- cast(entrance_bottom, choice~variable, length)
cutoff_joint<- cbind(cutoff, quality, size)
cutoff_joint_combined<- cutoff_joint[ ,c(-3,-5)]


# Rename variables
names(cutoff_joint_combined)[2:4]<-c("cutoff_Low to Top","quality_Average Score", "size_Number of Admission")

# Merging 
total_dt<- merge(Final_choices, cutoff_joint_combined, by="choice")
Result_Merge_View <- merge(total_dt, datasss, by="schoolcode")
Final_dt<- unique(merge(total_dt, datasss, by="schoolcode"))
head(Final_dt, 20)

#Exercise 3 

#where ssslong and ssslat are the coordinates of the district of the school (students apply to), while
#jsslong and jsslat are the coordinates of the junior high school, calculate the distance between junior
#high school, and senior high school.

library(readr)
datjss <- read_csv("~/Econ613-master/Econ613-master/Assignments/A1/dat/datjss.csv")
datsss <- read_csv("~/Econ613-master/Econ613-master/Assignments/A1/dat/datsss.csv")

datjss$district <- datjss$jssdistrict
datsss$district <- datsss$sssdistrict

library(dplyr)


# use left_join
df3 <- left_join(datjss, datsss, by = "district")
df3$dist <- sqrt(69.172*(df3$ssslong - df3$point_x)*cos(df3$point_y/57.3))^2 + (69.172*(df3$ssslat - df3$point_y))
head(df3$dist)

#exercise 4
datstu <- subset(datstu, !is.na(datstu$rankplace))
datstu_1st <- subset(datstu, datstu$rankplace == "1")
datstu_1st %>%
  group_by(choicepgm1) %>%
  sort(datstu$score)

#Exercise 5 Data Creation
set.seed(123)
# #After setting a seed, construct the following objects
# . X1: vector of 10,000 draws from a uniform distribution with range 1:3.
# . X2: vector of 10,000 draws from a gamma distribution with shape 3 and scale 2
# . X3: vector of 10,000 draws from a binomial distribution (one trial) with probability 0.3
# . e: vector of 10,000 draws from a normal distribution with mean 2 and sd 1.

x1 <- runif(10000,1,3)
x2 <- rgamma(10000,3, scale = 2)
x3 <- rbinom(10000,1,.3)
e <- rnorm(10000,2,1)

# Create the variables
# . Y = 0.5 + 1.2X1 ??? 0.9X2 + 0.1X3 + e
# . ydum =
#   ???
# ?????????
# ?????????
# 1, if Y > Y¯
# 0, otherwise

y = .5 + 1.2*x1 - .9*x2 + .1*x3 + e
ydum <- ifelse(y > mean(y),1,0)




#Exercise 6 OLS

# . Calculate the correlation between Y and X1. How different is it from 1.2?
i <- 1:10000

a <- sum( (x1[i] - mean(x1) ) * y[i] - mean(y) ) 

  
b <- sum( (x1[i] - mean(x1) )^2* ( (y[i] - mean(y) ))^2 )

corr <- a/b
corr
#The correlation is ~ 1 point away from 1.2

# . We are interested in the outcome of the regression of Y on X where X = (1, X1, X2, X3).
# . Calculate the coefficients on this regression.
XI <- cbind(1,x1,x2,x3)
solve( (t(XI)%*%XI) ) %*% t(XI)%*%y

# . Calculate the standard errors using the standard formulas of the OLS
SD_x1 <- sqrt(sum((x1-mean(x1))^2/(length(x1))))
SE_X1 <- SD_x1 / sqrt(length(x1))
SD_x1
SE_X1


SD_x2 <- sqrt(sum((x2-mean(x2))^2/(length(x2))))
SE_X2 <- SD_x2 / sqrt(length(x2))
SD_x2
SE_X2


SD_x3 <- sqrt(sum((x3-mean(x3))^2/(length(x3))))
SE_X3 <-SD_x3 / sqrt(length(x3))
SD_x3
SE_X3

# Exercise 7 Discrete choice
# We consider the determinants of ydum.
# . Write and optimize the probit, logit, and the linear probability model. You can use pre-programmed
# optimization packages.
#Probit
flike = function ( par, x1,x2,x3,ydum)
{
  xbeta = par[1] + par[2]*x1 + par[3]*x2 + par[4] *x3
  pr = pnorm(xbeta)
  pr[pr > 0.999999] = 0.999999
  pr[pr < 0.0000001] = 0.0000001
  like = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(like))
}


glm_prob <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "probit"))

test_par = glm_prob$coefficients
flike(test_par,x1,x2,x3,ydum)
logLik(glm_prob)


ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum)
  out[i0,] = res$par
}


start = runif(4)
res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum,hessian=TRUE)

fisher_info = solve(res$hessian)       
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma

est = cbind(res$par,prop_sigma)
colnames(est) = c("R: own : est","R: own :se")
est

#Logit
flike = function ( par, x1,x2,x3,ydum)
{
  xbeta = par[1] + par[2]*x1 + par[3]*x2 + par[4] *x3
  pr = exp(xbeta)/(1+exp(xbeta))
  pr[pr > 0.999999] = 0.999999
  pr[pr < 0.0000001] = 0.0000001
  like = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(like))
}


glm_log <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "logit"))

test_par = glm_log$coefficients
flike(test_par,x1,x2,x3,ydum)
logLik(glm_log)


ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=ydum)
  out[i0,] = res$par
}


start = runif(4)
res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=yvar,hessian=TRUE)

fisher_info = solve(res$hessian)       
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma

est = cbind(res$par,prop_sigma)
colnames(est) = c("R: own : est","R: own :se")
est



flike = function ( par, x1,x2,x3,ydum)
{
  xbeta = par[1] + par[2]*x1 + par[3]*x2 + par[4] *x3
  n = 10000
  pr = exp(xbeta)/(1+exp(xbeta))
  pr[pr > 0.999999] = 0.999999
  pr[pr < 0.0000001] = 0.0000001
  like = - n/2 * log(var(y)) - (1/2*var(y)) * t(y - xbeta)*(y-xbeta) 
  return(-sum(like))
}


lm_lpm <- lm( yvar ~ x1 + x2 + x3)

test_par = lm_lpm$coefficients
flike(test_par,x1,x2,x3,yvar)
logLik(lm_lpm)


ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=yvar)
  out[i0,] = res$par
}


start = runif(4)
res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,ydum=yvar,hessian=TRUE)

fisher_info = solve(res$hessian)       
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma

est = cbind(res$par,prop_sigma)
colnames(est) = c("R: own : est","R: own :se")
est



# . Interpret and compare the estimated coefficients. How significant are they?
#logit and probit very similar, lpm incosistent and changes significantly depending on the optimization algorithm
#probit


prob1_nested <- glm( ydum ~  x2 + x3, family = binomial(link = "probit"))
prob1_complex <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "probit"))
A <- logLik(prob1_nested)
B <- logLik(prob1_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val1 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#highly significant

prob2_nested <- glm( ydum ~  x1 + x3, family = binomial(link = "probit"))
prob2_complex <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "probit"))
A <- logLik(prob2_nested)
B <- logLik(prob2_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val2 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#highly significant



prob3_nested <- glm( ydum ~  x1 +  x2, family = binomial(link = "probit"))
prob3_complex <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "probit"))
A <- logLik(prob3_nested)
B <- logLik(prob3_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val3 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#non significant



#logit
log1_nested <- glm( ydum ~  x2 + x3, family = binomial(link = "logit"))
log1_complex <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "logit"))
A <- logLik(log1_nested)
B <- logLik(log1_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val4 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#highly significant

log2_nested <- glm( ydum ~  x1 + x3, family = binomial(link = "logit"))
log2_complex <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "logit"))
A <- logLik(log2_nested)
B <- logLik(log2_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val5 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#highly significant



log3_nested <- glm( ydum ~  x1 +  x2, family = binomial(link = "logit"))
log3_complex <- glm( ydum ~ x1 + x2 + x3, family = binomial(link = "logit"))
A <- logLik(log3_nested)
B <- logLik(log3_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val6 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#non significant

#lpm
lpm1_nested <- lm( ydum ~  x2 + x3)
lpm1_complex <- glm( ydum ~ x1 + x2 + x3)
A <- logLik(lpm1_nested)
B <- logLik(lpm1_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val7 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#highly significant

lpm2_nested <- lm( ydum ~  x1 + x3)
lpm2_complex <- glm( ydum ~ x1 + x2 + x3)
A <- logLik(lpm2_nested)
B <- logLik(lpm2_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val7 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#highly significant



lpm3_nested <- lm( ydum ~  x1 + x2)
lpm3_complex <- glm( ydum ~ x1 + x2 + x3)
A <- logLik(lpm3_nested)
B <- logLik(lpm3_complex)
teststat <- -2 * (as.numeric(A)-as.numeric(B))
p.val7 <- pchisq(teststat, df = 1, lower.tail = FALSE)
#non significant



# Exercise 8 Marginal Effects
# We consider the determinants of ydum.
# . Compute the marginal effect of X on Y according to the probit and logit models.
# . Compute the standard error of the marginal effects.
dataset <- cbind(1,x1,x2,x3,y)
dataset <- as.data.frame(dataset)

mfxboot <- function(modform,dist,data,boot=1000,digits=3){
  x <- glm(modform, family=binomial(link=dist),data)
  # get marginal effects
  pdf <- ifelse(dist=="probit",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  # start bootstrap
  bootvals <- matrix(rep(NA,boot*length(coef(x))), nrow=boot)
  for(i in 1:boot){
    samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
    x1 <- glm(modform, family=binomial(link=dist),samp1)
    pdf1 <- ifelse(dist=="probit",
                   mean(dnorm(predict(x, type = "link"))),
                   mean(dlogis(predict(x, type = "link"))))
    bootvals[i,] <- pdf1*coef(x1)
  }
  res <- cbind(marginal.effects,apply(bootvals,2,sd),marginal.effects/apply(bootvals,2,sd))
  if(names(x$coefficients[1])=="(Intercept)"){
    res1 <- res[2:nrow(res),]
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res1)),nrow=dim(res1)[1])     
    rownames(res2) <- rownames(res1)
  } else {
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep="")),nrow=dim(res)[1]))
    rownames(res2) <- rownames(res)
  }
  colnames(res2) <- c("marginal.effect","standard.error","z.ratio")  
  return(res2)
}


probit <- mfxboot(ydum ~ x1 + x2 + x3,"probit",dataset)
mfxdat <- data.frame(cbind(rownames(probit),probit))
mfxdat$me <- as.numeric(as.character(mfxdat$marginal.effect))
mfxdat$se <- as.numeric(as.character(mfxdat$standard.error))
mfxdat$z.ratio <- NULL
mfxdat


logit <- mfxboot(ydum ~ x1 + x2 + x3,"logit",dataset)
mfxdat <- data.frame(cbind(rownames(logit),logit))
mfxdat$me <- as.numeric(as.character(mfxdat$marginal.effect))
mfxdat$se <- as.numeric(as.character(mfxdat$standard.error))
mfxdat$z.ratio <- NULL
mfxdat

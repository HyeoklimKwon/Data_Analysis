library(quadprog)
dat = read.csv("C:/Users/khlzz/OneDrive/문서/SKKU/datasets/Stock_Bond.csv", header = T)
prices = cbind(dat$GM_AC, dat$F_AC, dat$CAT_AC, dat$UTX_AC,
               dat$MRK_AC, dat$IBM_AC)
n = dim(prices)[1]
returns = 100 * (prices[2:n, ] / prices[1:(n-1), ] - 1) #이율의 퍼센트화

pairs(returns)
mean_vect = colMeans(returns)

cov_mat = cov(returns)
cov_mat
sd_vect = sqrt(diag(cov_mat))
sd_vect
Amat = cbind(rep(1,6),mean_vect,diag(1,nrow=6))  # set the constraints matrix
Amat
muP = seq(min(mean_vect)+.0001,max(mean_vect)-.0001,length=300)
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=6) 
for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
  bvec = c(1,muP[i],rep(0,6))
  result = 
    solve.QP(Dmat=2*cov_mat,dvec=rep(0,6),Amat=Amat,bvec=bvec,meq=2)
  sdP[i] = sqrt(result$value)
  weights[i,] = result$solution
}
sdP
muP
par(mfrow = c(1,1))
plot(sdP,muP,type="l",xlim=c(0.001,3.0),ylim=c(0.001,0.1),lty=3, lwd = .5)
weights[225,]
max(weights[225,])
min(weights[225,])
mufree = 3.0/253 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # 기울기compute Sharpe's ratios
ind = (sharpe == max(sharpe)) # 기울기가 최대 ind maximum Sharpe's ratio
max(sharpe)
ind
options(digits=3)
weights[ind,]
lines(c(0,2),mufree+c(0,2)*(muP[ind]-mufree)/sdP[ind],lwd=4,lty=1, col = "blue")
points(sdP[ind],muP[ind],cex=4,pch="*")

A <- matrix(nrow=3, ncol = 3,c(2,-1,0,-1,2,-1,0,-1,2))
A
eigen(A)
eigen(A)$vectors
install.packages("tidyverse")

#mutate() 새로운 변수를 만들어서 기존에 있는 변수의 함수로 만든다 ex) v1+v2 = v5
#seclect() filter() 특정 값이 포함된 행 뺍기 summarize() 변수 요약 arrange() 행들을 특정한 기준에 따라 순서 바꾸기
#
arrange(flights, year, month, day)

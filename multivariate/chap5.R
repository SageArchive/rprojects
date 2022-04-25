# 5장. 다차원척도법(MDS)

# 자료 읽기
auto = read.csv("c:/data/mva/auto.csv")
head(auto)

# 표준화 변수 만들기
X = auto[,-1] # auto 데이터의 1열은 변수이름이므로 제외
autoName = auto[,1]
# z-standardization = (X-X-bar)/S
zX = scale(X, center=TRUE, scale=TRUE) # zX는 평균은 0이고 분산이 1인 표준화 변수
# 0-1 transformation = (Xi-minXi)/(maxXi-minXi)
maxX = apply(X, 2, max) # apply 함수 안 2는 변수는 의미(1은 케이스를 의미)
minX = apply(X, 2, min)
z01X = scale(X, center=minX, scale=maxX - minX) # z01X는 최소값이 0이고 최대값이 1인 변수

# 거리행렬 만들기
z01X_dist = dist(z01X, method='euclidean')
z01X_dist = as.matrix(z01X_dist)
colnames(z01X_dist) = autoName
rownames(z01X_dist) = autoName
z01X_dist

# cmdscale 실행
mds1 = cmdscale(z01X_dist, k=2) # 2차원(k=2)으로 적합
plot(mds1[,1], mds1[,2], type='n', xlab='', ylab='', main='cmdscale(Auto)')
text(mds1[,1], mds1[,2], rownames(z01X_dist), cex=0.9)
abline(h=0, v=0, lty=2) # 가로, 세로 0인 지점을 점선(lty=2)으로 표시

# smacof(stress minimization using majorization algorithm) mds 실행 예
install.packages("smacof")
library(smacof)
mds2 = mds(z01X_dist, ndim=2)
names(mds2)
plot(mds2$conf[,1], mds2$conf[,2], type='n', xlab='', ylab='', main="smacof(Auto)")
text(mds2$conf[,1], mds2$conf[,2], rownames(z01X_dist), cex=0.9)
abline(h=0, v=0, lty=2)
mds2$stress # 스트레스 값이 0.0259로 적합도가 매우 좋음.

# 스크리 그림 그리기: 차원을 늘려가면서 스트레스 값 표시
mds2_1 = mds(z01X_dist, ndim=1)
mds2_2 = mds(z01X_dist, ndim=2)
mds2_3 = mds(z01X_dist, ndim=3)
mds2_4 = mds(z01X_dist, ndim=4)
stress = c(mds2_1$stress, mds2_2$stress, mds2_3$stress, mds2_4$stress)
plot(stress, type="l")
points(stress, cex=0.9)

# 비메트릭 MDS 분석
readMatrix <- function(datam, nrows, cname, lower=1)
{
  # lower=1 : Lower Triangular Matrix (default)
  # lower=2 : upper Triangular Matrix
  n <- nrows
  if( lower == 1)
  { DistanceArray <- array(0, n*(n-1)/2 )
  for(i in 1:(n-1) )
    for(j in (i+1):n )
    { kk1 <- (j-1)*(j-2)/2 + i
    kk2 <- n*(i-1)- i*(i-1)/2 + j-i
    DistanceArray[kk2] <- datam[kk1] }
  }
  else
    DistanceArray <- datam
  
  DistanceArray = 10 - DistanceArray
  MD <- matrix(0, nrow=n, ncol=n)
  
  for(j in 1:(n-1) )
    for(k in (j+1):n)
    { kk <- n*(j-1) - j*(j-1)/2 + k-j
    MD[j,k] <- MD[k,j] <- DistanceArray[kk]
    } 
  
  colnames(MD) = cname
  rownames(MD) = cname
  return(MD)
}

# 행렬자료 읽기
source("c:/data/mva/nreadMatrix.r")
datam = scan("c:/data/mva/country1968.txt")
country_name = scan("c:/data/mva/countryname.txt", what="")
country_name
cdata = readMatrix(datam, nrows=12, country_name) # 거리행렬 생성
cdata 

# 비메트릭 MDS isoMDS 함수 실행
library(MASS)
nmds = isoMDS(cdata, k=2)
names(nmds)
nmds # 스트레스 값은 0.189($stress=18.85542)
x = nmds$points[,1]
y = nmds$points[,2]
plot(x,y, xlab="", ylab="", main="isoMDS", type="n")
text(x,y, labels=row.names(cdata), cex=0.9)
abline(h=0, v=0, lty=2)
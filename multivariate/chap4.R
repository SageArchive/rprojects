# Chap4. 군집분석(Cluster Analysis)

# 군집분석 사례1
# 데이터 읽기
beer = read.csv("c:/data/mva/beerbrand.csv", header=T, row.names=1)
head(beer)
summary(beer)

# 자료 표준화
zbeer = scale(beer)
round(apply(zbeer, 2, mean), 3) # 평균은 0
round(apply(zbeer, 2, sd), 3) # 분산은 1

# 거리행렬 계산
zbeer_euc = dist(zbeer) # dist함수의 디폴트가 유클리디안 거리 계산
zbeer_euc[1]
zbeer_man = dist(zbeer, "manhattan")
zbeer_man[1]

# 계층적 군집분석: 최단연결법
hc_s = hclust(zbeer_euc, method='single')
hc_s
par(mfrow=c(1,1))
plot(hc_s)
plot(hc_s, hang=-1) # Put the labels at the same height: hang = -1

# 계층적 군집분석: 최장연결법
hc_c = hclust(zbeer_euc, method='complete')
hc_c
plot(hc_c, hang=-1)

# 계층적 군집분석: 중심연결법
hc_cen = hclust(zbeer_euc, method="centroid")
hc_cen
plot(hc_cen, hang=-1)

# 소속 군집 알기
hc_cen24 = cutree(hc_cen, 2:4)
hc_cen24

# 계층적 군집분석: 와드의 방법
hc_w = hclust(zbeer_euc, method="ward.D")
hc_w
plot(hc_w, hang=-1)

# 비계층적 군집분석: K-평균 군집분석
kmc = kmeans(zbeer, centers=2)
kmc

# K-평균 소속 군집 산점도
plot(zbeer, col=kmc$cluster, pch=16) # 처음 두 개 변수 사용
pairs(zbeer, col=kmc$cluster, pch=16, cex.labels=1.5) # 모든 변수 사용
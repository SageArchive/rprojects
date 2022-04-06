# 4장 군집분석 연습문제 5번 p.176

# 자료 가져오기
customer = read.csv("c:/data/mva/mall_customer.csv", header=T)
head(customer)
# 변수 선택 및 변환
customer_data = customer[,2:5]
customer_data$Gender=ifelse(customer_data$Gender=="Female",1,0)
head(customer_data)
summary(customer_data)


# 자료 표준화
zcustomer = scale(customer_data)
round(apply(zcustomer, 2, mean), 3)
round(apply(zcustomer, 2, sd), 3)

# 거리행렬 계산하기
# 유클리디안 거리
zcustomer_euc = dist(zcustomer)
zcustomer_euc[1]
# 맨해튼 거리
zcustomer_man = dist(zcustomer, "manhattan")
zcustomer_man[1]

# 계층적 군집분석 - 와드의 방법
hc_w = hclust(zcustomer_euc, method="ward.D")
hc_w
plot(hc_w, hang=-1)

# 소속 군집 알기
hc_wmember <- cutree(hc_c, k=4)
hc_wmember
table(hc_wmember)

# 각 군집별 중심점 찾기
data_with_ward = cbind(customer_data, hc_wmember)
summary(data_with_ward)
aggregate(.~hc_wmember, data_with_ward, mean)

# 계층적 군집분석 - 최장연결법
hc_c = hclust(zcustomer_euc, method='complete')
hc_c
plot(hc_c, hang=-1)

# 소속 군집 알기
hc_cmember <- cutree(hc_c, k=4)
hc_cmember
table(hc_cmember)

# 각 군집별 중심점 찾기
data_with_complete = cbind(customer_data, hc_cmember)
aggregate(.~hc_cmember, data_with_complete, mean)

# K-평균 군집분석 실행
kmc = kmeans(zcustomer, centers=6)
kmc

# K-평균 소속 군집 산점도
pairs(zcustomer, col=kmc$cluster, pch=16, cex.labels=1.5)

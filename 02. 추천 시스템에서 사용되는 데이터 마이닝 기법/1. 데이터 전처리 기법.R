# 유사도 측정
# 1. 유클리디안 측정
x1 <- rnorm(30)
x2 <- rnorm(30)
Euc_dist <- dist(rbind(x1, x2), method = "euclidean")

# 2. 코사인 거리
vec1 <- c(1,1,1,0,0,0,0,0,0,0,0,0)
vec2 <- c(0,0,1,1,1,1,1,1,1,1,1,1)
library(lsa)
cosine(vec1, vec2)

# 3. 피어슨 상관계수
Coef <- cor(mtcars, method = "pearson")
Coef

# 차원축소
# 1. 주성분 분석(PCA)
data("USArrests")
colnames(USArrests)
rownames(USArrests)
apply(USArrests, 2, var)

pca <- prcomp(USArrests, scale. = T)
pca

names(pca)
pca$rotation  # 각 변수들의 비율
pca$rotation = -pca$rotation
pca$x = -pca$x

biplot(pca, scale = 0)

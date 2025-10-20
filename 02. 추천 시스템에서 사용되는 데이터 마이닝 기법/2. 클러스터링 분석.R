# K-평균 클러스터링
# 1. 클러스터링 할당 단계 : 임의의 클러스터 포인트를 선택 후 각각의 포인트들을 자신과 가까운 클러스터 포인트로 할당
# 2. 중심이동단계 : 각 클러스터 데이터 포인트의 평균을 구해 계산된 지점으로 중심점을 이동
# 3. 평균값과 중심점이 같아질 때까지 반복

library(cluster)
data(iris)
head(iris)
iris$Species <- as.numeric(iris$Species)
kmeans <- kmeans(x = iris, centers = 5)
clusplot(iris, kmeans$cluster, color = T, shade = T, labels = 13, lines = 0)

# Elbow 기법으로 적절한 군집 수 구하기
library(cluster)
library(ggplot2)
iris$Species <- as.numeric(iris$Species)
cost_df = data.frame()

for (i in 1:100){
  kmeans <- kmeans(iris, centers = i, iter.max = 50)
  cost_df <- rbind(cost_df, cbind(i, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")

ggplot(data = cost_df, aes(x = cluster, y = cost, group = 1)) +
  theme_bw(base_family = "Garamond") +
  geom_line(colour = "darkgreen") +
  theme(text = element_text(size = 20)) +
  ggtitle("Reduction In Cost For Values of 'k'\n") +
  xlab("\nClusters") +
  ylab("Within-Cluster Sum of Squares\n")

# 서포트 벡터 머신(SVM)
library(e1071)
data(iris)
sample <- iris[sample(nrow(iris)), ]
train <- sample[1:105, ]
test <- sample[106:150, ]
tune <- tune(METHOD = svm, train.x = Species ~ ., data = train, 
             ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)), kernel = "radial",
             scale = F)
tune$best.model
summary(tune)

model <- svm(Species ~ ., data = train, kernel = "radial", cost = 1, scale = F)
summary(model)

pred <- predict(model, test)
pred

# 의사결정나무
library(tree)
sample <- iris[sample(nrow(iris)), ]
train <- sample[1:105, ]
test <- sample[106:150, ]
model <- tree(Species ~ ., data = train)
summary(model)

plot(model)
text(model)

pred <- predict(model, test[, -5], type = "class")
pred

# 랜덤포레스트
library(randomForest)
data(iris)
sample <- iris[sample(nrow(iris)), ]
train <- sample[1:105, ]
test <- sample[106:150, ]
model <- randomForest(Species ~ ., data = train, mtry = 2, importance = T, proximity = T)
model

pred <- predict(model, newdata = test[, -5])
pred

# 부스팅
library(gbm)
data(iris)
sample <- iris[sample(nrow(iris)), ]
train <- sample[1:105, ]
test <- sample[106:150, ]

model <- gbm(Species ~ ., data = train, distribution = "multinomial",
             n.trees = 5000, interaction.depth = 4)
model
summary(model)

pred <- predict(model, newdata = test[, -5], n.trees = 5000)
pred

p.pred <- apply(pred, 1, which.max)
p.pred

# 아이템 기반 협업 필터링
library(recommenderlab)
data(MovieLense)
rating_movies <- MovieLense[rowCounts(MovieLense) > 50,   # 50개 이상의 영화를 평가한 사용자 
                            colCounts(MovieLense) > 100]  # 100개 이상 평가된 영화
rating_movies

# 트레이닝 및 테스트 세트 정의
which_train <- sample(x = c(T, F), size = nrow(rating_movies),
                      replace = T,  # 복원추출
                      prob = c(0.8, 0.2))  # 훈련 데이터는 TRUE, 테스트 데이터는 FALSE
head(which_train)

recc_data_train <- rating_movies[which_train, ]  # 훈련 데이터
recc_data_test <- rating_movies[-which_train, ]  # 테스트 데이터

which_set <- sample(1:5, size = nrow(rating_movies), replace = T)  # 복원추출
for(i in 1:5){
  which_train <- which_set == i
  recc_data_train <- rating_movies[which_train, ]
  recc_data_test <- rating_movies[-which_train, ]
  # Recommender 구성하기
}  # evaluationScheme 함수로 대체 가능

# 추천 모델 생성
recommnder_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommnder_models$IBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train, 
                          method = "IBCF",  # 유사도 함수. cosine(기본값), pearson
                          parameter = list(k = 30))  # k개의 가장 유사한 아이템 선별 및 저장
recc_model
class(recc_model)

# 추천 모델 탐색
model_details <- getModel(recc_model)
model_details$description  # 모델 설명
model_details$sim  # 유사도 매트릭스
class(model_details$sim)
dim(model_details$sim)

image(model_details$sim[1:20, 1:20], main = "Heatmap of the first row and columns")

model_details$k

row_sums <- rowSums(model_details$sim > 0)
table(row_sums)

col_sums <- colSums(model_details$sim > 0)
table(col_sums)

qplot(col_sums) + 
  stat_bin(binwidth = 1) +
  ggtitle("Distribution of the column count")  # 열 개수의 분포

which_max <- order(col_sums, decreasing = T)[1:6]
rownames(model_details$sim)[which_max]  # 사람들이 많이 본 영화 순위 top6

# 테스트 세트에 추천 모델 적용
n_recommended <- 6
recc_predicted <- predict(recc_model, 
                          newdata = recc_data_test, 
                          n = n_recommended)
recc_predicted
class(recc_predicted)
slotNames(recc_predicted)
# "items" : 각 사용자에 대한 추천 아이템들의 색인에 대한 목록
# "itemLabels" : 아이템의 이름
# "n" : 추천 수

recc_predicted@items[[1]]  # 1번째 사용자를 위한 추천 항목 인덱스들
recc_predicted@ratings[[1]]
recc_predicted@itemLabels
recc_predicted@n

recc_user_1 <- recc_predicted@items[[1]]  # 13 22 27 56 71 73
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]  # 1번째 사용자를 위한 추천 영화 목록 top6
movies_user_1

recc_matrix <- sapply(X = recc_predicted@items,
                      FUN = function(x){
                        colnames(rating_movies)[x]
                      })
recc_matrix[,1:4]  # # 처음 4명의 사용자에 대한 추천 항목

number_of_itmes <- factor(table(recc_matrix))
qplot(number_of_itmes) +
  ggtitle("Disteibution of the number of items for IBCF")

number_of_itmes_sorted <- sort(number_of_itmes, decreasing = T)
number_of_itmes_top <- head(number_of_itmes_sorted, 4)
table_top <- data.frame(names(number_of_itmes_top),
                        number_of_itmes_top)
table_top  # 가장 많이 추천된 영화제목과 추천횟수

#install.packages("recommenderlab")
library(recommenderlab)
data_package <- data(package = "recommenderlab")
data_package$results[, "Item"]

data(MovieLense)
MovieLense # 희소행렬
class(MovieLense)

object.size(MovieLense)
object.size(as(MovieLense, "matrix"))
object.size(as(MovieLense, "matrix")) / object.size(MovieLense) # recommenderlab 매트릭스가 더 간결함.

# 유사도 매트릭스 계산
# 처음 4명의 사용자가 서로 얼마나 유사한지 확인
similarity_users <- similarity(MovieLense[1:4, ], method = "cosine", which = "users")
similarity_users
class(similarity_users)

as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")

# 처음 4개 아이템 간의 유사도 계산
similarity_items <- similarity(MovieLense[, 1:4], method = "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")

# 추천모델
# class(MovieLense) = "realRatingMatrix"
recommender_models <- recommenderRegistry$get_entries(dataType = class(MovieLense))
recommender_models
names(recommender_models)

recommender_models$IBCF_realRatingMatrix$parameters # IBCF 추천모델의 파라미터 확인
lapply(recommender_models, "[[", "description") # 각 모델에 대한 설명
################################################################################
# 데이터 탐구
library(recommenderlab)
library(ggplot2)
data("MovieLense")
class(MovieLense)

dim(MovieLense) # 934명 사용자, 1664개 영화
slotNames(MovieLense) # 객체 내에 저장된 모든 데이터를 표시

# 평점 값 탐구
vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings) # 평점은 0 ~ 5 사이의 정수
table_ratings <- table(vector_ratings)
table_ratings

vector_ratings <- vector_ratings[vector_ratings != 0] # 평점 0은 필요없으므로 제거
vector_ratings <- factor(vector_ratings)
qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings") # 평점 분포

# 조회된 영화 탐색
views_per_movie <- colCounts(MovieLense) # 각 열(영화)에 존재하는 값의 수
#views_per_movie

table_views <- data.frame(movie = names(views_per_movie), 
                          views = views_per_movie)
table_views <- table_views[order(table_views$views, decreasing = T), ]
table_views

ggplot(data = table_views[1:10, ], aes(x = movie, y = views)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Number of views of the top movies")

# 평균 평점 탐색
average_ratings <- colMeans(MovieLense)
qplot(average_ratings) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 100] # 100이하의 조회 수 영화 제외
qplot(average_ratings_relevant) +
  stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))

# 매트릭스 시각화
image(MovieLense, main = "Heatmap of the rating matrix")
image(MovieLense[1:10, 1:15], main = "Heatmap of the first rows and columns")

min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)
min_n_movies
min_n_users

image(MovieLense[rowCounts(MovieLense) > min_n_movies, 
                 colCounts(MovieLense) > min_n_users], main = "Heatmap of the top user and movies")
################################################################################
# 데이터 준비
# 영화를 50편 이상 평가한 사용자 & 최소 100번 이상 시청된 영화
rating_movies <- MovieLense[rowCounts(MovieLense) > 50,
                            colCounts(MovieLense) > 100]
rating_movies

# 가장 적절한 데이터 탐색
min_movies <- quantile(rowCounts(rating_movies), 0.98)
min_users <- quantile(colCounts(rating_movies), 0.98)

image(rating_movies[rowCounts(rating_movies) > min_movies,
                    colCounts(rating_movies) > min_users], main = "Heatmap of the top users and movies")

average_ratings_per_user <- rowMeans(rating_movies) # 사용자별 평점 분포
qplot(average_ratings_per_user) + 
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

# 데이터 정규화
# 모든 영화에 높은/낮은 평점을 부여하는 사용자는 결과를 왜곡시킬 수 있음.
# 따라서, 각 사용자의 평균 평점이 0이 되도록 데이터를 정규화해 해결 가능.
rating_movies_norm <- normalize(rating_movies)
sum(rowMeans(rating_movies_norm) > 0.00001)  # 사용자의 평균 평점

image(rating_movies_norm[rowCounts(rating_movies_norm) > min_movies,
                         colCounts(rating_movies_norm) > min_users], main = "Heatmap of the Top users and movies")

# 데이터 이진화
# 사용자가 영화에 평점을 매기면 1, 아니면 0인 매트릭스 정의
# 또는, 평점이 일정한 기존값 이상인 경우 1, 아니면 0인 매트릭스 정의
rating_movies_watched <- binarize(rating_movies, minRating = 1)  # 평점이 1이상인 경우 매트릭스를 1로 정의
min_movies_binary <- quantile(rowCounts(rating_movies), 0.95)
min_users_binary <- quantile(colCounts(rating_movies), 0.95)
image(rating_movies_watched[rowCounts(rating_movies) > min_movies_binary,
                            colCounts(rating_movies) > min_users_binary], main = "Heatmap of the Top users and movies")

rating_movies_good <- binarize(rating_movies, minRating = 3)
image(rating_movies_good[rowCounts(rating_movies) > min_movies_binary,
                        colCounts(rating_movies) > min_users_binary], main = "Heatmap of the Top users and movies")

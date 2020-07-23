# 복원
sample(1:10,5 , replace = T)
# 비복원
sample(1:10,5,replace = F)

sample(c("앞면","뒷면"),10,replace = T)
# 0과 1사이 난수 추출
runif(10,min = 0,max = 1)
runif(10,0,1)


# 그래프 초기화
iteration <- 5000
plot(0,0,
     xlab = "동전을 던진 횟수", 
     ylab = "앞면이 나오는 비율",
     xlim = c(0, iteration), 
     ylim = c(0,1))

abline(a = 0.5, b= 0, col = "red",,lty =2)

# 실험 반복에 따른 확률의 변화
iteration <- 5000
sum <- 0
for(x in 1 : iteration){
  coin <- sample(c("앞면", "뒷면"),1,replace = T)
  if (coin =="앞면")
    sum = sum + 1
  prob <- sum/x
  points(x,prob)
}

# 복원 추출을 이용한 실험
# 시행회수 : 5000
iteration <- 5000
x <- sample(c(0,1), iteration, replace = T)
round(sum(x)/iteration*100,1)

# 원주율 계산
iteration <- 5000
n_circle <- 0
PI = 3.141592

for (i in 1: iteration) {
  x <- runif(1, min=0,max=1) # 난수를 꺼내 x,y 좌표를 나타냄
  y <- runif(1,min = 0, max = 1) # 
  
  dist <- sqrt(x^2+y^2)
  
  if(dist <= 1)
    n_circle <- n_circle +1
}
pi <- 4*n_circle/iteration
pi
PI - pi


##################################
# 12.5 회귀선 구하기 (간단한 실습)
# x, y 데이터 준비 및 산포도 그리기
x <- c(1, 2.2, 3.5, 4, 5.2, 6.4, 7.1, 8.3, 9.2, 10)
y <- c(41, 47, 62, 75, 85, 90, 110, 115, 125, 140)

plot(x, y, xlim=c(0,10), ylim=c(0,150))
fit <- lm(y ~x)
abline(fit, lty=2, col="blue")

# 임의의 기울기(b)와 y절편(a) 설정에 따른 비용함수 값 구하기
B <- 10.95
# A <- 26.70 수가 커질수록 멀어짐.
#A <- 30
# A <- 26.7
# A <- 88
# A <- 50

A.set <-  seq(2,56 , by = 2)
cost.set <- NULL
# length(A)
for (A in A.set){
  abline(a=A, b=B, lty=2, col="green")
  err.sum <- 0
  for(k in 1:length(x)) {
    y_hat <- B*x[k] + A
    err <- (y_hat - y[k])^2
    err.sum <- err.sum + err
  }
  cost <- err.sum/length(x)
  print(cost)
  cost.set <- c(cost.set, cost)
}
cost.set
plot(A.set,cost.set)
cost.mtx[1:5, 1:5]

min(cost.set)
cost.ind <- which(cost.set == min(cost.set))
cost.ind

A.set[1]
A.set[cost.ind]

abline(v = A.set[cost.ind], col = "red", lty = 2)
abline(h = cost.set[cost.ind], col = "red", lty = 2)


# 기울기와 y절편 구하기
range(cost.mtx)
idx <- which(cost.mtx == min(cost.mtx), arr.ind = TRUE)
idx
idx.Bmin <- idx[1,1]
idx.Amin <- idx[1,2]
Bmin <- B[idx.Bmin]
Bmin
Amin <- A[idx.Amin]
Amin
abline(a=Amin, b=Bmin, lty=2, col="red")

# 기울기와 y절편 범위에서의 비용함수 그래프
plot(B, cost.mtx[,idx.Amin],
     ylim=c(0, 800),
     type='o',
     lwd=3)

for(j in 1:length(A)) {
  lines(B, cost.mtx[,j], col=j)
  Sys.sleep(0.5)
}

plot(A, cost.mtx[idx.Bmin,],
     ylim=c(0, 700),
     type='o',
     lwd=3)

for(i in 1:length(B)) {
  lines(A, cost.mtx[i,], col=i)
  Sys.sleep(0.5)
}



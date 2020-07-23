# 애니메이션
library(animation) # "animation 패키지 설치후 실행

ani.options(interval = 1) # 1초 간격의 애미메이션 시간 설정
plot.new() # 그래픽 프레임 생성

for (i in 10:0) { # 10에서 0까지 감소하면서 반복
  rect(0, 0, 1, 1, col="yellow") # 그래픽 출력 영역 지정(전체 영역 설정)
  text_size = 11-i # 숫자 출력 크기 
  text(0.5, 0.5, i, cex=text_size, col=rgb(0,0,1,1))
  ani.pause()
}

# 반복문
sum <-  0
for (i in 1 : 10) {
  sum <- sum + i
}
print(sum)

sum <- 0
for (i in 1:10) {
  if (i %% 2 == 0)
    next
  sum <- sum + i
}
print(sum)

# while
sum <- 0
i <- 1
while (i < 10) {
  sum <- sum + i
  i <-  i + 1
}
print(sum)


# repeat, break
sum <- 0
i <- 1
repeat{
  sum <- sum + i
  i <- i + 1
  if(i > 10)
      break
}
print(sum)

# next
sum <- 0
for (i in 1:10) {
  if(1%%2 == 0)
    next()
  sum <- sum+i
}
print(sum)
}

# 1초 간격으로 그래프 그리기
library(animation)

ani.options(interval = 1)

while(TRUE){
  y <- runif(5,0,1)
  barplot(y, ylim = c(0,1), col = rainbow(5))
  ani.pause()
}

# 동전 던지기
plot.new()
count <- c(0,0,0)
for (n in 1:50){
  coin <- sample(c(0,1),2,replace = T) # 복원추출
  inindex <- sum(coin) + 1
  count[index] <- count[index] + 1
  probability <- count/n
  title <- paste("반복 수: ",n,"/500")
  barplot(probability,
        names.arg = c(0,1,2),
        xlab = "앞면이 나온 횟수",
        ylab = "확률",
        col = rainbow(3),
        main = title,)
        Sys.sleep(0.05)
}

# 룰렛 돌리기
# 룰렛 이미지 출력
library(imager)
img_path <- "C:/Users/82104/Desktop/Program/DATA SCIENCE/roulette.png"
img <- load.image(img_path)
plot(img)

# 크기 변환
img <- resize(img, size_x = 400L, size_y = 400L)
plot(img, xlim = c(0,400),ylim=c(0,400),axes = FALSE)

# 룰렛 회전
rot.count <- sample(11:20,1)
rot.count
angle <- 0

for(i in 1: rot.count) {
  angle <- angle + 36
  title <- paste("룰렛 회전:", i, "/", rot.count)
  imrotate(img,angle,cx=200, cy=200) %>%
  draw_circle(x= 200, y=30,radius=30,
              col=c(1,0,0),opacity = 0.5) %>% # 투명도
  plot(axes = FALSE, main = title)
  Sys.sleep(0.2)
}

# 라이언킹
library(magick)
lion_bg <- image_read("C:/Users/82104/Desktop/Program/DATA SCIENCE/lion_bg.png")
lion_1 <- image_read("C:/Users/82104/Desktop/Program/DATA SCIENCE/lion1.png")
lion_2 <- image_read("C:/Users/82104/Desktop/Program/DATA SCIENCE/lion2.png")

lion_bg <- image_scale(lion_bg, "600x300!")
lion_1 <- image_scale(lion_1, "100x50!")
lion_2 <- image_scale(lion_2, "100x50!")

print(lion_bg)
print(lion_1)
print(lion_2)

# 이미지 합성 테스트
img <- image_composite(lion_bg, lion_1, offset = "+100+200")
print(img)

# 이미지 이동
moving <- 0
x <- 0
y <- 150
while(TRUE) {
  if(x < 0)
    position <- paste(x, "+", y, sep ="")
  else
    position <- paste("+",x,"+",y, sep = "")
  if(moving %% 2 == 0){
    img <- image_composite(lion_bg, lion_1, offset = position)
  } else{
    img <- image_composite(lion_bg, lion_2, offset = position)
  }
  print(img)
  Sys.sleep(0.3)
  if (x > 600)
    break
  moving <- moving + 1
  x <- x + 20
}



  

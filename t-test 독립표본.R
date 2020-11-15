library(dplyr)
library()
#t-test 

#t-test 독립표본 
mymethod <- read.csv("mymethod.csv", header = TRUE)
head(mymethod) #제대로 불러왔는지 확인 (이상치 99가 보인다.)

company <- mymethod %>% filter(performance < 99)
head(company)
table(company$method)
after <- company %>% filter(method == 1)
realafter <- company %>% filter(method == 2)
shapiro.test(after$performance) #첫번째 method가 정규성을 띄는지 샤피로 검정
shapiro.test(realafter$performance) # 두번째 method가 정규성을 띄는지 샤피로 검정
var.test(after$performance, realafter$performance) #등분산성을 띄는지 바테스트
t.test(after$performance, realafter$performance, alternative = "less", conf.int = 0.95)

#12월 4일 R활용 빅데이터 분석

getwd()
setwd("C:\\Rtest")

#교육수준과 흡연정도의 상관분석

#분류형 변수들 간의 관계에 대해 검정하는 것을 독립성 분석이라고 한다.

#Q. 교육수준과 흡연정도는 서로 관련이 있을까?

study <- read.csv("mysmoke.csv", header = TRUE)
head(study) 
table(study$education) #1:대졸 2:고졸 3:중졸
table(study$smoking) #1:과다흡연 2:보통흡연 3:비흡연

#교육수준과 흡연 간에는 서로 관련성이 있을까?

table(study$education, study$smoking)
#123,123으로 헷갈리므로 뭔지 이름을 지어주자.
study$education[study$education==1] <- "univ"
study$education[study$education==2] <- "high"
study$education[study$education==3] <- "middle"
study$smoking[study$smoking==1] <- "heavy"
study$smoking[study$smoking==2] <- "light"
study$smoking[study$smoking==3] <- "none"

table(study$education, study$smoking)

chisq.test(study$education, study$smoking)
#p값이 0.05보다 낮으므로 교육수준과 흡연에는 관계가 있다고 볼 수 있다.
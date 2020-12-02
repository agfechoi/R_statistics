# 12월 2일 R활용 빅데이터 분석

#영업 사원 교육 효과 분석

#두 집단의 평균이 서로 같은지 다른지를 검증하는 것을 두 집단 평균 차이 분석이라 한다.
#two sample t-test. 두 그룹 간의 평균을 기준으로 차이에 의미를 부여하는 방법

#두 집단의 특정변수가 수치데이터로 이루어진 경우 평균값을 분석할 수 있으며,
#이 평균값이 두 집단 간에 동일한지/다른지를 비교하는 분석이라고 할 수 있다.

#Q. 영업사원들의 실적을 향상시키기 위해 어떠한 교육이 더 좋을까
#두 가지 교육방법별로 교육을 이수한 영업사원들의 영업성적을 조사한 결과
#차이를 보고싶다.

#method = 1 집체교육 2 멘토링교육
library(dplyr)
method <- read.csv("mymethod.csv", header = T)
head(method)

method <- method %>% filter(performance < 99) #이상치 제거
groupA <- method %>% filter(method == 1) #집체교육을 받은 애들만 추출
groupB <- method %>% filter(method == 2) #멘토링교육을 받은 애들만 추출

range(groupA$performance)
range(groupB$performance)
var.test(groupA$performance, groupB$performance) #분석 집단의 데이터 분포가
#정규분포를 이루고 있는지 검정
#두 집단의 분포 모양이 서로 동질적이면 t-test를 아니라면 wilcox.test를 써야한다.
#var.test결과 p값이 0.05보다 크다 == 정규분포를 따른다. => t.test가능
t.test(groupA$performance, groupB$performance, alternative = "two.sided", conf.level = 0.95)
#양측검정 결과 p값이 0.05보다 작다 --> 두 그룹의 퍼포먼스 차이가 있다고 볼 수 있다.
t.test(groupA$performance, groupB$performance, alternative = "greater", conf.level = 0.95)
#단측검정(greater)결과 p값이 1이 나오므로 groupA(집체교육)의 퍼포먼스가
#groupB(멘토링교육)의 퍼포먼스보다 작다는걸 알 수 있다. less로 해보자
t.test(groupA$performance, groupB$performance, alternative = "less", conf.level = 0.95)
#단측검정(less)결과 p값이 0.05보다 작으므로 그룹A의 퍼포먼스가 
#그룹 B보다 작다고 볼 수 있다.
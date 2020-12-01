# 12월 1일 R활용 빅데이터 분석

#신차 색상 고객 선호도 분석
#신차 색상이 고객 선호도에 영향을 미치는가

# H0 : 자동차 색상은 고객 선호도에 영향을 미치지 않을 것이다.
# H1 : 자동차 색상은 고객 선호도에 영향을 미칠 것이다.
#자동차 색상은 범주형 변수.
#카이제곱 검정 --> 관찰된 빈도가 기대되는 빈도와 의미있게 다른지 여부를 검증
#하기 위해 사용되는 검증방법
getwd()
setwd("C:\\Rtest")

mycar <- read.csv("mycar.csv", header = TRUE)

table(mycar$color) # 차 색상의 빈도를 table함수로 본다.

chisq.test(table(mycar$color)) #카이제곱검정 안에 빈도수를 그대로 넣는다.
#p값이 유의수준(0.05)보다 낮으므로 자동차 색상은 유의미하게 고객 선호도에
#영향을 미쳤다고 볼 수 있다.

#책상 납품을 위한 학생 신장 분석
#한 집단의 평균이 어떤 특정한 값과 같은지를 검증하는 것을 단일집단 평균 분석
#이라고 한다. (ONE SAMPLE TEST) 또한 한 변수의 평균이 특정한 값과 같은지를
#알아보기 위한 방법으로 가장 간단하게 사용할 수 있다.
# 한 집단의 특정 변수가 수치데이터로 이루어진 경우 평균값을 분석할 수 있으며
#이 평균값이 사전에 조사된 특정평균값과 동일한지 다른지를 비교하는 분석이다.

#단일 집단 평균 분석은 통상 t-검정에 의해 이루어 진다.
# t-검정 : 두 집단간 평균이 통계적으로 유의미한 차이를 보이고 있는지 여부 검증

#책상 제조회사에서 중학생 표본을 대상으로 조사한 신장 데이터

# 일반적으로 중학생들의 평균신장이 145cm로 알려져 있다. 이때 책상을 이에 맞춰서
#제작해도 되는지 알아보고자 한다.

mid_height <- read.csv("myheight.csv", header = T)
head(mid_height) #제대로 불러왔는지 확인
mean(mid_height$height)  #평균값은 149.7
range(mid_height$height) #140~165 사이의 값들만 존재한다.

library(Hmisc)
describe(mid_height$height)
table(mid_height$height)

#분석집단의 데이터가 정규분포를 이루고 있는지 확인을 해야한다.
#정규분포를 이루고 있으면 t.test가 가능하고 아니면 wilcox.test를 해야한다.
#정규분포를 이루고 있는지 여부는 shapiro.test로 한다.

shapiro.test(mid_height$height) #p값이 0.05보다 낮으므로 이경우 정규분포를
#안따르는것이다. 이때는 t.test를 할수 없다.
#샤피로 테스트는 p값이 0.05보다 크면 정규분포를 띄는거고 작으면 정규분포가
#아닌것을 의미한다.

wilcox.test(mid_height$height, mu = 145.0)
#우선 양측검정을 통해 145와 데이터속 중학생 평균 신장이 다르다는 것을 확인한다.

wilcox.test(mid_height$height, mu = 145.0, alter = "greater", conf.level = 0.95)
#alter = "greater" 인 겨우 샘플이 주어진 평균보다 크다는 것을 대립가설로 둘때
#사용한다. 이경우 p값이 0.05보다 작게 나왔으므로 mid_height$height가 145보다
#더 크다는 것을 알 수 있다.
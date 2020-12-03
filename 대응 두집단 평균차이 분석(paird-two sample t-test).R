#12월 3일 R활용 빅데이터 분석

#다이어트 식품 효과 분석

#짝을 이루는 두 집단의 평균이 서로 같은지, 다른지를 검증하는 것을 
#대응 두 집단 평균차이 분석이라고 한다,(paired-two sample t-test)
#짝지어진 변수값 차이의 평균이 특정한 값과 같은지를 검증한다.

#짝을 이룬다, 대응을 이룬다는 표현은 독립적인 두 집단이 아니라 실제는 하나의
#집단에서 두 번 측정이 이루어진 것을 의미한다. 한 집단을 대상으로
#사전-사후 효과 비교시 많이 사용되고 있는 방법이다.

#Q. 연구 개발 중인 다이어트 식품의 효과는 있는 것일까??
#before == 다이어트 효능식품 복용 전 몸무게
#after == 다이어트 효능식품 복용 후 몸무게
#중도포기자가 있다. == 이상치가 있다. or 결측치가 있다.

getwd()
setwd("C:/Rtest")
library(dplyr)
effect <- read.csv("myeffect.csv", header = T)
head(effect) #5번 id after에 999가 보인다. 중도 포기자를 999로 표현했나보다.
effect <- effect %>% filter(after < 999) #after가 999인 행을 제거해준다.
head(effect)

#귀무가설 : 다이어트 효능식품의 효과는 없다.
#대립가설 : 다이어트 효능식품의 효과는 있다.

#t.test를 하기전에 먼저 정규분포를 이루는지 확인하기 위해서 var.test를 본다.
var.test(effect$before, effect$after)
#var.test결과 p값이 0.05보다 낮으므로 정규분포를 이루지 않는다고 볼 수 있다.
#고로 t.test가 아니라 wilcox.test를 해야한다.

wilcox.test(effect$before, effect$after, paired = TRUE)
#양측검정(기본값)결과 p값이 0.05보다 낮으므로 복용 전/후 차이가 있다고 볼 수 있다.

wilcox.test(effect$before, effect$after, paired = TRUE, alternative = "greater")
#단측검정(greater)결과 p값이 0.05보다 낮으므로 비포어가 애프터보다 값이 더 크다고
#볼수 있다. --> 애프터의 값(복용 이후의 몸무게)가 더 낮다, 고로 다이어트 효능식품
#효과가 있다고 볼 수 있다.

wilcox.test(effect$before, effect$after, paired = TRUE, alternative = "less")
#혹시 모르니까 참고로 less단측검정을 해보면 p값이 1이 나와버린다.
#왼쪽거가 더 작다?라고 했을떄 어림도 없다고 나온 것이다.
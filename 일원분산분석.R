#12월 5일 R활용 빅데이터 분석

#분산분석

#분산분석이란 종속변수의 개별 관측치와 이들 관측치의 평균값 사이의 변동을
#그 원인에 따라 몇 가지로 나누어 분석하는 방법으로, 실험요인의 종류가 하나인
#모형은 일원분산분석, 둘일 경우는 이원분산분석이라고 한다.

#일원배치 분산분석
#두 집단에서 나온 자료가 동일한 분포를 가지고 있는가를 검정하는 t-test검정을
#확장하여, k개의 표본들이 같은 분포를 가지고 있는가를 검정하는데 사용하며,
#요인이 하나인 실험에 대한 분산분석으로서 일원배치분산분석(일원분산분석)이라 한다.

#Q1. 세 종류의 건전지의 수명에는 차이가 있을까?
a <- c(100 ,98, 85, 90, 88, 80)
b <- c(73, 80, 80, 75, 67, 57)
c <- c(110, 104, 91, 109, 85, 95)

life <- data.frame(a, b, c)
b.life <- stack(life) #박스그림으로 세 건전지 수명의 분포를 보기 위해 스택으로 묶
b.life

op = par(mfrow = c(1,2)) #그림을 1X2로 그릴거다. 
boxplot(values ~ ind, data = b.life) #그림으로 보여준다.

stripchart(life)

par(op)
dev.off() #그림 지우기

oneway.test(values ~ ind, data = b.life, var.equal = TRUE)
#oneway.test(열1 ~ 열n, var.equal은 샘플들의 분산이 같다고 가정하는 것.)

#앞에서 이용한 건전지의 내용을 가지고 aov를 이용하여 분산 분석을 실시해보자
#또한 TukeyHSD()를 이용하여 다중비교를 해보도록 하자
type = c(rep('a', 6), rep('b', 6), rep('c', 6))
y = c(100, 98, 85, 90, 88, 80, 73, 80, 80, 75, 67, 57, 110, 104, 91, 109, 85, 95)
ty = as.factor(type)
life.aov = aov(y ~ ty)
summary(life.aov)
#p값이 0.05볻 낮다. 별 세개가 옆에 있다 -- 세 종류의 건전지에는 수명의 차이가 있다.
#F값은 항상 1과 같거나 크다. F값이 클수록 두 집단간의 분산의 차이가 큰것.
#14.18정도면 큰편이다.

#TukeyHSD해보자 TukeyHSD는 다중비교방법중 하나다.
life.tucky = TukeyHSD(life.aov, "ty", ordered = TRUE)
life.tucky

#a-b, c-b는 평균차가 크고(18, 27) p값이 0.05보다 작으므로 이 그룹들끼리는 
#평균의 차이가 있다고 판단 가능. 하지만 c-a는 평균차도 작고 p값이 0.05 
#유의수준보다 크므로 평균차이가 없다고 판단 가능

#따라서 a-b, c-b는 건전지 수명이 각 회사마다 차이가 있다고 볼 수 있지만
#c-a의 경우 건전지 수명의 차이가 별루 없다고 볼 수 있다.

#그림으로 함 봐볼까
plot(life.tucky)
#c-a의 differences가 적은게 눈으로도 보인다.

pairwise.t.test(y, ty, p.adjust = 'none', pool.sd = TRUE)
#pairwise.t.test --> 집단간 t-test를 수행하여 그 값을 행렬로 보여준다.
#p.adjust ==> 다중검정보정 수행
#pool --> 각각 분석결과를 하나로 통합
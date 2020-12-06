#12월 6일 이요인 분산분석(이원분산분석)

#요인이 두개인 실험에 대한 분석법을 이원배치 분산분석이라고 한다.
#이원 배치 분산분석은 두 개의 요인에 대해 서로 상호 작용이 존재하는지를
#먼저 검정한다. 검정 결과 상호 작용이 존재하지 않는다고 한다면 각 요인의
#효과를 따로 구분하여 분석하는 것이 가능하다.

#Q. 온도와 압력에 따른 반응값의 차이는 어떻게 될까?(2차 반복 실험)

pressure = as.factor(c(320, 340, 360, 310, 330, 350, 300, 320, 340, 310, 330, 350))
temp = as.factor(c(rep('low', 6), rep('high', 6)))

y = c(130.5, 120.2, 150.8, 170.2, 157.1, 164.7, 102.6, 181.6, 160.5, 189.5, 165.3, 176.5)

op = par(mfrow = c(2,2)) #그래픽 장치의 설정을 정의
#mfrow = c(x, y) ==> x*y (행x열)의 그래프 연역을 만듬.

plot(y ~ temp)
plot(y ~ pressure)
stripchart(y ~ temp, vertical = TRUE, xlab = "temperature")
stripchart(y ~ pressure, vertical = TRUE, xlab = "pressure")
par(op)

op = par(mfrow = c(1,2))
interaction.plot(temp, pressure, y, bty = 'l', main = 'interaction plot')
#temp가 독립변수 mean of y가 종속변수
#그림으로 미루어보아 온도가 오를 수록 압력도 크다.
#온도 압력은 정비례 관계다.

interaction.plot(pressure, temp, y, bty = 'o')
#mean값이 y축으로 x축은 pressure

aov_pt = aov(y ~ temp + pressure + temp:pressure)
aov_pt
#제곱의 합 = sum of squares.

summary(aov_pt)

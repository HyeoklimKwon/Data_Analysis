#install.packages 
library(linprog)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(twitteR)
library(stringi)
test19 <- read_csv("~/SKKU/datasets/test19.csv")
filter(test19,Y1H7_2_1 == -1)

# 데이터 전처리 과정
education <- select(test19,region = REGION,sector = SECTOR, class_bylevel = Y1H7_2_1, math_score = Y1MAT_S )%>%
              group_by(region,sector)%>%filter(class_bylevel == 1 | class_bylevel == 0)
education

###공립vs사립####

public <- filter(education, sector == 1)
ggplot(public, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
t.test(math_score ~ class_bylevel, data = public, var.equal = FALSE)
# t-test 결과 높은 p value -> 분반 여부가 수학성적에 영향이 있다고 결론 내릴 수 없음


private <- filter(education, sector == 2)
ggplot(private, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
t.test(math_score ~ class_bylevel, data = private, var.equal = FALSE)


###지역##
special <- filter(education, region == 1| region == 2)
ggplot(special, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
t.test(math_score ~ class_bylevel, data = special, var.equal = FALSE)
# t-test 결과 낮은 p value와 함께 분반한 쪽의 수학 성적이 높게나옴 -> 광역시에서는 분반하는 것이 수학 성적이 높게나온다고 결론 가능

metro <- filter(education, region == 2)
ggplot(metro, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
t.test(math_score ~ class_bylevel, data = metro, var.equal = FALSE)

median <- filter(education, region == 3)
ggplot(median, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
t.test(math_score ~ class_bylevel, data = median, var.equal = FALSE)
#t-test 결과 0.1의 p value와 함께 분반한 쪽의 수학 성적이 높게나옴 -> 신뢰도 90%에서는 중소도시에서 분반하는 것이 수학 성적이 높게나온다고 결론 가능
 

#읍면지역에는 분반안하는 학교가 존재하지 않음.
small <- filter(education, region == 4)

install.packages("gridExtra")
library(gridExtra)


graph1 <- ggplot(public, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
graph2 <- ggplot(private, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
grid.arrange(graph1,graph2,ncol=2)





ggplot(special, aes(as.factor(class_bylevel),math_score))+geom_boxplot()
ggplot(metro, aes(as.factor(class_bylevel),math_score))+geom_boxplot()



############

data2 <- data%>%group_by(SECTOR,Y1H7_2_1)%>%summarise(count=n())
data2 <- data2[c(1,2,4,5),]
data2$SECTOR <- c("공립","공립","사립","사립")
ggplot(data2, aes(x = factor(SECTOR), y = count, fill = factor(Y1H7_2_1))) +
  geom_bar (stat="identity", position=position_dodge (), width = 0.5) +
  theme_classic () +
  labs(title = "학교설립유형 그래프", x = "학교설립유형", y = "학교 수", fill = "수준별 수업 여부")
 
data3 <- data%>%group_by(REGION,Y1H7_2_1)%>%summarise(count=n())
data3 <- data3[c(1,2,4,5,7,8,10,11),]
data3$REGION <- c("특별시","특별시","광역시","광역시","중소도시","중소도시","읍면지역","읍면지역")
data3
ggplot(data3, aes(x = factor(REGION), y = count, fill = factor(Y1H7_2_1))) +
  geom_bar (stat="identity", position=position_dodge (), width = 0.5) +
  theme_classic () +
  scale_x_discrete(limits=c("특별시","광역시","중소도시","읍면지역"))+
  labs(title = "지역규모 그래프", x = "지역", y = "학교 수", fill = "수준별 수업 여부")

######Final Homework######

##Question 3
Stu_family_test <- test19%>%filter(Y2KOR_S >= 0 , Y2KOR_S >= 0 , INCOME >= 0 , PEDU >= 0, MEDU >= 0 
                     , Y2S27_1 >= 0
                     , Y2S27_2 >= 0
                     , Y2S27_3 >= 0
                     , Y2S27_4 >= 0
                     , Y2S27_5 >= 0
                     , Y2S27_6 >= 0
                     , Y2S27_7 >= 0
                     , Y2S27_8 >= 0
                     , Y2S27_9 >= 0
                     , Y2S27_10 >= 0 #음수값 제거
                     )%>%
  mutate(KOR_score = Y2KOR_S, A_sup = (Y2S27_1+ Y2S27_2+ Y2S27_3+ Y2S27_4+ Y2S27_5+ Y2S27_6+ Y2S27_7+ Y2S27_8)/8, 
         E_sup = (Y2S27_9 + Y2S27_10)/2,
         parent_sup = A_sup + E_sup # 부모님의 정서적 및 학술적 지원의 합 
         )%>%
  select(KOR_score, A_sup, E_sup, PEDU, MEDU, INCOME,parent_sup)

#부모님의 학력을 학년 수로 변환
for( i in 1:nrow(Stu_family_test)){
  if(Stu_family_test[i,4] == 1){
    Stu_family_test[i,4] = 6
  }else if(Stu_family_test[i,4] == 2){
    Stu_family_test[i,4] = 9
  }else if(Stu_family_test[i,4] == 3){
    Stu_family_test[i,4] = 12
  }else if (Stu_family_test[i,4] == 4){
    Stu_family_test[i,4] = 14
  }else if (Stu_family_test[i,4] == 5){
    Stu_family_test[i,4] = 16
  }else if (Stu_family_test[i,4] == 6){
    Stu_family_test[i,4] = 18
  }else if (Stu_family_test[i,4] == 7){
    Stu_family_test[i,4] = 22
  }
}
Stu_family_test
for( i in 1:nrow(Stu_family_test)){
  if(Stu_family_test[i,5] == 1){
    Stu_family_test[i,5] = 6
  }else if(Stu_family_test[i,5] == 2){
    Stu_family_test[i,5] = 9
  }else if(Stu_family_test[i,5] == 3){
    Stu_family_test[i,5] = 12
  }else if (Stu_family_test[i,5] == 4){
    Stu_family_test[i,5] = 14
  }else if (Stu_family_test[i,5] == 5){
    Stu_family_test[i,5] = 16
  }else if (Stu_family_test[i,5] == 6){
    Stu_family_test[i,5] = 18
  }else if (Stu_family_test[i,5] == 7){
    Stu_family_test[i,5] = 22
  }
}

Stu_family <- Stu_family_test%>%mutate(Parent_EDU = (PEDU + MEDU)/2 
                                       )%>%select(KOR_score,Parent_EDU,INCOME,parent_sup)
Stu_family
#데이터 전처리



#부모님의 정서적 및 학술적 지원과 국어성적 상관검증 
cor.test(Stu_family$KOR_score,Stu_family$parent_sup,method = 'pearson')
plot(x = Stu_family_test$parent_sup, y = Stu_family_test$KOR_score)+
  abline(lm(Stu_family_test$KOR_score~Stu_family_test$parent_sup), col = "red")
#약한 상관관계 매우 낮은 p값(0.004871)



#부모님의 소득과 국어성적 상관검증 
cor.test(Stu_family_test$KOR_score,Stu_family_test$INCOME,method = 'pearson')
plot(x = Stu_family_test$INCOME, y = Stu_family_test$KOR_score)+
  abline(lm(Stu_family_test$KOR_score~Stu_family_test$INCOME), col = "red")
#0에 가까운 상관관계 높은 p값(상관관계가 없다는 귀무 가설을 reject할 수 없음)

#부모님의 학력(학년)과 국어성적 상관검증 
cor.test(Stu_family$KOR_score, Stu_family$Parent_EDU,method = 'pearson')
plot(x = Stu_family$Parent_EDU, y = Stu_family$KOR_score)+
  abline(lm(Stu_family$KOR_score~Stu_family$Parent_EDU), col = "red")
#약한 상관관계 낮은 p값(0.01238)




summary(lm(KOR_score~.,Stu_family))
#다중 회귀분석 결과 의미있는 변수 값 부모의 감정적 및 학술적 지원 변수가 나옴


#parent_sup 상관검증 결과 학부모의 감정적 지원 및 학술적지원 변수가 가장 선형 결합도가 높게 나오고 p값이 작았다.


library(rcompanion)

school <- test19%>%filter(Y2KOR_S >= 0)%>%
  select(Y2KOR_S,REGION, SECTOR, COEDU, CLASS1)
#학급 수가 적은 소규모 학교일 경우(2개 학급 이하) 1 그렇지 않을 경우 0

for(i in 1: nrow(school)){
  if(school[i,5] <= 2){
    school[i,5] = 1
  }else{
    school[i,5] = 0
  }
}
school

summary(lm(Y2KOR_S~.,school))
#다중회귀분석 결과 학급 수를 제외한 변수들은 의미있는 값으로 나타나지 않았다.

#크래머 룰을 이용해보았다. 
cramerV(school$REGION,school$Y2KOR_S)

#지역변수와 국어성적의 카이제곱 검증
chisq.test(school$REGION,school$Y2KOR_S)
ggplot(school,aes(x=factor(REGION),y=Y2KOR_S)) +
  geom_boxplot()
#약한 상관관계가 있는것으로 나왔다.


chisq.test(school$SECTOR,school$Y2KOR_S)

chisq.test(school$COEDU,school$Y2KOR_S)
#카이제곱 검증 결과 높은 p값으로 인해 학교의 설립 구분과 공학유무는 선형관계가 있다는 가설(대립가설)을 채택하기는 근거가 부족하다는 결과가 나왔다.



chisq.test(school$CLASS1,school$Y2KOR_S)
ggplot(school,aes(x=factor(CLASS1),y=Y2KOR_S)) +
  geom_boxplot()

##class1## 카이제곱 검증결과 REGION 변수와 CLASS1(소규모 학급 여부)가 의미있다고 나옴 그 중 P값이 더 작은 CLASS1 선택

Stu_individual <- test19%>%filter(Y2KOR_S >= 0, 
                                  Y2S14_2 >= 0,
                                  Y2S14_3 >= 0,
                                  Y2S14_5 >= 0,
                                  Y2S14_7 >= 0,
                                  Y2S2_24 >= 0,
                                  Y2S2_25 >= 0,
                                  Y2S2_26 >= 0,
                                  Y2S2_28 >= 0,
                                  Y2S2_29 >= 0,
                                  Y2S2_30 >= 0,
                                  Y2P31_1 >= 0,
                                  Y1KOR_S >= 0)%>%  
  mutate(KOR_score = Y2KOR_S, Book_enjoy = (Y2S14_2+Y2S14_3 +Y2S14_5 +Y2S14_7)/4, 
         Self_con = (Y2S2_24 + Y2S2_25 + Y2S2_26+ Y2S2_28+ Y2S2_29 + Y2S2_30)/6,
         private_edu = Y2P31_1,
         KOR_1stscore = Y1KOR_S,
         GENDER = GENDER -1)%>%
  select(KOR_score,GENDER, Book_enjoy, Self_con,private_edu, KOR_1stscore )
Stu_individual

chisq.test(Stu_individual$GENDER,Stu_individual$KOR_score)
chisq.test(Stu_individual$private_edu,Stu_individual$KOR_score)
#카이제곱 검증 결과 성별과 사교육여부는 국어성적과 선형관계가 있다고 하기에는 근거가 부족하다고 나왔다.



cor.test(Stu_individual$Book_enjoy,Stu_individual$KOR_score, method =  'pearson')
plot(Stu_individual$Book_enjoy,Stu_individual$KOR_score)+
  abline(lm(Stu_individual$KOR_score~Stu_individual$Book_enjoy), col = 'red')
#독서에 대한 즐거움과 국어성적의 상관검증 결과 약한 선형관계와 낮은 p값/ 상관관계는 있지만 강하지 않음.

cor.test(Stu_individual$Self_con,Stu_individual$KOR_score, method =  'pearson')
plot(Stu_individual$Self_con,Stu_individual$KOR_score)+
  abline(lm(Stu_individual$KOR_score~Stu_individual$Self_con), col = 'red')
#학업 자아개념과 국어성적의 상관검증 결과 독서에 대한 즐거움보단 강한 선형관계 매우 낮은 p값

cor.test(Stu_individual$KOR_1stscore,Stu_individual$KOR_score, method =  'pearson')
plot(Stu_individual$KOR_1stscore,Stu_individual$KOR_score)+
  abline(lm(Stu_individual$KOR_score~Stu_individual$KOR_1stscore), col = 'red')
#1학년 국어성적과 2학년 국어성적의 상관검증 결과 매우 강한 선형관계와 매우 낮은 p값/

#KOR_1stscore /카이제곱 검증 결과 성별과 사교육 여부는 큰 의미가 없었다.'1학년 국어 성적'변수가 가장 높은 선형 결합도와 낮은 p값이 나옴

###########################################################################################################################

#Question4##각각 1개씩 변수 추출 위의 comment에 따라 1학년 국어 성적, 학부모의 감정적 및 학술적 지원, 학급 수로 선택
Stu_3 <- test19%>%filter(Y2KOR_S >= 0 , Y2KOR_S >= 0 , INCOME >= 0 , PEDU >= 0, MEDU >= 0 
                                   , Y2S27_1 >= 0
                                   , Y2S27_2 >= 0
                                   , Y2S27_3 >= 0
                                   , Y2S27_4 >= 0
                                   , Y2S27_5 >= 0
                                   , Y2S27_6 >= 0
                                   , Y2S27_7 >= 0
                                   , Y2S27_8 >= 0
                                   , Y2S27_9 >= 0
                                   , Y2S27_10 >= 0
                                                  )%>%
  mutate(KOR_score = Y2KOR_S, A_sup = (Y2S27_1+ Y2S27_2+ Y2S27_3+ Y2S27_4+ Y2S27_5+ Y2S27_6+ Y2S27_7+ Y2S27_8)/8, 
         E_sup = (Y2S27_9 + Y2S27_10)/2,
         KOR_1stscore = Y1KOR_S,
         parent_sup = (A_sup + E_sup)
  )%>%
  select(KOR_score,parent_sup,KOR_1stscore, CLASS1 )
Stu_3
lm(KOR_score~.,Stu_3)
summary(lm(KOR_score~.,Stu_3))
#3개의 모델로 구성한 결과 1학년 국어 성적을 제외한 2개의 변수는 다소 높은 p값을 가져 의미있는 변수로 생각하기에 근거가 부족하다는 결과가 나왔다.
######################################################################################


#Question5
#최적의 모델 구성
#Backward , Foward, Stepwise Regression

final_data <- test19%>%filter(Y2KOR_S >= 0 , Y2KOR_S >= 0 , INCOME >= 0 , PEDU >= 0, MEDU >= 0 
                , Y2S27_1 >= 0
                , Y2S27_2 >= 0
                , Y2S27_3 >= 0
                , Y2S27_4 >= 0
                , Y2S27_5 >= 0
                , Y2S27_6 >= 0
                , Y2S27_7 >= 0
                , Y2S27_8 >= 0
                , Y2S27_9 >= 0
                , Y2S27_10 >= 0
                , Y2S14_2 >= 0,
                Y2S14_3 >= 0,
                Y2S14_5 >= 0,
                Y2S14_7 >= 0,
                Y2S2_24 >= 0,
                Y2S2_25 >= 0,
                Y2S2_26 >= 0,
                Y2S2_28 >= 0,
                Y2S2_29 >= 0,
                Y2S2_30 >= 0,
                Y2P31_1 >= 0,
                Y1KOR_S >= 0)%>%
  mutate(KOR_score = Y2KOR_S, A_sup = (Y2S27_1+ Y2S27_2+ Y2S27_3+ Y2S27_4+ Y2S27_5+ Y2S27_6+ Y2S27_7+ Y2S27_8)/8, 
         E_sup = (Y2S27_9 + Y2S27_10)/2,
         Book_enjoy = (Y2S14_2+Y2S14_3 +Y2S14_5 +Y2S14_7)/4, 
         Self_con = (Y2S2_24 + Y2S2_25 + Y2S2_26+ Y2S2_28+ Y2S2_29 + Y2S2_30)/6,
         private_edu = Y2P31_1,
         KOR_1stscore = Y1KOR_S,
         parent_sup = (A_sup + E_sup),
         GENDER= GENDER -1 )%>%select(KOR_score,GENDER, Book_enjoy, Self_con,PEDU,MEDU,private_edu,
                                      KOR_1stscore, REGION, SECTOR, COEDU, CLASS1,INCOME,parent_sup)

for( i in 1:nrow(final_data)){
  if(final_data[i,6] == 1){
    final_data[i,6] = 6
  }else if(final_data[i,6] == 2){
    final_data[i,6] = 9
  }else if(final_data[i,6] == 3){
    final_data[i,6] = 12
  }else if (final_data[i,6] == 4){
    final_data[i,6] = 14
  }else if (final_data[i,6] == 5){
    final_data[i,6] = 16
  }else if (final_data[i,6] == 6){
    final_data[i,6] = 18
  }else if (final_data[i,6] == 7){
    final_data[i,6] = 22
  }
}
for( i in 1:nrow(final_data)){
  if(final_data[i,5] == 1){
    final_data[i,5] = 6
  }else if(final_data[i,5] == 2){
    final_data[i,5] = 9
  }else if(final_data[i,5] == 3){
    final_data[i,5] = 12
  }else if (final_data[i,5] == 4){
    final_data[i,5] = 14
  }else if (final_data[i,5] == 5){
    final_data[i,5] = 16
  }else if (final_data[i,5] == 6){
    final_data[i,5] = 18
  }else if (final_data[i,5] == 7){
    final_data[i,5] = 22
  }
}

for(i in 1: nrow(final_data)){
  if(final_data[i,12] <= 2){
    final_data[i,12] = 1
  }else{
    final_data[i,12] = 0
  }
}


final_data <- final_data%>%mutate(parent_edu = PEDU +MEDU)
final_data
#Backward
full.model=lm(KOR_score~.,data= final_data)
reduced.model=step(full.model,direction="backward")

#KOR_score ~ GENDER + Self_con + KOR_1stscore + private_edu



#Foward
step(lm(KOR_score ~1, final_data), scope = list(lower ~ 1, upper = ~GENDER + Book_enjoy+Self_con + private_edu + KOR_1stscore + parent_sup
                                                + REGION + SECTOR + COEDU + CLASS1 + parent_edu + INCOME ), direction = "forward")
#KOR_score ~ GENDER + Self_con + KOR_1stscore + private_edu

#Stepwise
step(lm(KOR_score ~1, final_data), scope = list(lower ~ 1, upper = ~GENDER + Book_enjoy+Self_con + private_edu + KOR_1stscore + parent_sup
                                                + REGION + SECTOR + COEDU + CLASS1 + parent_edu + INCOME ), direction = "both")
#KOR_score ~ GENDER + Self_con + KOR_1stscore + private_edu

#Conclusion 최적의 모델
#KOR_score ~ GENDER + Self_con + KOR_1stscore + private_edu
lm(KOR_score ~ GENDER + Self_con + KOR_1stscore + private_edu,final_data)
summary(lm(KOR_score~(GENDER + Self_con + KOR_1stscore + private_edu),final_data))
# 성별이 여자이고 1학년때 국어성적이 높으며 학업 자아개념이 높으며 사교육을 할 수록 2학년 국어성적이 높다. 
# p값을 고려했을 때, 학업 자아개념 변수와 사교육 여부 변수는 유의 수준 95%안에서 의미있다고 볼 수 없다
plot(0.6328*final_data$KOR_1stscore+4.0201*final_data$GENDER+1.9175*final_data$Self_con + 2.6408*final_data$private_edu,final_data$KOR_score )
data <- test19
data$Y2KOR_S
data$Y2KOR_S_re <-ifelse(data$Y2KOR_S<=20,E,ifelse(20<data$Y2KOR_S<=40,D,ifelse(40<data$Y2KOR_S<=60,C,ifelse(60<data$Y2KOR_S<=80,B,A ))))

data <- data%>%mutate(Y2KOR_S_re = Y2KOR_S)

for(i in 1: length(data$Y2KOR_S_re)){
  if(data$Y2KOR_S_re[i] <= 20){
    data$Y2KOR_S_re[i] = 'E'
  }else if(data$Y2KOR_S_re[i] <= 40){
    data$Y2KOR_S_re[i] = 'D'
  }else if(data$Y2KOR_S_re[i] <= 60){
    data$Y2KOR_S_re[i] = 'C'
  }else if(data$Y2KOR_S_re[i] <= 80){
    data$Y2KOR_S_re[i] = 'B'
  }else if(data$Y2KOR_S_re[i] > 80){
    data$Y2KOR_S_re[i] = 'A'
  }
}

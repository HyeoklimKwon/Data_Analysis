#mutate() 새로운 변수를 만들어서 기존에 있는 변수의 함수로 만든다 ex) v1+v2 = v5
#seclect() filter() 특정 값이 포함된 행 뺍기 summarize() 변수 요약 arrange() 행들을 특정한 기준에 따라 순서 바꾸기
#tibble은 tydtverse에서 데이터프레임 
#as_tibble() 안에 데이터프레임 넣고 돌리면 티블됨
#arrange(flights, year, month, day)
install.packages("dplyr")
install.packages("nycflights13")
library(nycflights13)
library(dplyr)
dim(flights)
flights$day
filter(flights, month == 1, day == 1) 
# 1월 1일 값의 데이터를 꺼낸다. select(flights, tail_num = tailnum) 뽑아냄과 동시에 새로운 이름 지정
#rename(): 변수의 이름을 바꾼다. 더 유용함
select(flights, tail_num = tailnum)
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)
#함수안에서 생성된 변수 바로 사용 가능 -> tranform()은 사용하지 못함
#transmute() 생성된 변수 이외에 사라진다. 출려된 결과물만 보이는거지 기존에 있는 티블을 바꾼건이 아니다. flight자체를 바꾸고 싶으면
# flight <- 에 냏어야한다. 
summarise(flights,
delay = mean(dep_delay, na.rm = TRUE)
)
# sample_n()  랜덤한 n개의 행 추출/ smple_frac() /replace = TRUE- > 복원 추출
sample_frac(flights, 0.01) # 퍼센트 샘플
#group_by() function 특정 그룹별로 데이터 사용할때
# mutate() and filter() are most useful in conjunction with window functions
# 예를 들어, group by (성별) 하면 남자와 여자를 나누고 분석 / summarise() 또한 각각 그룹별로 적용
#count = n() 
by_tailnum <- group_by(flights, tailnum) # tailnum에 대해서 그룹별로 나줘짐 새로운 티블
by_tailnum
delay <- summarise(by_tailnum,
                   count = n(), # we will learn about this in the next slides 그룹 별 숫자를 센다.
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay
delay <- filter(delay, count > 20, dist < 2000)
delay
destionations <- group_by(flights,dest)
destionations
summarise(destionations, planes = n_distinct(tailnum),flights = n()) 
daily <- group_by(flights,year,month,day)
(per_day <- summarise(daily,flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
per_year <- summarise(per_month, flights = sum(flights))
per_year
df <- select(flights, year:dep_time) # year부터 dep_time까지 열 뽑기
df
#dpylr을 이용해서 데이터 처리 piping
#flights %>% # > 방향으로 가라 x % f(y) x라는 것을 f라는 함수에 적용시켜라
a1 <-group_by(flights, year, month, day)
a2 <-select(a1, arr_delay, dep_delay)
a3 <-summarise(a2,arr =mean(arr_delay, na.rm = TRUE),dep =mean(dep_delay, na.rm = TRUE))
a4 <-filter(a3, arr>30|dep>30)  

flights%>%
  group_by(year, month, day)%>%
  select(arr_delay, dep_delay)%>%
  summarise(arr =mean(arr_delay, na.rm = TRUE),
            dep =mean(dep_delay, na.rm = TRUE))%>%
  filter(arr>30|dep>30)


#magrittr
library("tidyverse")
library("magrittr")
# f(x) 보다 x %>% f를 사용한다
car_data <-mtcars%>%
  subset(hp>100)%>% 
  aggregate(.~cyl, data = ., FUN = .%>%mean%>% round(2))%>% # data= . -> place holder 앞에서 처리했던 데이터
  # .~ cyl -> 뒤에서 data 라고 정의했던 데이터 불러오기 # FUN = . %>% function을 만들겠다. 
  # 즉, cyl에 대해서 앞의 데이터를 정리하는데 그 변수 값들은 평균값의 소수점 2자리까지 rounding한 값이다.
  transform(kpl = mpg%>% multiply_by(0.4251))%>% #새로운 열 생성
  print
mtcars
# %<>% 뒤에서%>% 했던 결과물들을 최종적으로 앞의 것에 넣는다 .
  mtcars%<>%
  filter(hp>100)%>%group_by(cyl)%>%
  summarise_all(mean)%>%
  mutate(mpg =round(mpg, 2),kpl =multiply_by(mpg, 0.4251))%>%
  print()
mtcars



##tidyr 매우 중요함!!
library("tidyverse")
#본인이 원하는 모델에 맞는 데이터 처리 데이터 재구성
# variable 속성에대한 값들 / observation 하나의 유닛에 대해 모든 values
# tip. 행간의 관계보다는 열 간의 관계를 생각해봐야한다. 
# 1. 경우 variable이 단지 value일때 (value들의 이름이 아니라) -> gather function 사용
 # gather(처음만들때 header 였던 값의 이름, 안에 있는 값들로 새롭게 정의하고자하는 이름, 무엇에 대해서 합치고 싶은지)
# day1:day3 -> day1 col ~ day3 col
#na.rm =True -> missing value값 없애기
billboard <-  billboard %>%
  gather(week, rank, contains("wk"))
billboard
billboard <- billboard %>%
  mutate(
    wk = paste0("week", parse_number(week))
    # or wk = str_c("week", parse_number(wk))
  )
billboard
billboard <- billboard %>%
  mutate(
    week = parse_number(week),#week 변수에서 숫자만 뽑기 
    date = as.Date(date.entered)+ 7 * (week - 1)) %>% #날짜로 인식하게 해주는 함수  + 7 * (week - 1)) %>%
  select(-date.entered) #date.entered 변수 삭제 / 유용성이 더 좋은 date 변수가 생겼기 때문
billboard
x <- 1999-11-13
y <- as.Date(1999-11-13)
?as.Date
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
x <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
z <- as.Date(x, "%d%b%Y")
z
z+100
library(epitools)
install.packages("epitools")

binom.exact(7,525, conf.level = 0.95)



#seperate() - 한 col 안에 두가지 variable이 있을떄 spereate(분리할 col, c(분리되는 이름1, 이름2), 몇번째 글자부터 분리(-를 붙이면 뒤에서부터 분리))
# 3개일 경우에는 seperate(demo, c(name1,name2,name3)) - default 값으로 특수기호로 분리됨 이름에 NA를 치면 없앨수 있다.
#gsub() - col 값들 character string을 다른 것으로 대체 ex) gsub("new_sp", " ", tb$sex)

#extract_numeric() 대신에 parse_nuber() 쓰기

#spread(적용 요소,밑의 값으로 저장될 col)

#ggplot2
library("mpg")
install.packages("mpg")
library("mpg")
mpg
install.packages("ggplot2")
library("ggplot2")
mpg
ggplot(data = mpg)+#mpg를 가지고 ggplot을 만들것이다.  
  geom_point(mapping = aes(x = displ, y =hwy,color = class, shape = class))+ # class의 variable에따라 다른 색상
  facet_wrap(~class, nrow=3) # subplot 만들기 
ggplot(data = mpg)+
  geom_point(mapping = aes(x= displ, y =hwy))+
  facet_wrap(~class, nrow=3)


ggplot(data = mpg)+
  geom_point(mapping = aes(x= displ, y =hwy))+
  facet_grid(drv~cyl) #어떤 변수의 상관관계에 관해서 그리고 싶을때
mpg$drv

ggplot(data = mpg)+
  geom_smooth(mapping = aes(x=displ, y = hwy)) # 하나의 그래프로 만들고 싶을때
ggplot(data = mpg)+
  geom_smooth(mapping = aes(x = displ, y = hwy), method = lm) # 회귀분석 으로 

#se= True -> confidence band 
# linetype -> 각 변수별로 선의 모양을 바꾼다


ggplot(data = mpg, mapping = aes(x= displ, y = hwy, color = drv))+ # mapping 을 global option으로 변환
  geom_point()+
  geom_smooth(mapping = aes(linetype=drv))+
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE) # 다른 데이터와 비교하고 싶을때 
#결론: 게속해서 한가의 상관관계에 대해서 분석할때 global option으로 두면 편하다
# 4/14 중간고사 4/21까지 중간보고서 prsentation 과 함께 제출 다음주 수요일 끝나고 숙제 
ggplot(data = mpg, mapping = aes(x=displ, y= hwy))+
  geom_point(mapping = aes(color = drv))+
  geom_point(size=4,color = "white")
ggplot(data = mpg, mapping = aes(x=displ, y= hwy))+
  geom_point(size=4,color = "white")+
  geom_point(mapping = aes(color = drv))
diamonds
ggplot(data = diamonds)+
  geom_bar(mapping = aes( x = cut)) # y축의 값은 function안에서 자동으로 계산



#stringr skils

# 문장이나 주석같은 데이터가 주어졌을때 이를 가공하여 처리하는 밥법

library("tidyverse")
#str_length() 문자열의 길이 빈공간까지 다 count
#str_sub(이용할 vector, 몇번째 캐릭터에 접근, 언제까지 접근하겟냐) 문자열에서 특정 캐릭터 접근
x <- c("abcdef", "ghifjk")
# The 3rd letter
str_sub(x, 3, 3) # 3~3까지니까 3번재 값 각각
#str_pad(n, "right") 오른쪽 문자열에 정해진길이만큼 빈광간 넣기
#정해진 길이보다 원래 문자열이 길때 아무것도 작용하지 않음
#str_trunc(10)  10까지 유지하고 나머지는 짤림, 짤리는 부분 = ...("..."포함 10)
#str_dup(x, c(2,3)) - 첫번째는 2번 반복 두번째는 3번 반복
#str_trim() 앞,뒤 빈공간 지우기  str_trim(x, "left") 왼쪽 빈공간 지우기
#str_c("a", "b","c", sep=" ||") 합치기 -> ("a || b || c")
#str_wrap(x, width =40) 한줄에 40 space 주기 
#cat() = print()

#Pattern matching
#1 character string 안에 "를 넣고 싶을떄 ex)  " I " " 그냥 하면 안됨 -> 역슬래쉬를 써야함
" I \" " 
writeLines("I \" ") #컴퓨터가 인식하는 데이터 보기
# \ 는 뒤에 있는 기호 or 문자를 program에서 인식하는 기호가 아니라 본연의 character string으로 돌아가라는 의미
"첫번째 줄\n두번쨰 줄 " # new line
writeLines("첫번째 줄\n두번쨰 줄 ")
"hahah\t하하하" # tab 만큼 공간 띄우기
writeLines("hahah\t하하하")
# 특정한 캐릭터 찾기
install.packages("htmlwidgets")
library(htmlwidgets)
x <- c("apple", "banana.", "pear")
str_view(x, "an")
#첫번째 패턴만 찾아준다. -> 전체다 찾고 싶으면 
str_view_all(x, "an")
# .a. 아무글자a아무글자 패턴 찾기
str_view(x, ".a.")
# .을 아무글자가 아니라 마침표로서 찾기 !\가 아니라 \\ 을 써야한다 이유: 단순히 캐릭터 스트링의 출력이 아니기 때문에 \.자체를 전달해야해서 \\.해야함
str_view(x, "\\.")
str_view(c("abc", "a.c", "bef"), "a\\.c")

#비슷한 원리로, 캐릭터 스트링안에서 \\가 두개가 필요할 경우, \\\\ 앞의 역슬래쉬를 위헤 하나 뒤의 역슬래쉬를 위해 하나 총 4개의 역슬래쉬를 써야한다.
x <- "a\\b"
writeLines(x)
#> a\b

str_view(x, "\\\\")  


#^a a로 시작하는 스트링  a 찾기
x <- c("apple", "banana", "pear", "apples","123")
str_view(x, "^a")
#a$ a로 끝나는 스트링 a 찾기 
str_view(x, "a$")
# 응용하면 apple만 찾기 가능하다
str_view(x, "^apple$")
# 이렇게 쓸수도 있다.
str_view(x, "\\bapple\\b")
#\d: matches any digit.
str_view(x,"\\d3")
#\s: matches any whitespace (e.g. space, tab, newline).
#\w: matches any “word” character
#\b: matches word boundaries.
#[abc]: matches a, b, or c. a b c중에 최소 한개 이상 매칭
#[a-z]: matches every character between a and z. a부터 z까지 사이의 모든 캐릭터 매칭
#[^abc]: matches anything except a, b, or c. a b c 빼고 모든 매칭



#응용
str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c") # [.] 원래는 .은 아무거나 상관없다는 뜻이지만 [.]은 점만을 의미한다. = \\.
str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c") # 아무거나*c 찾기



#Alternation
str_view(c("grey", "gray"), "gr(e|a)y") # gr (e 또는 a) y 찾기

str_view(c("grey", "gray"), "gr[ea]y") #같은 말
# repetition
#?: 0 or 1     0번(원본) 이나 한번 반복되는 것 찾기 앞에 한개만 적용 CC?이면 C가0번 반복 1번 반복인 CC나 CCC를 찾게됨 
#하지만 젤 짧은것을 찾음 (원본이 있으면 원본 우선)
#+: 1 or more  1번이상 반복되는 것까지 찾음 
#*: 0 or more  원본이상 반복되는 것까지 찾음
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+') #C + (L 또는 X) 가 1번이상 반복되는 패턴 찾기
#{n}: exactly n n번 반복
#{n,}: n or more n번이상 반복
#{,m}: at most m 최대 m번 반복
#{n,m}: between n and m n~m번 반복 패턴 찾기
str_view(x, "C{2}") # C가 2번 반복 되는것 CC
str_view(x, "C{2,}") # C가 2번이상 반복 되는것 CC...
str_view(x, "C{2,3}") # C가 2번에서 3번 반복 되는것  이것은 최대 반복 수에 맞춤 


#Grouping and backreferences
str_view(fruit, "(..)\\1", match = TRUE)#\\1의 의미 컴퓨터에선 \1로 의미: 첫번째 그룹을 의미한다. 따라서 (..) <- 그룹 그룹이 하나밖에 없기때문에
# 첫번쨰 그룹은 (..) 따라서 (..)(..)을 찾아라는 뜻이 됨 ex) abab caca dada 찾기
str_view(fruit, "(.)(.)\\2\\1", match = TRUE) #\\2는 두번째 그룹 (.)<- 첫번쨰 그룹 (.) <- 두번째 그룹 (x)(y)(y)(x)를 찾으라는 의미


#응용
strings <- c(
  "apple",
  "219 733 8965",
  "329-293-8753",
  "Work: 579-499-7527",
  "Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- . ]([0-9]{3})[-. ]([0-9]{4})" #첫번째 그룹 (2~9숫자, 0~9숫자, <-얘를 2번 반복) -> (2~9)+(0~9)+(0~9) 
                                                          # 두번째 [- . ] -나 빈공간 . 아무거나 
                                                          # 세번째 (0~9) 3번 반복  (0~9)(0~9)(0~9)
#detect() pattern 이 있는지 없는지 찾기
str_detect(strings, phone)
# 매칭된 패턴 보기
str_subset(strings, phone)

#str_count() 매칭된 패턴이 몇개 있는지 
str_count(strings, phone)

#str_locate() 매칭된 패턴이 어디서 부터 어디까지 일어난건지 알려준다. 그냥 쓰면 매칭된 첫번째 패턴에만 적용 
(loc <- str_locate(strings, phone))
#위 문제를 해결하기 위해 -all을 추가한다
str_locate_all(strings, phone)

#str_extract() 매칭된 패턴 뽑기
# What are the phone numbers?
str_extract(strings, phone)
#마찬가지로 첫번째 패턴에만 적용되기 때문에 -all을 사용할 수 있다.
str_extract_all(strings, phone, simplify = TRUE) #simplofy = TRUE 는 list가 아니라 data.frame형태로 보여줌 가장 긴 스페이스를 기준으로 나타남

#str_match() 매칭된 패턴 그룹별로 뽑기 
str_match(strings, phone)

#str_replace() 매칭된 패턴 바꾸기
str_replace(strings, phone, "XXX-XXX-XXXX") # 매칭된 패턴들을  XXX-XXX-XXXX로 바꾼다.

#str_split() 분할하기
str_split("a-b-c", "-") # -를 기준으로 분할
# 내가 정한 갯수로 분할 
str_split_fixed("a-b-c", "-", n = 2) # -를 기준으로 분할 하지만 2 그룹으로 만들기 앞에서부터 기준을 잡는다.

#응용
# How many common words start with t?
sum(str_detect(words, "^t"))
#> [1] 65

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))
#> [1] 0.2765306

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]") # 모음이 들어간 단어를 찾은 다음 !그것을 뺀다. 

# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$") #자음으로 시작해서 + 한번이상 쭉 반복되고 그걸로 끝나는 단어 찾기
identical(no_vowels_1, no_vowels_2)
#> [1] TRUE

# Select the elements that match a pattern
words[str_detect(words, "x$")] # x로 끝나는 단어를 detect하고 그것을 이용하여 words vetor에서 가져오기 
#> [1] "box" "sex" "six" "tax"
str_subset(words, "x$")
#> [1] "box" "sex" "six" "tax"

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))
#> [1] 1.991837

#패턴 매칭에서는 중첩 즉 곂치는 패턴은 유효하지 않다.
str_count("abababa", "aba")
str_view_all("abababa", "aba")

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|") #합치기 요소 사이의 구분점 | 넣기 
colour_match
# | 는 or의 의미를 지니기 때문에 red" 또는 "orange"또는  "yellow"또는 "green"또는  "blue" 또는 "purple" 찾기를 할 수 있다. 
has_colour <- str_subset(sentences, colour_match) # 문장에서 매칭된 패턴을 가지고 잇는 문장 찾기
has_colour
matches <- str_extract(has_colour, colour_match) # 그 문장들중 패턴 뽑기
head(matches)

noun <- "(a|the) ([^ ]+)" #a나 the로 시작하고 한칸 띄우고 빈공간없이 이어지는 단어 
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>% 
  str_extract(noun)
#숙제는 regular expression 사용하기


#분석하고자는 목표 /데이터 가공 /최종 데이터셋 /각 데이터 셋 /기초통계 혹은 기본적인 통계 분석툴 이용
#data table : 종종 분석하는 데이터가 R에서 사용할때 쓰는 RAM보다 용량이 클 때가 있다. -> 이런 경우 data.table을 사용함
#하드 디스크에서 가져오는 것이 아니기 때문에 작동 가능
#큰 용량뿐이 아니어도 상당히 빠르다. 
#증명

system.time(Airpass <- data.table::fread("SKKU/datasets/AirPassengers.csv"))
Airpass2 <- read.csv("SKKU/datasets/AirPassengers.csv")
system.time(Airpass2 <- read.csv("SKKU/datasets/AirPassengers.csv"))

library(dplyr)

install.packages("data.table")
library(data.table)
set.seed(0)
sample_dt <- data.table(y = rnorm(10000), x1 = rnorm(10000))
class(sample_dt)

Airpass
#data.table doesn’t set or use row names, ever.
##   R:                 i                 j                                                by
## SQL:  where | order by (행)  select | update(열 변수의 이름을 뜻함, 숫자로 쓰면 안됨)   group by
flights <- data.table::fread("C:/Users/khlzz/Downloads/flights14.csv")
flights
ans <- flights[origin == "JFK" & month == 6] 
ans
nrow(ans)
#pipe operator as same answer
flights %>% filter(origin == "JFK" & month == 6) %>% nrow
#행 정리 
ans <- flights[1:2]
ans
#행 1~2 뽑기

ans <- flights[order(origin, -dest)]
head(ans)

ans2 <- flights[order(-dest,origin)]
head(ans2)

#열 정리
#arr_delay 뽑기 
ans <- flights[, arr_delay]
flights
ans
head(ans) #특이 사항 vector로 나옴

#data frame 으로 보고 싶을 때
ans <- flights[, list(arr_delay)]
head(ans)[1]
#===같은 결과 다른 용법 
flights[, .(arr_delay)]


#vs data.frame dataframe[,1] or dataframe["dog"]

head(flights %>% select(arr_delay))

#Selecting both arr_delay and dep_delay columns
ans <- flights[, .(arr_delay, dep_delay)]
ans

#Select both arr_delay and dep_delay columns and rename them to delay_arr and delay_dep.
ans <- flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]
head(ans)

#계산하기
flights[, sum( (arr_delay + dep_delay) < 0 )] # arr_delay+ dep_delay가 0보다 작을 경우를 다 더함

flights[origin == "JFK" & month == 6,   # JFK이고 6달인 행을 뽑은 후
        .(m_arr = mean(arr_delay),      # 그 행의 arr_delay의 평균과 dep_delay의 평균을 구한다. 
          m_dep = mean(dep_delay))]

#행의 개수 세기 
flights[origin == "JFK" & month == 6, .N]
nrow(flights[origin == "JFK" & month ==6])
#data.frame 용법도 사용이 가능하다. 
flights[, c("arr_delay", "dep_delay")]
flights[, !c("arr_delay", "dep_delay")] # 두 컬럼 빼고


#응용
flights[, .(.N), by = .(origin)] # .(.N) 개수 리스트를 만들기/ by = .(origin) # origin 변수에 관해서 group by를 하고 그 갯수를 리스트로 만들어라

flights[carrier == "AA", .N, by = origin] # carrier가 aa이고 이를 origin에 대하여 group by를 하고 그 개수
#순서는 가장 먼저 발견한 것부터 순서대로 기록 

ans <- flights[carrier == "AA", .N, by = .(origin, dest)] # carrier가 aa이고 이를 origin과 dest 그 combination에 대하여 group by를 하고 그 개수
ans

flights[carrier == "AA",                      #carrier가 aa이고 이를 origin dest month의 combination에 대하여 group by를 하고 그것들의 arr_delay와
        .(mean_arr= mean(arr_delay),mean_dep =  mean(dep_delay)),  # dep_delay의 평균 
        by = .(origin, dest, month)]

#sorting 하기 "keyby"
flights[carrier == "AA",                                         # 먼저 origin에 대하여 sorting 후 dest, month 순으로 sort
        .(mean_arr= mean(arr_delay),mean_dep =  mean(dep_delay)),
        keyby = .(origin, dest, month)]


#Chaining [][]에 작업 순서대로 하기
ans <- flights[carrier == "AA", .N, 
               by = .(origin, dest)
][order(origin, -dest)]    #origin과 dest에 대하여 group by한후 origin 오름차순 dest 내림차순으로 정렬
head(ans, 10)

#.SD (SubData)
#DT[, print(.SD), by = ID] 아래와 같이 sub data table을 group by에 의해서 만든다. 
##    a b  c
## 1: 1 7 13
## 2: 2 8 14
## 3: 3 9 15
##    a  b  c
## 1: 4 10 16
## 2: 5 11 17
##    a  b  c
## 1: 6 12 18


#응용
flights[carrier == "AA",                       ## Only on trips with carrier "AA"인 행 뽑기
        lapply(.SD, mean),                     ## compute the mean 이에 대해 평균을 계산하는데
        by = .(origin, dest, month),           ## for every 'origin,dest,month' 뽑은 행을 origin dest month에 대해서 group by함
        .SDcols = c("arr_delay", "dep_delay")] ## for just those specified in .SDcols arr_dealy와 dep_dely만


#DT[, .SD[c(1, .N)], by = ID] .SD로 만들어진 그룹 또한 data table이기 때문에 [c(1,.N)]은 첫번째 행과 길이의 마지막 행을 뽑곗다는 뜻 sub data별로
##    ID a  b  c
## 1:  b 1  7 13  ID가 b그룹의 첫번째 행
## 2:  b 3  9 15  ID가 b그룹의 마지막 행
## 3:  a 4 10 16
## 4:  a 5 11 17
## 5:  c 6 12 18
## 6:  c 6 12 18


#기존 데이터테이블에 추가 혹은 업데이트하기
flights[ ,.(m_arr = 60 * arr_delay)] #이렇게 하면 m_arr만 나오지만

flights[ ,m_arr := 60 * arr_delay] # :=를 하면 기존에 있으면 업데이트가 되고 없다면 새롭게 추가한다.
flights
#복수의 변수 추가 혹은 업데이트
flights[ ,c("m_arr", "m_dep") := list(60 * arr_delay, 60 * dep_delay)]
flights

#Set key
#임의의 data table 생성
DT <- data.table(V1=c(1L,2L),              
                 V2=LETTERS[1:3],
                 V3=round(rnorm(4),4),
                 V4=1:12)
DT
#특이 사항 변수에 대해서 setkey를 할떄 자동적으로 sort됨
setkey(DT, V2)
DT["A"]

#응용
DT["A",mult="first"]   # Return first row of all rows that match value A in key column V2
DT["A",mult="last"]    # Return last row of all rows that match value A in key column V2

#복수 setkey sorting 순서는 먼저 적은 순서대로

setkey(DT,V1,V2)    # Sort by V1 and then by V2 within each group of V1 (invisible)
DT[.(2,"C")]     # Select rows that have value 2 for the first key (V1) and the value C for the second key (V2)
DT[.(2,c("A","C"))]  

##function## 
install.packages("data.table")
library(data.table)

#switch function
switch_op <- function(x, y, op) {
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("Unknown op!")
  )
}
switch_op(1,3, "plus")

# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95)  #default 값으로 confidence level = 0.95
  { 
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)

#silent error
wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_mean(1:6, 1:3) # 오류가 발생해야하는데 발생하지 않음

#따라서 검정하는 파트를 만들어야한다
wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}


wt_mean(1:6, 1:3)


#using stopifnot

wt_mean <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1) #이 조건을 충족시키지않으면 멈춤
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}
wt_mean(1:6, 6:1, na.rm = "foo")


##lubriate 날짜와 시간##
library(tidyverse)
library(lubridate)
library(nycflights13)

now()
#from strings
ymd("2017-01-31")
#> [1] "2017-01-31"
mdy("January 31st, 2017")
#> [1] "2017-01-31"
dmy("31-Jan-2017")
#> [1] "2017-01-31"
flights
ymd_hms("2017-01-31 20:11:59")
#> [1] "2017-01-31 20:11:59 UTC"
mdy_hm("01/31/2017 08:01")
#> [1] "2017-01-31 08:01:00 UTC"
ymd(20170131, tz = "UTC")
#> [1] "2017-01-31 UTC"

#from individual
flights %>% 
  select(year, month, day, hour, minute)

flights_dt <- flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

flights_dt %>% 
  ggplot(aes(departure)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day
# geom_freqpoly는 빈번도 

flights_dt %>% 
  filter(departure < ymd(20130102)) %>%  #13년 1월 2일 전
  ggplot(aes(departure)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes
flights_dt

#Homework solution
billboard <- read_csv("~/SKKU/datasets/billboard.csv")

billboard
billboard2 <- gather(billboard,wk,rank,contains("week"))
head(billboard2)
billboard3 <- mutate(billboard2,
                     wk = paste("week" , parse_number(wk))
)
billboard4 <- rename(
  billboard3,
  artist = artist.inverted
)
str_view(billboard3$artist.inverted,"([^ ]+), ([^ ]+)")

billboard4$artist<- str_replace(billboard4$artist,"([^ ]+), ([^ ]+)", "\\2 \\1")
#아무거나 ,  아무거나 패턴 찾는다. 그거를 2번째 요소와 1번째 요소로 바꾼다.

separate(billboard4$time,c("min","sec","micro"),sep = ':')
time2<- separate(billboard4,time,c("min","sec","micro"),sep = ':')
billboard4$time <- as.numeric(time2$min)*60+as.numeric(time2$sec)
billboard4%>%
  group_by(artist,track)%>%
  filter(rank == min(rank, na.rm = T))%>%
  select(highest_rank = rank, highest_date = date.peaked, genre, time)

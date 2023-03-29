# Do it! R 텍스트 마이닝 

############### 1단원: 단어 빈도 분석 #########################
### 무엇을 강조했을까? ###

### 텍스트 전처리 
setwd("C:/Users/user/OneDrive/바탕 화면/전공 수업 자료/텍스트마이닝_안정용")

#데이터 불러오기
raw_moon = readLines("Doit_TM_Data/Data/speech_moon.txt" , encoding = "UTF-8")
head(raw_moon)

library(stringr) # str_replace_all()
## 불필요한 문자 제거 
# str_replace_all(string= 처리할 텍스트, pattern = 규칙, replacement = 바꿀 문자)
# [^가-힣]: 한글이 아닌 모든 문자

### 보충 학습 ###
txt = "치킨은!! 맛있다. xyz 정말 맛있다!@#"
txt
txt = str_replace_all(string = txt , pattern = "[^가-힣]" , replacement = " ")
### 보충 학습 끝###

moon = raw_moon %>% str_replace_all("[^가-힣]" , " ")
head(moon)

## 연속된 공백 제거
# str_squish(): 연속된 공백을 하나의 공백으로 남겨줌
str_squish(txt)

moon = moon %>% str_squish()
head(moon)

## 티블 구조로 변환(보고 다루기 쉬워진다.)
library(dplyr)  # as_tibble()
moon = as_tibble(moon)
moon

## 전처리 작업 함수
precleansing = function(raw_data){
  data = str_replace_all(raw_data , "[^가-힣]" , " ")  ## 한글만 남김
  data = str_squish(data) ## 연속된 공백 제거
  data = as_tibble(data)  ## tibble로 변환
  return(data)
}

precleansing(raw_moon)

### 토큰화 하기
## 텍스트의 기본 단위(단락, 문장, 단어, 형태소..) -> 토큰
## 토큰화:  전처리 후 텍스트를 분석 목적에 따라 토큰으로 나누는 작업
## unnest_tokens(input = 토큰화할 텍스트 , output = 토큰을 담을 변수명 , token = 텍스트를 나누는 기준)
## token = "sentences"/"words"/"charactiers"/"ngrams"
## tibble/data frame 구조의 변수만 입력으로 넣을 수 있음

library(tidytext)  #unnest_token()
### 보충학습 ###
text = tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text
# 문장 기준 토큰화
text %>% unnest_tokens(input = value ,output =  word , token = "sentences")
# 띄어쓰기 기준 토큰화
text %>% unnest_tokens(input = value , output = word , token = "words")
# 문자 기준 토큰화
text %>% unnest_tokens(input = value , output = word, token = "characters")
### 보충학습 끝 ### 

# 띄어쓰기 기준 토큰화
word_space = moon %>% unnest_tokens(input = value , output = word , token= "words")
word_space

# 토큰화 함수 만들기 
token_func = function(df, value , token_criter){
  after_token = df %>% unnest_tokens(input = value , output = word , token = token_criter)
  return(after_token)
}

token_func(moon, value ,token_criter =  "words")

### 단어 빈도 분석하기
## 단어 빈도 분석: 단어가 텍스트에 몇 번 사용되었는지 알아보는 분석 방법
## 글쓴이가 무엇을 강조했는지 알 수 있다.
library(dplyr) # count(), 개수를 세준다.

# 변수 word를 기준으로 세주고, 내림차순으로 정렬해줘
word_space = word_space %>% count(word , sort=T )
word_space

# 한 글자로 된 단어(조사)는 의미없으므로 제거
# str_count(): 문자열의 글자수를 구함
str_count("사과") ; str_count("배")
#filter(): 조건에 맞는 행 추출
word_space = word_space %>% filter(str_count(word) > 1)

# 단어 빈도를 구하는 함수, 조사 생략
term_freq = function(after_token, word){
  output = after_token %>% count(word , sort =T ) %>% # 빈도 수 내림차순 정렬
    filter(str_count(word) > 1 ) # 글자 수 세서 조사 생략
  return(output)
}

word_space = term_freq(word_space , word_space$word)
word_space

## 빈도 분석: 자주 사용된 단어 추출

top20 = word_space%>% head(20)
top20

## 자주 사용된 단어 함수로 만들기
top_freq = function(data , n){
  top_n = data %>% head(n)
  return(top_n)
}

top20 = top_freq(word_space,20)
top20

### 막대그래프 만들기 
# ver.1
library(ggplot2)
ggplot(top20 , aes(x = reorder(word,n) , y = n)) + # n을 기준으로 내림차순으로 재정렬해줌
  geom_col() + # 막대그래프(y축을 빈도수가 아니라 지정해줌)
  coord_flip() + # 회전
  geom_text(aes(label = n) , hjust = -0.3) + # 막대 옆에 빈도표시(hjust: 막대와의 거리)
  labs(title = "문재인 대통령 출마 연설문 단어 빈도", x = NULL , y= NULL) + #제목/축 이름 지정
  theme(title = element_text(size = 12)) # 제목 크기 설정

# ver.2
barplot(top20$n , horiz = TRUE , names.arg =  top20$word , cex.names =  0.6 , las =1)

BarChart = function(top20){
  ggplot(top20 , aes(x = reorder(word,n) , y = n)) + # n을 기준으로 내림차순으로 재정렬해줌
    geom_col() + # 막대그래프(y축을 빈도수가 아니라 지정해줌)
    coord_flip() + # 회전
    geom_text(aes(label = n) , hjust = -0.3) + # 막대 옆에 빈도표시(hjust: 막대와의 거리)
    labs(title = "문재인 대통령 출마 연설문 단어 빈도", x = NULL , y= NULL) + #제목/축 이름 지정
    theme(title = element_text(size = 12)) # 제목 크기 설정
}

BarChart(top20)

### 워드 클라우드 만들기     
# 워드 클라우드: 단어 빈도를 구름 모양으로 표현한 그래프

## 구글 폰트 불러오기 
library(showtext) # font_add_google(name= 불러올 구글 폰트 , family= 지정할 이름)

# 구글폰트에서 나눔고딕 폰트를 불러와 "nanumgothic"이름으로 저장
font_add_google(name = "Nanum Gothic" , family = "nanumgothic")
#font_add_google(name = "Black Han Sans" , family = "blackhansans")
showtext_auto() # R에서 폰트 활용
library(ggwordcloud) # geom_text_wordcloud()

#demo("colors")

ggplot(word_space , aes(label = word , size = n , col = n)) +
  geom_text_wordcloud(seed = 1234, family = "nanumgothic") +  # 워드클라우드 생성(난수 고정), 폰트 적용
  scale_radius(limits = c(3,NA), range = c(3,30)) + # (최소,최대)단어빈도/ (최소,최대)글자크기
  scale_color_gradient(low = "indianred1" , high = "red2") + # (최소,최대)빈도 색깔
  theme_minimal()  # 배경없는 테마

# low = "#66aaf2", high = "#004EA1"

# 워드클라우드를 그려주는 함수
WordCloud_func = function(word_space){
  ggplot(word_space , aes(label = word , size = n , col = n)) +
    geom_text_wordcloud(seed = 1234, family = "nanumgothic") +  # 워드클라우드 생성(난수 고정), 폰트 적용
    scale_radius(limits = c(3,NA), range = c(3,30)) + # (최소,최대)단어빈도/ (최소,최대)글자크기
    scale_color_gradient(low = "indianred1" , high = "red2") + # (최소,최대)빈도 색깔
    theme_minimal()  # 배경없는 테마
}

WordCloud_func(word_space)

## 추가: 폰트를 바꿔 막대그래프 그리기
font_add_google(name = "Gamja Flower" , family = "gamjaflower")
showtext_auto()

ggplot(top20 , aes(x= reorder(word, n ) , y = n)) +
  geom_col() +
  coord_flip() +  
  geom_text(aes(label = n) , hjust = -0.3) + 
  
  labs(title = "문재인 대통령 출마 선언문 단어 빈도" , x = NULL , y= NULL) + 
  theme(title = element_text(size = 12) , text = element_text(family = "gamjaflower")) #폰트 적용

######### 1단원 단어 빈도 분석 정리 #############

# 전처리 
precleansing = function(raw_data){
  out = raw_data %>% str_replace_all("[^가-힣]" , " ") %>%  # 한글만 남기기
      str_squish() %>%   # 연속된 공백 제거
      as_tibble()    # tibble로 변환
  return(out)
}

# 토큰화
tokenization = function(moon){
  token = moon %>% unnest_tokens(input = value , output = word , token = "words") # 띄어쓰기를 기준으로 토큰화
  return(token)
}

# 단어 빈도 구하기
term_freq = function(word_space){
  freq = word_space %>% count(word , sort = T) %>%  # word별로 숫자를 세서 내림차순 정렬
    filter(str_count(word) > 1) # word의 길이가 1이상인 행만 추출(조사 제거)
  return(freq)
}

# 상위 20개 단어 추출
top = function(word_space){
  top20 = head(word_space, 20)
  return(top20)
}

# 막대그래프 생성
barplot_func = function(top20){
  ggplot(top20 , aes(x = reorder(word,n) , y = n)) + # n을 기준으로 내림차순으로 재정렬해줌
    geom_col() + # 막대그래프(y축을 빈도수가 아니라 지정해줌)
    coord_flip() + # 회전
    geom_text(aes(label = n) , hjust = -0.3) + # 막대 옆에 빈도표시(hjust: 막대와의 거리)
    labs(title = "연설문 단어 빈도", x = NULL , y= NULL) + #제목/축 이름 지정
    theme(title = element_text(size = 12)) # 제목 크기 설정
}

# 워드클라우드 생성
wordcloud_func= function(word_space){
  ggplot(word_space , aes(label = word , size = n , col = n)) +
    geom_text_wordcloud(seed = 1234, family = "nanumgothic") +  # 워드클라우드 생성(난수 고정), 폰트 적용
    scale_radius(limits = c(3,NA), range = c(3,30)) + # (최소,최대)단어빈도/ (최소,최대)글자크기
    scale_color_gradient(low = "indianred1" , high = "red2") + # (최소,최대)빈도 색깔
    theme_minimal()  # 배경없는 테마
}

moon = precleansing(raw_moon)  # 전처리
word_space = tokenization(moon) # 토큰화
word_space = term_freq(word_space) # 단어 빈도 구하기기
top20 = top(word_space) # 상위 20개 단어 추출
barplot_func(top20) # 막대그래프
wordcloud_func(word_space) # 워드클라우드

###### 1단원 연습문제 #########
raw_park = readLines("Doit_TM_Data/Data/speech_park.txt" , encoding = "UTF-8")
park = precleansing(raw_park)
word_space = tokenization(park)
word_space = term_freq(word_space)
top20 = top(word_space)
top20 %>% print(n= Inf) # 생략 없이 전부 보이기
barplot_func(top20)
wordcloud_func(word_space)



######################### 2단원: 형태소 분석기를 이용한 단어 빈도 분석 ########################
# 토큰화는 의미 단위 기준으로 해야 텍스트에서 무엇을 강조하는지 알 수 있다.
# 형태소: 의미를 가진 가장 작은 말의 단위
# 형태소 분석: 문장에서 형태소를 추출해 명사, 동사, 형용사 등 품사로 분류하는 작업
# 일반적으로 형태소를 기준으로 토큰화를 한다.(명사를 보면 텍스트가 무엇에 관한 내용인지 파악 가능)

library(KoNLP) # 의존성 패키지, 형태소 분석기, extractNoun()
# extractNoun(): 텍스트의 형태소를 분석해 명사를 추출하는 함수

### 보충 학습 ### 
text = tibble(value = c("대한민국은 민주공화국이다." ,
                        "대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다."))
text
extractNoun(text$value) # 각 행에 대한 형태소들을 리스트에 저장
### 보충 학습 끝 ###

# 토큰화에 형태소 분석함수(문자열이 아니므로 따옴표 생략)를 넣어서 실행
text %>% unnest_tokens(input = value , output = word , token = extractNoun)

### 연설문에서 명사 추출
raw_moon = readLines("Doit_TM_Data/Data/speech_moon.txt" , encoding = "UTF-8")

#기본 전처리
library(textclean)
moon = raw_moon %>% 
  str_replace_all("[^가-힣]" , " ") %>% 
  str_squish() %>% 
  as_tibble()

# 명사 기준 토큰화
word_noun = moon %>% unnest_tokens( input = value , output = word , token = extractNoun)
word_noun

# 명사 빈도 분석
word_noun = word_noun %>% count(word , sort = T) %>%
  filter(str_count(word) >1 )
word_noun

# 상위 20개 추출
top20 = word_noun %>% head(20)
top20

# 막대그래프 만들기
font_add_google(name = "Nanum Gothic" , family = "nanumgothic")
showtext_auto()

ggplot(top20 , aes(x = reorder(word , n) , y= n )) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = n) , hjust = -0.3) +
  labs(title = "문재인 대통령 연설문 명사 빈도 분석" , x = NULL) + 
  theme(text = element_text(family = "nanumgothic"))

# 워드 클라우드 만들기 
font_add_google(name = "Black Han Sans" , family = "blackhansans")
showtext_auto()

ggplot(word_noun , aes(label = word , size = n , col = n ))+
  geom_text_wordcloud( seed = 1234 , family = "blackhansans") +
  scale_radius(limits = c(3,NA) , range= c(3,15)) +
  scale_color_gradient(low = "#66aaf2" , high = "#004EA1") +
  theme_minimal()

### 특정 단어가 서용된 문장 살펴보기

# 문장 기준 토큰화
sentences_moon = raw_moon %>% 
  # 특수문자 제거하지 않음(문장의 기준점이 되는 마침표로 문장을 구분해야하기 때문에)
  str_squish() %>% 
  as_tibble() %>% 
  unnest_tokens(input = value , output = sentence , token = "sentences")

sentences_moon  

# str_detect(): 문장에 특정 단어가 들어있는지 확인,  TRUE/FALSE 출력
### 보충학습 ###
str_detect("치킨은 맛있다" , "치킨")
str_detect("치킨은 맛있다" , "피자")
### 보충학습 끝 ###

# 국민이 들어있는 문장만 출력
sentences_moon %>% 
  filter(str_detect(sentence , "국민"))

# 일자리가 들어있는 문장만 출력 =
sentences_moon %>% 
  filter(str_detect(sentence , "일자리"))

########## 2장: 형태소 분석기를 이용한 단어 빈도 분석 정리 ###################

# 전처리 
precleansing = function(raw_data){
  out = raw_data %>% str_replace_all("[^가-힣]" , " ") %>%  # 한글만 남기기
    str_squish() %>%   # 연속된 공백 제거
    as_tibble()    # tibble로 변환
  return(out)
}

# 명사 기준 토큰화
noun_tokenization = function(moon){
  token = moon %>% unnest_tokens( input = value , output = word , token = extractNoun)
  return(token)
}

# 문장 기준 토큰화
sentence_tokenization = function(raw_data){
  out = raw_data %>% str_squish() %>% as_tibble() %>% 
    unnest_tokens(input = value , output = sentence , token = "sentences")
  return(out)
}

# 특정 단어가 들어간 문장
in_term = function(sentences_moon , noun){
  senten = sentences_moon %>% filter(str_detect(sentence , noun))
  return(senten)
}

# 단어 빈도 구하기
term_freq = function(word_noun){
  freq = word_noun %>% count(word , sort = T) %>%  # word별로 숫자를 세서 내림차순 정렬
    filter(str_count(word) > 1) # word의 길이가 1이상인 행만 추출(조사 제거)
  return(freq)
}

# 상위 20개 단어 추출
top = function(word_noun){
  top20 = head(word_noun, 20)
  return(top20)
}

# 막대그래프 생성
barplot_func = function(top20){
  ggplot(top20 , aes(x = reorder(word,n) , y = n)) + # n을 기준으로 내림차순으로 재정렬해줌
    geom_col() + # 막대그래프(y축을 빈도수가 아니라 지정해줌)
    coord_flip() + # 회전
    geom_text(aes(label = n) , hjust = -0.3) + # 막대 옆에 빈도표시(hjust: 막대와의 거리)
    labs(title = "연설문 단어 빈도", x = NULL , y= NULL) + #제목/축 이름 지정
    theme(title = element_text(size = 12)) # 제목 크기 설정
}

# 워드클라우드 생성
wordcloud_func= function(word_space){
  ggplot(word_noun , aes(label = word , size = n , col = n)) +
    geom_text_wordcloud(seed = 1234, family = "nanumgothic") +  # 워드클라우드 생성(난수 고정), 폰트 적용
    scale_radius(limits = c(3,NA), range = c(3,30)) + # (최소,최대)단어빈도/ (최소,최대)글자크기
    scale_color_gradient(low = "indianred1" , high = "red2") + # (최소,최대)빈도 색깔
    theme_minimal()  # 배경없는 테마
}

####### 2단원 연습문제 ######
raw_park = readLines("Doit_TM_Data/Data/speech_park.txt" , encoding = "UTF-8")
park = precleansing(raw_park) # 전처리
word_noun = noun_tokenization(park) # 명사 기준 토큰화
word_noun = term_freq(word_noun) # 명사 기준 단어 빈도
top20 = top(word_noun) # 자주 사용된 상위 20개 단어 빈도
barplot_func(top20) # 상위 20개 단어 빈도의 막대그래프
sentences_park = sentence_tokenization(raw_park) # 문장 기준 토큰화
in_term(sentences_park , "경제") # 경제가 사용된 문장 출력



##################### 3장: 비교분석 #######################
# 여러 텍스트를 비교해 차이를 알아보자
# 여러 텍스트를 비교하려면 우선 각 텍스트들을 하나의 데이터셋으로 합쳐야 함.

# 문재인 대통령 연설문 
raw_moon = readLines("Doit_TM_Data/Data/speech_moon.txt" , encoding = "UTF-8")
# 연설문 구분을 위해 대통령 변수 추가
moon = raw_moon %>% as_tibble() %>% mutate(president = "moon")

# 박근혜 대통령 연설문
raw_park = readLines("Doit_TM_Data/Data/speech_park.txt" , encoding = "UTF-8")
park = raw_park %>% as_tibble() %>% mutate(president = "park")

### 데이터 합치기
# bind_rows(): 두 데이터를 행 방향(위아래)으로 결합

bind_speeches = bind_rows(moon , park) %>% select(president , value) #변수 순서 바꿈
head(bind_speeches)
tail(bind_speeches)

### 집단별 단어 빈도 구하기

# 전처리 
speeches = bind_speeches %>% 
      mutate(value = str_replace_all(value , "[^가-힣]" , " "),  # df에서 특정 열만 전처리
         value = str_squish(value))

# 토큰화 
speeches = speeches %>% unnest_tokens(input = value , output = word , token = extractNoun)

# 하위 집단별 단어 빈도 구하기
### 보충 학습 ###
df = tibble(class = c("a" , "a" , "a" , "b" , "b" , "b") , 
            sex = c("female" , "male" , "female" , "male" , "male" , "female"))
df
df%>% count(class, sex)  # (변수1 , 변수2) 로 카운트
### 보충 학습 끝 ### 

frequency = speeches %>% 
  count(president , word) %>%  #( 대통령, 단어) 별 빈도 
  filter(str_count(word) > 1) # 두 글자 이상인 단어만 추출

### 자주 사용된 단어 추출
# slice_max(value , n): 값이 큰 상위 n개의 행을 추출해 내림차순으로 정렬
### 보충 학습 ### 
df= tibble(x= 1:100)
df %>% slice_max(x, n=3) # 가장 큰 3개값만 내림차순 정렬되어 출력

top10 = frequency %>% 
  group_by(president) %>%  # 연설문별로 집단 구분
  slice_max(n , n=10) # 빈도 수를 기준으로 최대 10개만 추출(집단별로)

top10

top10 %>% filter(president == "park") %>% print(n = Inf) # 빈도가 같은 명사까지 추출되어 10개가 넘음

### 동점 단어를 제외하여 자주 사용된 단어 추출
### 보충 학습 ###
df = tibble(x = c("A" , "B" , "C" , "D") , y = c(4,3,2,2)) 
df
df %>% slice_max(y , n=3)
df %>% slice_max(y , n=3, with_ties = F) # 동점이 있어도 정한 n에 개수를 맟춘다.
### 보충 학습 끝 ### 

top10 = frequency %>% group_by(president) %>% slice_max(n , n=10 , with_ties = F) # 각 연설문에서 10개씩만 출력
top10

### 변수의 항목별로 그래프 만들기
ggplot(top10 , aes(x = reorder(word , n ) , y  = n , fill = president)) + # 막대 색깔 대통령별로 채움
  geom_col() +
  coord_flip() + 
  facet_wrap(~president , scale = "free_y") # 대통령 별로 그래프 나눔, y값도 그래프 별로 따로 만든다.

### 특정 단어를 제거하고 막대그래프 만들기
# 박근혜 대통령 그래프에서 국민의 빈도가 너무 높아 다른 단어들의 차이가 잘 드러나지 않는다. 
# reorder_within(x= 축, by = 정렬 기준 , within = 그래프 나누는 기준): 축 순서를 변수의 항목별로 따로 구함
# scale_x_reordered() : 각 단어 뒤에 집단이 붙어있음 -> 집단(항목) 이름 제거

top10 = frequency %>% 
  filter(word != "국민") %>%  # 국민이 아닌 행만 추출
  group_by(president) %>% 
  slice_max(n , n=10 , with_ties =  F)

top10

font_add_google(name = "Nanum Gothic" , family = "nanumgothic")
showtext_auto()

library(tidytext)
ggplot(top10 , aes(x = reorder_within(word, n , president ), y= n , fill = president)) +  # 집단 내 정렬
  geom_col() + 
  coord_flip() + 
  facet_wrap(~president , scales = "free_y") + 
  scale_x_reordered() +  # x축 뒤 집단 이름 제거
  labs(x = NULL) +  # x축 이름 제거 
  theme(text = element_text(family = "nanumgothic"))


### 오즈비 구하기
# 일반적으로 자주 사용하는 단어의 빈도가 높다 -> 두 연설문의 차이가 잘 드러나지 않는다.
# 단순히 빈도가 높기만한 단어는 특징이 없기 때문에 문서의 차이를 드러내지 못한다.
# 텍스트를 비교할 때는 "특정 텍스트에는 많이 사용되었지만 다른 텍스트에는 적게 사용된 단어, 
# 즉 상대적으로 많이 사용된 단어" 를 살펴야 한다.

### 보충학습 ###
# long form을 wide form으로 변환 
df_long = frequency %>% 
  group_by(president) %>% 
  slice_max(n , n=10 ) %>%
  filter(word %in% c("국민" , "우리" , "정치" , "행복")) # 위 단어만 추출

df_long # 같은 단어가 범주(president)별로 다른 행을 구성

#### wide form으로 변환
# pivot_wider(names_from = 변수명으로 만들 값이 들어있는 변수, values_from = 변수에 채울 값이 들어있는 변수)
library(tidyr)  # pivot_wider()
df_wide = df_long %>% pivot_wider(names_from = president , values_from = n , values_fill = list(n=0)) # NA->0으로 변경
df_wide
### 보충학습 끝 ###

frequency_wide = frequency %>% 
  pivot_wider(names_from = president , values_from = n , values_fill =list(n=0))
frequency_wide

### 오즈비 구하기
# 오즈비: 어떤 사건이 A조건에서 발생할 확률이 B조건에서 발생할 확률에 비해 얼마나 더 큰지 나타낸 값
# 상대적인 중요도를 알 수 있다. p(X|A)/p(X|B)
# 어떤 단어가 한 연설문에서 전혀 사용되지 않으면 빈도 0 , 오즈비도 0이므로 모든 값에 1을 더해준다.
frequency_wide = frequency_wide %>% 
    mutate(ratio_moon = (moon+1)/(sum(moon+1)), 
           ratio_park = (park+1)/(sum(park+1)))  

# 단어가 어떤 텍스트에서 상대적으로 더 많이 사용되었는지 알 수 있다.
# moon에서 상대 비중이 클수록 1 , park에서 상대 비중이 클수록 0 , 비중이 비슷할수록 1
frequency_wide = frequency_wide %>% 
    mutate(odds_ratio = ratio_moon/ratio_park)

# 오즈비가 클수록 moon에 더 많이 사용되었다.
frequency_wide %>% arrange(-odds_ratio) #arrange(): 오름차순 정렬 함수
# 오즈비가 작을수록 park에 더 많이 사용되었다.
frequency_wide %>% arrange(odds_ratio)

# B에 대한 A의 odds ratio = [(n+1)/(total+1)]_A / [(n+1)/(total+1)]_B

## 오즈비 구하기
frequency_wide %>% mutate(odds_ratio = ((moon+1)/sum(moon+1))/((park+1)/sum(park+1)))

### 상대적으로 중요한 단어 추출
## 오즈비가 가장 높거나/낮은 단어 추출
# rank(): 값이 작은 순(오름차순)으로 순위
top10 = frequency_wide %>% 
    filter(rank(odds_ratio) <=10 | rank(-odds_ratio) <=10) # 상위 10개, 하위 10개

top10 %>% arrange(-odds_ratio) %>% print(n = Inf) # 정렬

### 보충학습 ### 
df = tibble(x= c(2,5,10))
df
df %>% mutate(y = rank(x)) # 오름차순으로 순위
df %>% mutate(y = rank(-x)) # 내림차순으로 순위
### 보충학습 끝

### 비중이 큰 연설문을 나타낸 변수 추가하기
top10 = top10 %>% mutate(president = ifelse(odds_ratio >1 , "moon" , "park") , 
                         n = ifelse(odds_ratio >1 , moon , park)) # 비중이 큰 연설문에서 단어 빈도수 가져오기 

### 막대그래프 만들기
ggplot(top10 , aes(x = reorder_within(word , n, president ) , 
                   y= n , fill =president)) + 
  geom_col() + 
  coord_flip() + 
  facet_wrap(~president , scales = "free") +  ## x축, y축 크기를 그래프별로 따로 지정
  scale_x_reordered()
# x축의 크기가 그래프마다 다르면 막대 길이가 같아도 실제 값은 다르기 때문에 해석할 때 주의
# 각 텍스트에서 상대적으로 중요한 단어가 무엇인지만 살펴보기

### 주요 단어가 사용된 문장 살펴보기
# 문장 기준 토큰화
speeches_sentence = bind_speeches %>% 
    as_tibble() %>% 
    unnest_tokens(input = value , output = sentence , token = "sentences")

head(speeches_sentence)
tail(speeches_sentence)

# 주요 단어가 사용된 문장 추출
speeches_sentence %>% filter(president == "moon" & str_detect(sentence , "복지국가"))
speeches_sentence %>% filter(president == "park" & str_detect(sentence , "행복"))

# 중요도가 비슷한 단어 살펴보기(로그 오즈비가 1에 가까운 단어)
frequency_wide %>%  arrange(abs(1-odds_ratio)) %>% head(10)

# 중요도가 비슷하면서 빈도가 높은 단어 추출
frequency_wide %>% 
  filter(moon >= 5 & park >=5 ) %>%  # 두 연설문에 5번 이상 사용
  arrange(abs(1-odds_ratio)) %>%  # 오즈비가 1에 가까운 단어
  head(10)

### 로그 오즈비로 단어 비교하기
# log(x): 1보다 큰 값은 양수, 1보다 작은 값은 음수
# moon에 비중이 커서 오즈비>1 인 값은 양수 , park에 비중이 커서 오즈비<1 인 값은 음수
# 상대적 비중이 큰지에 따라 서로 다른 부호를 갖게된다.
# log odds ratio =  log([(n+1)/(total+1)]_A / [(n+1)/(total+1)]_B)

# 큰 양수일수록 moon에 비중이 크고 , 작은 음수일수록 park에 비중이 크고, 0에 가까울수록 비중이 비슷하다. 
frequency_wide = frequency_wide %>% mutate(log_odds_ratio = log(odds_ratio))

frequency_wide %>% arrange(log_odds_ratio) # 오름차순 정렬 ,   park에 비중이 큰 단어
frequency_wide %>% arrange(-log_odds_ratio) # 내림차순 정렬, moon에 비중이 큰 단어
frequency_wide %>% arrange(abs(log_odds_ratio)) # 비중이 비슷한 단어

## 로그 오즈비 정리
frequency_wide = frequency_wide %>% 
    mutate(log_odds_ratio = log(((moon+1)/ sum(moon+1))/((park+1)/sum(park +1 ))))

## 로그 오즈비를 이용해 중요 단어 비교하기
top10 = frequency_wide %>% group_by(president = ifelse(log_odds_ratio >0 , "moon" , "park")) %>% 
    slice_max(abs(log_odds_ratio) , n=10 , with_ties = F) # 절대값 기준 상위 10개 단어 추출

top10 %>% arrange(-log_odds_ratio) %>% 
  select(word, log_odds_ratio , president ) %>%  print(n=Inf)

### 막대 그래프 만들기
ggplot(top10 , aes(x = reorder(word , log_odds_ratio) , 
                   y = log_odds_ratio , 
                   fill = president )) + 
  geom_col() + 
  coord_flip() +
  labs(x= NULL) + 
  theme(text = element_text(family = "nanumgothic"))

### TF-IDF 로 여러 텍스트의 단어 비교하기
# 오즈비는 세 개 이상의 텍스트를 비교할 때 사용하기 적절하지 않다.
# 중요한 단어는 흔하지 않으면서도 특정 텍스트에서는 자주 사용된 단어이다. 
# TF-IDF:  어떤 단어가 흔하지 않으면서도 특정 텍스트에서는 자주 사용된 정도를 나타내는 지표
# TF: 단어 빈도, 단어가 특정 텍스트에 사용된 횟수
# DF: 문서 빈도, 단어가 사용된 텍스트 수 
# IDF: 역문서빈도 , 전체 문서수에서 DF가 차지하는 비중 log(N/DF)
# IDF는 DF가 클수록 작이지고(흔한 단어), DF가 작을수록 커진다.(특이한 단어)
# TF-IDF: 흔하지 않은 단어인데 특정 텍스트에서 자주 사용될 수록 큰 값
# TF x log(N/DF)

#전처리 
library(readr)
raw_speeches = read_csv("Doit_TM_Data/Data/speeches_presidents.csv" )
raw_speeches
speeches = raw_speeches %>%  mutate(value = str_replace_all(value , "[^가-힣]" , " ") , 
                                    value = str_squish(value))

# 토큰화 
speeches = speeches %>% unnest_tokens(input = value , output = word , token = extractNoun)
speeches

# 단어 빈도 구하기
frequency = speeches %>% count(president , word) %>% filter(str_count(word) > 1)
frequency

# TF - IDF 구하기
#bind_tf_idf(term = 단어 , document = 텍스트 구분 기준 , n = 단어 빈도)
# tf , idf , if_idf가 추가된다
frequency = frequency %>% bind_tf_idf(term = word  # 단어
                                      , document =  president # 텍스트 구분 변수 
                                      , n = n) %>%  # 단어 빈도 
            arrange(-tf_idf) # 내림차순 정렬

# TF-IDF가 높은 단어 살펴보기
# 각 대통령이 무엇을 강조했는지 알 수 있다. 
frequency %>% filter(president == "문재인")
frequency %>% filter(president == "박근혜")           
frequency %>% filter(president == "이명박")
frequency %>% filter(president == "노무현")

# TF-IDF가 낮은 단어 살펴보기
frequency %>% filter(president== "문재인") %>% arrange(tf_idf)
frequency %>% filter(president== "박근혜") %>% arrange(tf_idf)

### 막대그래프 만들기
# 주요 단어 추출
top10 = frequency %>% group_by(president ) %>% slice_max(tf_idf , n =10 , with_ties = F)
top10

# 그래프 순서 정하기
font_add_google(name = "Nanum Gothic" , family = "nanumgothic")
showtext_auto()

# 원하는 순서로 그래프 나열
top10$president = factor(top10$president , 
                         levels = c("문재인" , "박근혜" , "이명박" , "노무현"))

ggplot(top10 , aes(x= reorder_within( word , tf_idf , president)  , 
       y = tf_idf , fill = president) ) +
  geom_col(show.legend = F) +
  coord_flip()+
  facet_wrap(~president , scales = "free" , ncol =2) +
  scale_x_reordered() +
  labs(x= NULL)+
  theme(text = element_text(family = "nanumgothic"))

# TF-IDF의 한계: 모든 문서에 자주 사용된 단어 중 더 특히 많이 사용된 단어를 발견할 수 없다. 

##### 3단원: 비교분석 연습문제 #####
###1) : 두 연설문 분석 
raw_speeches = read_csv("Doit_TM_Data/Data/speeches_presidents.csv" )
raw_speeches  = raw_speeches %>% filter( president =="문재인" | president == "이명박")

# 전처리
precleansing =  function(raw_data){
  out = raw_data %>%  mutate(value = str_replace_all(value , "[^가-힣]" , " ") , 
                           value = str_squish(value))
  return(out)
}

# 토큰화 
tokenization = function(data){
  token= data %>% unnest_tokens(input = value , output = word , token = extractNoun)
  return(token)
}

# 단어 빈도 구하기
noun_freq = function(speeches){
  freq = speeches %>% count(president , word) %>% filter(str_count(word) > 1)
  return(freq)
}

# wide form으로 변환 
library(tidyr)
wide_func = function(frequency){
  frequency_wide = frequency %>% pivot_wider(names_from = president  , values_from = n ,values_fill = list(n=0))
  names(frequency_wide) = c("word" , "moon" , "lee")
  return(frequency_wide)
}

# 로그 오즈비
log_odds_func = function(frequency_wide){
  log_odds_ratio = frequency_wide %>% mutate(log_odds_ratio = log(((moon +1)/(sum(moon+1)))/((lee+1)/(sum(lee+1))))) 
  return(log_odds_ratio) 
}

# top10 
top10_func = function(frequency_wide){
  frequency_wide %>% group_by( president = ifelse(log_odds_ratio >0 , "moon" , "lee")) %>% 
  slice_max(abs(log_odds_ratio) , n =10 , with_ties = F)
}

#그래프
barplot_func= function(top10){
  font_add_google(name = "Nanum Gothic" , family = "nanumgothic")
  showtext_auto()
  ggplot(top10 , aes(x= reorder( word , log_odds_ratio)  , 
                     y = log_odds_ratio , fill = president) ) +
    geom_col() +
    coord_flip()+
    labs(x= NULL)+
    theme(text = element_text(family = "nanumgothic"))
}

speeches = precleansing(raw_speeches)
speeches = tokenization(speeches)
frequency = noun_freq(speeches)
frequency_wide = wide_func(frequency)
frequency_wide = log_odds_func(frequency_wide)
top10 = top10_func(frequency_wide)
barplot_func(top10)

### 2) 4명의 대통령의 취임사를 담은 연설문
raw_speeches = read_csv("Doit_TM_Data/Data/inaugural_address.csv" )
raw_speeches

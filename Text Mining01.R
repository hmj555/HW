library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(showtext)
library(ggwordcloud)
library(readr)
library(tidyr)   # id이용할 때 # 기호 
library(KoNLP)

setwd("C:/Users/user/OneDrive/바탕 화면/전공 수업 자료/텍스트마이닝_안정용")

############  1 ##########################
# 1-1 
vd = read.csv("Doit_TM_Data/Data/vd.csv" )
vd

# 1-2
vd = vd[, -5] %>% filter(year == 2020 | year ==2021)
vd

#1-3 

bar = function(vd){
  sum =  tapply(vd$income , vd$edu ,sum)
  sum
  barplot(sum , main= "교육수준별 수입의 합")
}
bar(vd)

# 1-4 

year_diff = function(vd){
  m = aggregate(expense~year , vd, mean)
  v = m$expense[2] - m$expense[1]
  paste("2021년도의 평균이 2020년도의 평균보다" , round(v,3) , "만큼 차이가 난다.")
}

year_diff(vd)

####################### 2 ##################################

library(httr)
library(XML)
library(rvest)
library(tidyverse)

url <- "https://n.news.naver.com/mnews/article/421/0006405379?sid=102"

# 2-1
web = GET(url)
web
html = read_html(web)
html

pre_clean = function(html){
  contents = html_nodes(html,"#dic_area")
  contents = gsub("[\r\n\t]","", contents) #r태그, t태그 제거
  contents = strsplit(contents , "</span><br><br>")[[1]][2]
  contents =gsub("</div>","", contents) #r태그, t태그 제거
  contents = gsub("<br>","", contents)
  contents = gsub("\\.", "",contents)
  contents %>% as_tibble()
}

contents = pre_clean(html)
contents


# 2-2 

# 토큰화 
noun_tokenization = function(contents){
  token = contents %>% unnest_tokens( input = value , output = word , token = "words")
  return(token)
}

word_noun = noun_tokenization(contents) # 명사 기준 토큰화
word_noun


# 단어 빈도 구하기
term_freq = function(word_noun){
  freq = word_noun %>% count(word , sort = T) %>%  # word별로 숫자를 세서 내림차순 정렬
    filter(str_count(word) > 1) # word의 길이가 1이상인 행만 추출(한글자 단어 제외)
  return(freq)
}

word_noun = term_freq(word_noun) # 명사 기준 단어 빈도
word_noun

# 2-3 

# 상위 20개 단어 추출
top = function(word_noun){
  top20 = head(word_noun, 20)
  return(top20)
}

top20 = top(word_noun) # 자주 사용된 상위 20개 단어 빈도
top20

# 막대그래프 생성
barplot_func = function(top20){
  ggplot(top20 , aes(x = reorder(word,n) , y = n)) + # n을 기준으로 내림차순으로 재정렬해줌
    geom_col() + # 막대그래프(y축을 빈도수가 아니라 지정해줌)
    coord_flip() + # 회전
    geom_text(aes(label = n) , hjust = -0.3) + # 막대 옆에 빈도표시(hjust: 막대와의 거리)
    labs(title = "연설문 단어 빈도", x = NULL , y= NULL) + #제목/축 이름 지정
    theme(title = element_text(size = 12)) # 제목 크기 설정
}

barplot_func(top20) # 상위 20개 단어 빈도의 막대그래프


# 워드클라우드 생성
wordcloud_func= function(word_noun){
  font_add_google(name = "Nanum Gothic" , family = "nanumgothic")
  showtext_auto()
  ggplot(word_noun , aes(label = word , size = n , col = n)) +
    geom_text_wordcloud(seed = 1234, family = "nanumgothic") +  # 워드클라우드 생성(난수 고정), 폰트 적용
    scale_radius( limits = c(2,NA) , range = c(3,20)) + # (최소,최대)단어빈도/ (최소,최대)글자크기
    scale_color_gradient(low = "indianred1" , high = "red2") + # (최소,최대)빈도 색깔
    theme_minimal()  # 배경없는 테마
}
wordcloud_func(word_noun)


###################### 3  ################

# 3-1 
# , locale = locale("ko" , encoding = "euc-kr")
raw_press = read_csv("press.csv" , locale = locale("ko" , encoding = "euc-kr"))  
raw_press

### 전처리
precleansing =  function(raw_data){
  out = raw_data %>%  mutate(article = str_replace_all(article , "[^가-힣]" , " ") , 
                             article = str_squish(article))
  return(out)
}

press = precleansing(raw_press)
press

# 3-2

# 명사 기준 토큰화 
tokenization = function(data){
  token= data %>% unnest_tokens(input = article , output = word , token = extractNoun)
  return(token)
}

press = tokenization(press)
press

# 단어 출현 빈도 구하기
noun_freq = function(press){
  freq = press %>% count(press , word) %>% filter(str_count(word) > 1)
  return(freq)
}

frequency = noun_freq(press)
frequency 


# 3-3 

# TF-IDF 구하기
TFIDF_func = function(frequency){
  TFIDF = frequency %>% bind_tf_idf(term = word  # 단어
                                    , document =  press # 텍스트 구분 변수 
                                    , n = n) %>%   # 단어 빈도
    arrange(-tf_idf) # 내림차순 정렬
  return(TFIDF)
}

frequency = TFIDF_func(frequency)
frequency

#TF-IDF가 높은 단어(상대적으로 중요한 단어)
frequency %>% filter(press == "중앙일보")  
frequency %>% filter(press == "국민일보")
frequency %>% filter(press == "프레시안")
frequency %>% filter(press == "한겨레")
frequency %>% filter(press == "동아일보")



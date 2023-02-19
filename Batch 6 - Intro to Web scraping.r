# -*- coding: utf-8 -*-

# -- IMDB --

library(tidyverse)
library(rvest) #scrap data from internet

mini project 01 IMDB web scraping

url <- "https://www.imdb.com/search/title/?groups=top_100&sort=user_rating,desc"

print(url)

#read html
imdb <- read_html(url)

#movie title
imdb %>%
    html_node("h3.lister-item-header") %>%
    html_text()

#movie title
imdb %>%
    html_node("h3.lister-item-header") %>%
    html_text2()

#movie title
titles <- imdb %>%
    html_nodes("h3.lister-item-header") %>%
    html_text2()

titles[1:10]

score <- imdb %>%
    html_nodes("div.ratings-imdb-rating")%>%
    html_text2()

#nuber of votr
num_vote <- imdb %>%
    html_nodes("p.sort-num_votes-visible")%>%
    html_text2()

#build a data set
#รวมvector
df <- data.frame(
    title = titles,
    rating = score,
    num_vote = num_vote
)

head(df)

# -- SpecPhone --

# **Mini project 02 0 Specphone phone database**


library(tidyverse)
library(rvest) #scrap data from internet

url <- "https://specphone.com/Realme-GT-3.html"

att <- url %>%
    read_html() %>%
    html_nodes("div.topic") %>%
    html_text2()

value<- url %>%
    read_html() %>%
    html_nodes("div.detail") %>%
    html_text2()

data.frame(attribute = att,value = value)

#All sumsung smartphone
samsung_url <- read_html("https://specphone.com/brand/Samsung")

#link to all samsung smartphone
links <- samsung_url %>%
    html_nodes("li.mobile-brand-item a") %>%
    html_attr("href")

full_links <- paste0("https://specphone.com", links)

#ดึงหลายหน้าจากเวปไซต์ เลยเขียนfor loop
result <- data.frame()

for (link in full_links[1:10]) {
    ss_topic <- link %>%
        read_html() %>%
        html_nodes("div.topic") %>%
        html_text2()

    ss_detail <- link %>%
        read_html() %>%
        html_nodes("div.detail") %>%
        html_text2()

    tmp <- data.frame(attribute = ss_topic,
                    value = ss_detail)
    result <- bind_rows(result , tmp)
    print("progress...")
        
}

print(head(result) ,3)

#write csv
write_csv(result,"samsung_phone.csv")


- 👋 Hi, I’m @amcnally89
- Below you will find the code that I am attempting to use for the R Capstone Project. It appears that the code is not able to correctly pull in the table from the Wikipedia webpage. In addition to reviewing the notes and videos in the Capstone course, I have also reviewed the videos and notes in the HTTP Request and REST API and Web Scraping in R videos in the R Programming Basics for Data Science.

- library(rvest), library(httr), library(shiny), -library(tidyverse)


url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"

root_node <- read_html(url)

body_node <- html_node(root_node,"body")

p_node <- html_node(body_node, "p")

p_content <- html_text(p_node)

table_node <- html_node(root_node, "table")

bicycle_data_frame <- html_table(table_node)




<!---
amcnally89/amcnally89 is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->

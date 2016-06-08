library(XML)
library(rvest)
library(stringr)
links <-list()
links=0
for (i in 1:10) {
URL <- paste0("https://www.goodreads.com/list/show/7.Best_Books_of_the_21st_Century?page=",i)
HTMLData <- read_html(URL)
links <- append(links, (html_attr(html_nodes(HTMLData, "a.bookTitle"), "href")))

}
print(links)

Books <- 0

for (i in 2:100) {

  Finalurl <- "https://www.goodreads.com"
  
  url <- paste0(Finalurl,links[i])

  #url <- "https://www.goodreads.com/book/show/2767052-the-hunger-games"
  ReviewerName <- html_text (url %>%
                             html() %>%
                             html_nodes(xpath='//*[@class="user"]'))
  if (length(ReviewerName) == 0){
    ReviewerName=NA
  }
  
  
  ReviewerComments <- html_text (url %>%
                              html() %>%
                              html_nodes(xpath='//*[@class="reviewText stacked"]'))
  if (length(ReviewerComments) == 0){
    ReviewerComments=NA
  }
  
  for (j in 1:5){
  data = c(url,ReviewerName[j],ReviewerComments[j])

  Books <- rbind(Books, data)
  }
  
}

Books <- Books[ -1, ]
df <- data.frame(Books)











for (i in 2:100) {
Finalurl <- "https://www.goodreads.com"

url <- paste0(Finalurl,links[i])


AuthorName <- html_text (url %>%
                           html() %>%
                           html_nodes(xpath='//*[@class="authorName"]'))
if (length(AuthorName) == 0){
  AuthorName=NA
}

RatingValue <- html_text (url %>%
                            html() %>%
                            html_nodes(xpath='//*[@itemprop="ratingValue"]'))
if (length(RatingValue) == 0){
  RatingValue=NA
}

RatingCount <- html_text (url %>%
                            html() %>%
                            html_nodes(xpath='//*[@itemprop="ratingCount"]'))
if (length(RatingCount) == 0){
  RatingCount=NA
}
RatingCount<-str_extract(RatingCount, "(^[0-9,]*)")

ReviewValue <- html_text (url %>%
                            html() %>%
                            html_nodes(xpath='//*[@class="value-title"]'))
if (length(ReviewValue) == 0){
  ReviewValue=NA
}
ReviewValue <-str_extract(ReviewValue, "(^[0-9,]*)")
ReviewValue <- ReviewValue[2]

NoOfPages <- html_text (url %>%
                          html() %>%
                          html_nodes(xpath='//*[@itemprop="numberOfPages"]'))
if (length(NoOfPages) == 0){
  NoOfPages=NA
}
NoOfPages<-str_extract(NoOfPages, "(^[0-9]*)")


Publication <- html_text (url %>%
                            html() %>%
                            html_nodes(xpath='//*[@class="row"][2]'))

if (length(Publication) == 0){
  Publication=NA
}

ISBN <- html_text (url %>%
                     html() %>%
                     html_nodes(xpath='//*[@class="infoBoxRowItem"][1]'))
if (length(ISBN) == 0){
  ISBN=NA
}

BookName <- html_text (url %>%
                          html() %>%
                          html_nodes(xpath='//*[@class="fn"]'))
if (length(BookName) == 0){
  BookName=NA
}

Edition <- html_text (url %>%
                         html() %>%
                         html_nodes(xpath='//*[@itemprop="bookEdition"]'))
if (length(Edition) == 0){
  Edition=NA
}


data = c(url,BookName,Edition,ISBN,AuthorName,NoOfPages,Publication,ReviewValue,RatingCount,RatingValue)
if(i==2){
  Books <- matrix(data, 1, byrow = T)
  next
}
Books <- rbind(Books, data)

}
df <- data.frame(Books)


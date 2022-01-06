##contoh script dasar untuk scraping data twitter

#simpan file di directory yang sudah ditentukan
getwd()

#install semua packages yang dibutuhkan
install.packages("rtweet")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("sigmajs")
install.packages("lubridate")


#load semua packages yang dibutuhkan

library(rtweet)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(graphTweets)
library(wordcloud2)
library(sigmajs)
library(lubridate)
library(readr) # to read and write files 
library(tidytext) # text mining
library(dplyr)  # data reshaping & restructuring
library(stringr) # to manipulate string variables
library(forcats) # for factors
library(tidyr) # to tidy data
library(reshape2) # reshape data
library(textdata) # to get sentiment libraries



#mendaftarkan akun twitter dev 

create_token(app = "percuma_lapor_polisi",
             consumer_key = "............",
             consumer_secret = "..........",
             access_token = "............",
             access_secret = "...........")


#jika sudah, maka kita sudah bisa menggunakan package rtweet



##menggunakan keywords atau hastags: 

percumalaporpolisi <- search_tweets("#PercumaLaporPolisi", n = 18000 , retryonratelimit = TRUE, lang = "id")


#jika tidak ingin mengikutsertakan data retweet, maka perlu ditambahkan perintah: include_rts = FALSE

percumalaporpolisi <- search_tweets("#PercumaLaporPolisi" , n = 18000 , include_rts = FALSE, retryonratelimit = TRUE, lang = "id")


#jika dataset yang kita minta lebih dari 18,000, twitter akan memberikan jeda 15menit

#jangan lupa simpan file yang sudah selesai kita download ke dalam format .csv


save_as_csv(percumalaporpolisi, file_name = "PercumaLaporPolisi.csv", prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")

# mengubah waktu ke timezone Jakarta 

percumalaporpolisi$created_at <- ymd_hms(percumalaporpolisi$created_at, tz="Asia/Bangkok")

#plot sederhana

percumalaporpolisi %>% 
  ts_plot("hours")


#plot dengan label 

percumalaporpolisi %>% 
  ts_plot(by = "hours") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = "waktu dalam jam",
    y = "frekuensi",
    title = "Frekuensi Tweet dengan kata Kunci 'PercumaLaporPolisi'",
    subtitle = "per jam",
    caption = paste0("Sumber: Twitter, tanggal: ", Sys.Date())
  )


##cleaning text

percumalaporpolisi %>% 
  select(text) %>% 
  mutate(text = gsub(pattern = "http\\S+", 
                     replacement = "", 
                     x = text)) %>% 
  mutate(text = gsub(pattern = "#", 
                     replacement = "", 
                     x = text)) %>% 
  mutate(text = gsub(pattern = "\\d+",
                     replacement = "",
                     x = text)) %>% 
  mutate(text = gsub(pattern = "@", 
                     replacement = "", 
                     x = text)) %>% 
  plain_tweets() -> text_cleaned_percuma


#simpan file teks ke dalam format .txt untuk keperluan 'analisis teks' (optional)

write.table(Book2, file = "text_cleaned_percuma.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


# membuat 'token' dan menghapus stopwords (pastikan anda punya file list stopwords di direktori yang sama)

Book2 %>%   
  unnest_tokens(input = word, output = token) %>% 
  count(token, sort = T)

stopword_indo <- read_csv("stopwords-id.txt", 
                          col_names = "stopwords")

# visualisasinya

Book2 %>% 
  unnest_tokens(input = word, output = token) %>% 
  anti_join(stopword_indo, by = c("token" = "stopwords")) %>% 
  count(token, sort = T) %>% 
  wordcloud2(size = 0.5)


Book2 %>%   
  unnest_tokens(input = word, output = token) %>% 
  count(token, sort = T) %>%
  top_n(15) %>%
  mutate(token = reorder(token, n)) %>%
  ggplot(aes(x = token, y = n)) +
  geom_col(fill="black") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Kata Yang Paling Banyak Muncul Di Tweet",
       subtitle = "Sebelum Stop Words Dihilangkan")
 


# Clean whitespace 

Book2 %>%  
  mutate(text=str_trim(word, side = "both"))

# generate ngram

text_sentiment <- Book2 %>%
  unnest_tokens(word, word, token = "ngrams", n = 1)

# See sentiment (pastikan file sentimen sudah ada di direktori yang sama 'Book1')

# Import Book1 to the environment

text_sentiment_2 <- text_sentiment %>%
  inner_join(Book1) %>%
  group_by(word)

# sum up all the sentiment values for each comment

text_sentiment_3 <- text_sentiment_2 %>%
  group_by(ID) %>%
  summarise(sentiment = sum(Polarity))
  
# collapse back all together by ID

sentiment_all <- text_sentiment_3 %>% 
  full_join(Book2, by="ID") %>%
  group_by(ID)

# simpan file

save_as_csv(sentiment_all, file_name = "Analisis_sentimen.csv", prepend_ids = TRUE, na = "",
            fileEncoding = "UTF-8")




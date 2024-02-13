# TEXT ANALYSIS of "The WhatsApp Group"

# Viewing the packages running in a current session
(.packages())

install.packages(c("rwhatsapp","lubridate","tidyverse","tidytext","kableExtra","RColorBrewer","textdata",
                   "knitr","xfun","extrafont","timeDate","ggimage", "stopwords", "zoo","ggwordcloud", "rvest")) # install these packages

# You should install the respective packages before calling the libraries below. Run the code above!
library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(stringr)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(extrafont)
library(timeDate)
library(ggimage)
library(stopwords)
library(ggwordcloud)
library(zoo)
library(ggimage)
library(xfun)
library(rvest)
library(textdata)

# install.packages()

groupchat <- rwa_read(choose.files()) # Reading the chat data into R
View(groupchat)
?top_n

# Passing groupchat into another label "gc1", extracting month, year and month-year columns from the time column,...
# ...filtering out NA rows and performing other transformations.
gc1 <- groupchat %>%
    mutate(date.d = date(as.POSIXlt(time, format="%Y-%m-%d %H:%M:%S"))) %>%
    filter(!is.na(author)) %>%
    filter(author != "You") %>%
    filter(!is.na(time)) %>%
    filter(text != "‎Messages and calls are end-to-end encrypted. No one outside of this chat, not even WhatsApp, can read or listen to them.") %>%
    filter(author != "The WhatsApp Group") %>%
    mutate(day.name = weekdays(as.POSIXlt(time, format="%Y-%m-%d %H:%M:%S"))) %>%
    mutate(day.num = wday(as.POSIXlt(time, format="%Y-%m-%d %H:%M:%S"), label = FALSE)) %>%
    mutate(month.name = month(as.POSIXlt(time, format="%Y-%m-%d %H:%M:%S"), label = TRUE, abbr = FALSE)) %>%
    mutate(month.num = month(as.POSIXlt(time, format="%Y-%m-%d %H:%M:%S"))) %>%
    mutate(yr.str = as.character(year(as.POSIXlt(time, format="%Y-%m-%d %H:%M:%S")))) %>%
    mutate(yr.num = year(as.POSIXlt(time, format="%Y-%m-%d %H:%M:%S"))) %>%
    mutate(Month_Yr = format(as.Date(time), "%b-%Y")) %>%
    mutate(Yr_Qtr = as.yearqtr(time))

View(gc1)

# To check datatypes of columns in your dataset
class(gc1$date.d)

# ?timeLastDayInMonth
    
# Creating a color palette for the different months in the gc1 dataset
# Do note; that choosing your color set would depend on the number of different variables you want to plot
# Also note that you can always go with R's default colors

color.set <- brewer.pal(12,"Set3")
# [c(4,5,7,1,3,2,6,9,8)]
colors.all <- display.brewer.all(n=NULL, type="div", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=FALSE) [c(5,6,7)]
# display.brewer.pal(n, "Set3")
# ?brewer.pal
windowsFonts(Seg=windowsFont("Segoe UI"))

gc3 <- gc1 %>%
  select(author)

# Plotting a bar chart of no. of chats by group members over time
gc1 %>%
  count(author) %>%
  slice_max(n=10,order_by = n) %>%
  ggplot(aes(x=reorder(author, n), y=n, fill=author)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n),label.size=0.2,color="black") +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("An Analysis of the Frequency of Messages by Author") +
  theme_minimal() +
  theme(text=element_text(family="Seg", face="bold", size =12)) +
  theme(legend.title = element_blank(), legend.position = "none")

# Plotting a bar chart of no. of chats by month across all years
gc1 %>%
  group_by(month.name, month.num, yr.num, yr.str) %>%
  count(text) %>%
  ggplot(aes(x=reorder(month.name, -month.num), y=n, fill=reorder(yr.str, -yr.num))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=color.set) +
  # geom_text(aes(label = n), position = position_dodge(0.9)) + ~ HAD TO COMMENT THIS OUT, AS THE PLOT LOOKED TOO BUSY
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("No. of Chats by Month and Year","An Analysis of the Frequency of Messages each Month") +
  theme_minimal() +
  theme(text=element_text(family="Seg", face="bold", size =12)) +
  theme(legend.title = element_blank(), legend.position = "right")

# Plotting a bar chart of no. of chats by day across all years
gc1 %>% 
  group_by(day.name, day.num, yr.num, yr.str) %>%
  count(text) %>%
  ggplot(aes(x=reorder(day.name, -day.num), y=n, fill=reorder(yr.str, -yr.num))) + 
  geom_bar(stat="identity") +
  #geom_text(aes(label = n)) +
  scale_fill_manual(values=color.set) +
  # geom_text(aes(label = txt_count), position = position_dodge(0.9)) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("No. of Chats by Day and Year","An Analysis of the Frequency of Messages each Day") +
  theme_minimal() +
  theme(text=element_text(family="Seg", face="bold", size =12)) +
  theme(legend.title = element_blank(), legend.position = "right")

# Plotting a chart to show the trend analysis since the earliest data point of the chats extracted
gc1 %>%
  group_by(date.d, yr.str) %>%
  count(text) %>%
  ggplot(aes(x=date.d, y=n, fill=reorder(yr.str, date.d))) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values=color.set) +
  # geom_text(aes(label = n), position = position_dodge(0.9)) +
  ylab("Number of Messages") + xlab("Time Period") +
  ggtitle("All-Time Trend Analysis of the Frequency of Messages") +
  theme(text=element_text(family="Seg", face="bold", size =12),legend.title = element_blank(), 
        legend.position = "bottom")

# EMOJI ANALYSIS WITH PNG IMAGE FETCH FROM https://abs.twimg.com

emojitable <- gc1 %>%
  unnest(c(emoji, emoji_name)) %>% # SEPARATING EMOJIS INTO DIFFERENT ROWS FOR AN ACCURATE COUNT
  count(emoji, emoji_name) %>%
  slice_max(n=10, order_by=n) %>% # FILTERING FOR THE TOP 10 EMOJIS USED
  # EXTRACTING THE STICKER IMAGE URL BEFORE VISUALIZING WITH GEOM_IMAGE
  mutate(emoji = str_sub(emoji, end = 1)) %>% 
  mutate(emoji_url = map_chr(emoji, ~paste0("https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)),".png")))

View(emojitable)

#PLOTTING THE GRAPH OF THE TOP 10 EMOJIS USED
emojitable %>% 
  ggplot(aes(x=reorder(emoji_name, n), y=n)) +
  geom_col(aes(fill=n), show.legend = FALSE, width = .2) +
  geom_image(aes(image=emoji_url), size=.05) +
  ylab("") + xlab("") +
  coord_flip() +
  ggtitle("Top 10 Emojis Used") +
  # geom_text(aes(label = n), position = position_dodge(0.9)) +
  theme(text=element_text(family="Seg", face="bold", size =12)) +
  scale_fill_distiller(type = "div", palette = 3, direction = -1)

# GROUPING THE TOP EMOJIS USED BY GROUP MEMBERS
emoji_author <- gc1 %>%
  unnest(c(emoji, emoji_name)) %>% # SEPARATING EMOJIS INTO DIFFERENT ROWS FOR AN ACCURATE COUNT
  count(author, emoji, emoji_name) %>%
  group_by(author) %>%
  slice_max(n=3, order_by=n) %>%
  slice(1:3) %>%
  mutate(emoji = str_sub(emoji, end = 1)) %>%
  mutate(emoji_url = map_chr(emoji, ~paste0("https://abs.twimg.com/emoji/v2/72x72/", as.hexmode(utf8ToInt(.x)),".png")))

View(emoji_author)

# PLOTTING THE GRAPH FOR TOP 3 EMOJIS USED BY EACH GROUP MEMBER
emoji_author %>%
 ggplot(aes(x=reorder(emoji, -n), y=n)) +
 geom_col(aes(fill=author, group=author), show.legend = FALSE, width = .2) +
 geom_image(aes(image=emoji_url), size=.05) +
 ylab("Emoji Count") + xlab("") +
 facet_wrap(~author, ncol=5, scales = "free") +
 ggtitle("Top 3 Emojis Used by Group Members") +
 theme(text=element_text(family="Seg", face="bold", size =12))

# GENERATING A WORDCLOUD OF THE 200 MOST USED WORDS

# CREATING A VECTOR CONTAINING GENERIC WORDS LIKE PRONOUNS AND OTHER UNNEEDED WORDS
generic_words <- c(stopwords("en", source="stopwords-iso"),"omitted","sticker","audio","image","video")

# SUMMARIZING THE TOP 50 WORDS BY WORD COUNT
summ <- gc1 %>%
  unnest_tokens(input = text, output = words, token = "words") %>%
  filter(!words %in% generic_words) %>%
  count(words)%>%
  slice_max(n=50, order_by=n)

# THE WORDCLOUD

summ %>%
  ggplot(aes(label = words, size = n, color = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE, max_steps = 1, grid_size = 1, eccentricity = 1) +
  scale_size_area(max_size = 100) +
  ggtitle("Most Used Words") +
  scale_color_gradient(low = "darkgreen",
                       high = "lightgreen",
                       space = "Lab",
                       guide = "colourbar",
                       aesthetics = "colour")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
## SENTIMENT ANALYSIS USING EMOJI
# USING WORD WOULD BE MORE ACCURATE BUT THERE IS USUALLY A STRONG USE OF CREOLES AND SLANGS IN PRIVATE CHATS
# WE WOULD BE USING THE PACKAGE rvest TO EXTRACT AN HTML WEBPAGE CONTAINING EMOJI SENTIMENTS

url <- read_html("http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html")

table_url <- url %>%
  html_node("#myTable") %>%
  html_table() %>%
  as_tibble() %>%
  select(1,6:9) %>% # "select" is used to select columns in a data frame. So I'm selecting only the columns critical to this analysis.
  set_names("emoji", "negative","neutral","positive","sentiment_score") # "set_names" renames columns in the order that they appear in the data frame

sentiment_table <- gc1 %>% 
  unnest(c(emoji, emoji_name)) %>% 
  mutate( emoji = str_sub(emoji, end = 1)) %>% 
  inner_join(table_url, by=c("emoji"="emoji")) %>%
  select(-source, -day.num)

sentiment_table1 <- sentiment_table %>% 
  group_by(author) %>% 
  summarise(
    positive=mean(positive),
    negative=-(mean(negative)),
    neutral=mean(neutral),
    balance=mean(sentiment_score)
  ) %>%
  arrange(desc(negative))

sentiment_table1 %>%
  gather("sentiment","mean", -author, -balance) %>% 
  mutate(sentiment = factor(sentiment, levels = c("negative","positive","neutral"), ordered = TRUE)) %>% 
  ggplot(aes(x=reorder(author,balance), y=mean, fill=sentiment)) +
  geom_bar(position="stack", stat="identity", show.legend = FALSE, width = .5) +
  scale_fill_manual(values = brewer.pal(4, "RdYlGn")[c(1,4,2)]) +
  ylab(" - Negative / Neutral / Positive +") + xlab("") +
  ggtitle("Sentiment Analysis", "Using Emojis Sent By Group Members") +
  coord_flip() +
  theme_minimal()

# SENTIMENT ANALYSIS USING EMOJI NAMES

# afinn_sentiment <- get_sentiments("afinn")
nrc_sentiment <- get_sentiments("nrc") %>% set_names("word", "sentiment") # "association_score"

emoji_sent <- gc1 %>%
  select(emoji, emoji_name) %>%
  unnest(c(emoji, emoji_name)) %>%
  mutate(emoji = str_sub(emoji, end = 1)) %>%
  unnest_tokens(input=emoji_name, output=emoji_name) %>% #unnest_tokens separates a chunk of strings like a sentence into different sub-strings i.e. words
  inner_join(nrc_sentiment, by=c("emoji_name"="word")) %>%
  filter(!sentiment %in% c("negative","positive"))
  # filter(association_score==1) %>%
  # select(-association_score)

emoji_sent %>%
  count(sentiment) %>%
  ggplot(aes(x=reorder(sentiment,-n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend = FALSE) +
  ylab("") + xlab("Emotion") +
  ggtitle("Emotion Sentiments","Using Emojis Sent By Group Members") +
  theme_minimal()
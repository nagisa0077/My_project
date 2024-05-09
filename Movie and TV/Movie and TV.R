#### library and read data####
library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(lubridate)
library(reshape2)
library(scales)
library(tidytext)
library(wordcloud)
library(tidyr)
library(stringr)
# Netflix viedo
data_n <- data_n.o <- read.csv('D:\\nagisa\\NAGISA\\求職\\作品資料\\Movie and TV\\netflix_titles.csv') 
data_n <- data_n %>% mutate_if(is.character, ~na_if(., "")) %>% 
          mutate(date_added = mdy(date_added))
data_n$date_added <- format(data_n$date_added,"%Y")
data_n$date_added <- as.numeric(data_n$date_added)
movie_n <- data_n[which(data_n$type=='Movie'),]
TV_n <- data_n[which(data_n$type=='TV Show'),]

# Disney viedo
data_d <- data_d.o <- read.csv('D:\\nagisa\\NAGISA\\求職\\作品資料\\Movie and TV\\disney_plus_titles.csv') 
data_d <- data_d %>% mutate_if(is.character, ~na_if(., "")) %>% 
  mutate(date_added = mdy(date_added))
data_d$date_added <- format(data_d$date_added,"%Y")
data_d$date_added <- as.numeric(data_d$date_added)
movie_d <- data_d[which(data_d$type=='Movie'),]
TV_d <- data_d[which(data_d$type=='TV Show'),]

# Amazon_prime viedo
data_a <- data_a.o <- read.csv('D:\\nagisa\\NAGISA\\求職\\作品資料\\Movie and TV\\amazon_prime_titles.csv') 
data_a <- data_a %>% mutate_if(is.character, ~na_if(., "")) %>% 
  mutate(date_added = mdy(date_added))
data_a$date_added <- format(data_a$date_added,"%Y")
data_a$date_added <- as.numeric(data_a$date_added)
movie_a <- data_a[which(data_a$type=='Movie'),]
TV_a <- data_a[which(data_a$type=='TV Show'),]

# IMBD viedo 
# IMDb <- read.csv('D:\\nagisa\\NAGISA\\求職\\作品資料\\Movie and TV\\Top 1000 IMDB movies.csv')
# IMDb <- IMDb[,-1]
# IMDb$Year.of.Release <- gsub("\\(|\\)", "", IMDb$Year.of.Release) # 刪除()
# colnames(IMDb) <- c('title', 'release_year', 'duration', 'IMDb_rating', 'score',
#                     'vote', 'gross', 'description_IMDb')

IMDb <- read.csv('D:\\nagisa\\NAGISA\\求職\\作品資料\\Movie and TV\\IMDbMovies.csv')
colnames(IMDb) <- c('title', 'description.IMDb', 'director', 'writer', 'listed.in',
                    'rating', 'release.year', 'duration','score', 'Number.of.Ratings',
                    'Budget','Gross.North.America', 'Gross','Opening.Weekend','Gross.Opening.Weekend')


# Netflix user
# user <- read.csv('D:\\nagisa\\NAGISA\\求職\\作品資料\\Movie and TV\\Netflix Userbase.csv')

# Netflix movie & IMDb score
dat_n <- merge(x = movie_n,y = IMDb, by = 'title')
dat_n <- dat_n %>% select(title, director = director.x, cast, writer, country, date_added,
                          duration = duration.x, listed.in, 
                          release.year = release_year, rating = rating.x, description, description.IMDb,
                          score, Number.of.Ratings, Gross, Gross.Opening.Weekend) %>% mutate(service = 'Netflix')

# Disney movie & IMDb score
dat_d <- merge(x = movie_d ,y = IMDb, by = 'title')
dat_d <- dat_d %>% select(title, director = director.x, cast, writer, country, date_added,
                         duration = duration.x, listed.in, 
                         release.year = release_year, rating = rating.x, description, description.IMDb,
                         score, Number.of.Ratings, Gross, Gross.Opening.Weekend) %>% mutate(service = 'Disney+')

# Amazon_prime & IMDb score
dat_a <- merge(x = movie_a ,y = IMDb, by = 'title')
dat_a <- dat_a %>% select(title, director = director.x, cast, writer, country, date_added,
                          duration = duration.x, listed.in, 
                          release.year = release_year, rating = rating.x, description, description.IMDb,
                          score, Number.of.Ratings, Gross, Gross.Opening.Weekend) %>% mutate(service = 'Amazon prime')

# data 
dat <- rbind(dat_a,dat_d,dat_n)
################################ Netflix ######################################
#### data_n summary ####
head(data_n)

##### all variable #####
variable <- colnames(data_n);variable
summary(data_n)

####### show_id #######
length(unique(data_n$show_id)) # 沒有重複

####### type #######
table(data_n$type)
type <- data_n %>% count(type, sort = T) %>%
  mutate(prop = paste0(round(n / sum(n) * 100, 0), "%"))
ggplot(type, aes(x = "", y = prop, fill = type)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Type ratio") +
  geom_text(aes(y = prop, label = prop),position = position_stack(vjust = 0.5),
    size = 6, col = "white",fontface = "bold") +
  scale_fill_manual (values = c('#F8766D', '#377eb8')) +
  theme_void()

rm(type);gc()
####### title #######
head(data_n$title)
length(unique(data_n$title))

####### director #######
director <- data_n %>% select(country,director) %>% filter(grepl("United States", country, ignore.case = TRUE))
director_list <- list()
for(i in 1:length(director$director)){
  director_list[[i]] <- strsplit(director$director[i], ",")
}
head(sort(table(unlist(director_list)),decreasing = TRUE),10) # 最多作品的演員
director <- data.frame(cast = unlist(director_list))
director <- director %>% count(cast, sort = T)
director <- head(director[-1,],10)

sum(is.na(data_n$director)) # 2634影片該資料集中沒有導演資料
head(sort(table(data_n$director),decreasing = TRUE)) # 最多作品的導演
director <- data_n %>% select(country,director) %>% filter(grepl("United States", country, ignore.case = TRUE)) %>% count(director,sort = T)
director <- director[-1,]
director <- director[order(director$n, decreasing = TRUE), ]
director <- head(director,10)

ggplot(director, aes(x = reorder(director, n), y = n, fill = director)) +
  geom_bar(stat = "identity") +
  labs(title = "Count by director of USA", x = "Director", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_flip()

rm(director,director_list);gc()
####### cast #######
cast <- data_n %>% select(country,cast) %>% filter(grepl("United States", country, ignore.case = TRUE))
cast_list <- list()
for(i in 1:length(cast$cast)){
  cast_list[[i]] <- strsplit(cast$cast[i], ",")
}
head(sort(table(unlist(cast_list)),decreasing = TRUE),10) # 最多作品的演員
cast <- data.frame(cast = unlist(cast_list))
cast <- cast %>% count(cast, sort = T)
cast <- head(cast[-1,],10)

ggplot(cast, aes(x = reorder(cast, n), y = n, fill = cast)) +
  geom_bar(stat = "identity") +
  labs(title = "Count by Cast of USA", x = "Cast", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_flip()

rm(cast,cast_list);gc()
####### country #######
sum(is.na(data_n$country)) # 831影片該資料集中沒有國家資料
data_n$country[is.na(data_n$country)] <- 'Other'

country_m <- data_n %>% select(type, country)%>% filter(type == "Movie")
country_list <- list()
for(i in 1:length(country_m$country)){
  country_list[[i]] <- strsplit(country_m$country[i], ",")
}
country_m <- data.frame(country = unlist(country_list))
country_m <- country_m %>% count(country, sort = T)
  
country_t <- data_n %>% select(type, country)%>% filter(type == "TV Show")
country_list <- list()
for(i in 1:length(country_t$country)){
  country_list[[i]] <- strsplit(country_t$country[i], ",")
}
country_t <- data.frame(country = unlist(country_list))
country_t <- country_t %>% count(country, sort = T) 

country <- merge(country_m, country_t, by = "country", suffixes = c("_movie", "_TV"), all = TRUE)
country[is.na(country)] <- 0
country <- head(country[order(-country$n_movie),])
colnames(country) <- c('country', 'Movie', 'TV Show')
country <- tidyr::gather(country , key = "Type", value = "Count", -country)

ggplot(country, aes(x = reorder(country, Count), y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Count by Country", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  coord_flip() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D"))

rm(country,country_m,country_t,country_list);gc()
####### date_added #######
sum(is.na(data_n$date_added))
date_added <- data.frame(merge(data.frame(table(movie_n$date_added)), data.frame(table(TV_n$date_added)), by = "Var1", suffixes = c("_movie", "_TV"), all = TRUE))
date_added[is.na(date_added)] <- 0
colnames(date_added) <- c("year", "count_movie", "count_TV")
date_added$year <- as.numeric(as.character(date_added$year))
date_added <- date_added[order(date_added$year), ]
date_added$year <- as.Date(paste0(date_added$year, "-01-01"))

ggplot(date_added, aes(x = as.Date(paste0(year, "-01-01")))) +
  geom_line(aes(y = count_movie, color = "Movie"), size = 1.2) +
  geom_line(aes(y = count_TV, color = "TV"), size = 1.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  labs(title = "Netflix Date added", x = "year", y = "count", color = "Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Movie" = "#F8766D", "TV" = "#00BFC4"),
                     labels = c("Movie", "TV"))

rm(date_added);gc()
####### release_year #######
sum(is.na(data_n$release_year))
table(data_n$release_year)
year <- data.frame(merge(data.frame(table(movie_n$release_year)), data.frame(table(TV_n$release_year)), by = "Var1", suffixes = c("_movie", "_TV"), all = TRUE))
year[is.na(year)] <- 0
colnames(year) <- c("year", "count_movie", "count_TV")
year$year <- as.numeric(as.character(year$year))
year <- year[order(year$year), ]
year$year <- as.Date(paste0(year$year, "-01-01"))

ggplot(year, aes(x = as.Date(paste0(year, "-01-01")))) +
  geom_line(aes(y = count_movie, color = "Movie"), size = 1.2) +
  geom_line(aes(y = count_TV, color = "TV"), size = 1.2) +
  scale_x_date(date_breaks = "10 year", date_labels = "%Y") +
  labs(title = "Number of Movie/TV show", x = "year", y = "count", color = "Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Movie" = "#F8766D", "TV" = "#00BFC4"),
                     labels = c("Movie", "TV"))

rm(year);gc()
####### rating #######
data_n$rating <- case_when(
  data_n$rating == "13+" ~ "PG-13",
  data_n$rating == "16+" ~ "PG-13",
  data_n$rating == "18+" ~ "R",
  data_n$rating == "7+" ~ "G",
  data_n$rating == "AGES_16_" ~ "PG-13",
  data_n$rating == "ALL" ~ "G",
  data_n$rating == "G" ~ "G",
  data_n$rating == "NR" ~ "NR",
  data_n$rating == "PG" ~ "PG",
  data_n$rating == "PG-13" ~ "PG-13",
  data_n$rating == "R" ~ "R",
  data_n$rating == "UNRATED" ~ "NR",
  data_n$rating == "TV-14" ~ "PG-13",
  data_n$rating == "TV-G" ~ "G",
  data_n$rating == "TV-PG" ~ "PG",
  data_n$rating == "TV-Y" ~ "G",
  data_n$rating == "TV-MA" ~ "NC-17",
  data_n$rating == "TV-Y7" ~ "PG",
  data_n$rating == "UR" ~ "NR",
  TRUE ~ "Other"
)
rating <- data_n %>%  count(rating, sort = T)


rating_m <- data_n %>% select(type, rating) %>% 
  filter(type == "Movie") %>% count(rating, sort = T)
  
rating_t <- data_n %>% select(type, rating) %>% 
  filter(type == "TV Show") %>% count(rating, sort = T)
  
rating <- rating[-c(13,16,17,18),] # 誤植和NA
rating_m <- rating_m[-c(15,16,17,18),] # 誤植和NA
rating_t <- rating_t[-c(9),] # 誤植和NA


ggplot(rating, aes(x = reorder(rating, n), y = n, fill = rating)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Count by Rating", x = "Rating", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_flip()

rm(rating,rating_m,rating_t);gc()
####### duration #######
movie_duration <- data.frame(movie_duration = as.numeric(gsub(paste(c('min'), collapse = "|"), "", movie_n$duration)))
TV_duration <- data.frame(TV_duration = as.numeric(gsub(paste(c('Seasons','Season'), collapse = "|"), "", TV_n$duration)))

# movie
summary(movie_duration)
p1 <- ggplot(movie_duration, aes(x = movie_duration)) +
  geom_histogram(binwidth = 1, fill = "#F8766D")

# TV
summary(TV_duration)
table(TV_duration)
p2 <- ggplot(TV_duration , aes(x = TV_duration)) +
  geom_bar(binwidth = 1, fill = "#00BFC4")

grid.arrange(p1,p2,nrow =1, ncol = 2)

rm(movie_duration,TV_duration,p1,p2)
####### listed_in #######
sum(is.na(data_n$listed_in)) # 0影片該資料集中沒有分類資料

list <- data_n %>% select(country,listed_in) %>% filter(country ==  "United States")
listed_in_list <- list()
for(i in 1:length(data_n$listed_in)){
  listed_in_list[[i]] <- strsplit(data_n$listed_in[i], ",")
}
head(sort(table(unlist(listed_in_list)),decreasing = TRUE)) # 最多作品的演員

rm(listed_in_list)
####### description #######
words <- data_n %>% select(type, show_id, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
count_word <- words %>%count(word, sort = TRUE)
wordcloud(words = count_word$word,freq = count_word$n, 
          min.freq = 50,max.words = nrow(count_word), 
          random.order = FALSE,rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2"))

rm(words,count_word)

# movie
words_m <- data_n %>% select(type, show_id, description) %>%
  filter(type == "Movie") %>% 
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
count_word_m <- words_m %>% count(word, sort = TRUE)
wordcloud(words = count_word_m$word,  
          freq = count_word_m$n, 
          min.freq = 50,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2")) 

# TV
words_tv <- data_n %>% select(type, show_id, description) %>%
  filter(type == "TV Show") %>% 
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
count_word_t <- words_tv %>% count(word, sort = TRUE)
wordcloud(words = count_word_t$word,  
          freq = count_word_t$n, 
          min.freq = 30,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2")) 



###################### different streaming service##############################
####### director #######
# Netflix
# director <- dat_n %>% select(country,director) %>% filter(grepl("United States", country, ignore.case = TRUE)) %>% count(director,sort = T)
director <- dat_n %>% select(director) %>%  count(director,sort = T)
director <- na.omit(director)
director <- director[order(director$n, decreasing = TRUE), ]
director <- head(director,10)

p_n <- ggplot(director, aes(x = reorder(director, n), y = n, fill = director)) +
  geom_bar(stat = "identity") +
  labs(title = "Netflix Count by director", x = "Director", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()

# Disney
director <- dat_d %>% select(director) %>%  count(director,sort = T)
director <- na.omit(director)
director <- director[order(director$n, decreasing = TRUE), ]
director <- head(director,10)

p_d <- ggplot(director, aes(x = reorder(director, n), y = n, fill = director)) +
  geom_bar(stat = "identity") +
  labs(title = "Disney Count by director", x = "Director", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()

# Amazon_prime
director <- dat_a %>% select(director) %>%  count(director,sort = T)
director <- na.omit(director)
director <- director[order(director$n, decreasing = TRUE), ]
director <- head(director,10)

p_a <- ggplot(director, aes(x = reorder(director, n), y = n, fill = director)) +
  geom_bar(stat = "identity") +
  labs(title = "Amazon_prime Count by director", x = "Director", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()

grid.arrange(p_n,p_d,p_a,nrow =1, ncol = 3)
rm(director);gc()

####### cast #######
# Netflix
cast <- dat_n %>% select(country,cast)
cast_list <- list()
for(i in 1:length(cast$cast)){
  cast_list[[i]] <- strsplit(cast$cast[i], ",")
}
head(sort(table(unlist(cast_list)),decreasing = TRUE),10) # 最多作品的演員
cast <- data.frame(cast = unlist(cast_list))
cast <- cast %>% count(cast, sort = T)
cast <- na.omit(cast)
cast <- head(cast,10)

p_n <- ggplot(cast, aes(x = reorder(cast, n), y = n, fill = cast)) +
  geom_bar(stat = "identity") +
  labs(title = "Netflix Count by Cast", x = "Cast", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()

# Disney
cast <- dat_d %>% select(country,cast)
cast_list <- list()
for(i in 1:length(cast$cast)){
  cast_list[[i]] <- strsplit(cast$cast[i], ",")
}
head(sort(table(unlist(cast_list)),decreasing = TRUE),10) # 最多作品的演員
cast <- data.frame(cast = unlist(cast_list))
cast <- cast %>% count(cast, sort = T)
cast <- na.omit(cast)
cast <- head(cast,10)

p_d <- ggplot(cast, aes(x = reorder(cast, n), y = n, fill = cast)) +
  geom_bar(stat = "identity") +
  labs(title = "Disney Count by Cast", x = "Cast", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()

# Amazon_prime
cast <- dat_a %>% select(country,cast)
cast_list <- list()
for(i in 1:length(cast$cast)){
  cast_list[[i]] <- strsplit(cast$cast[i], ",")
}
head(sort(table(unlist(cast_list)),decreasing = TRUE),10) # 最多作品的演員
cast <- data.frame(cast = unlist(cast_list))
cast <- cast %>% count(cast, sort = T)
cast <- na.omit(cast)
cast <- head(cast,10)

p_a <- ggplot(cast, aes(x = reorder(cast, n), y = n, fill = cast)) +
  geom_bar(stat = "identity") +
  labs(title = "Amazon_prime Count by Cast", x = "Cast", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none") +
  coord_flip()

grid.arrange(p_n,p_d,p_a,nrow =1, ncol = 3)
rm(cast,cast_list);gc()
####### score ######
score <- dat %>% select(title,score,Number.of.Ratings,Gross,service)
score  <- na.omit(score)

# 不同平台的電影的分數分布
ggplot(data = score, mapping = aes(x = service, y = score, fill = service)) +
  geom_boxplot() +
  theme(axis.text.x  = element_blank()) +
  labs(fill = "Service", title = "Service of score")+
  scale_fill_manual(values = c("Amazon prime" = "#619CFF", "Disney+" = "#00BFC4", "Netflix" = "#F8766D"),
                    labels = c("Amazon prime", "Disney+", "Netflix"))

# # 分數與話題度
# score1 <- score[which((score$Number.of.Ratings>300) & (score$Number.of.Ratings<1500)),]
# ggplot(score1, aes(x = score, y = Number.of.Ratings, color = service)) +
#   geom_point(position = "identity") +
#   labs(title = "IMDb rating vs. Number.of.Ratings", x = "IMDb rating") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1))
# 
# # 分數與票房
# score2 <- score[which((score$Gross>300) & (score$Gross<1500)),]
# ggplot(score2, aes(x = score, y = Gross, color = service)) +
#   geom_point(position = "identity") +
#   labs(title = "IMDb rating vs. Gross", x = "IMDb rating") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1))

####### gross ######
gross <- dat %>% select(title,score,Number.of.Ratings,Gross,Gross.Opening.Weekend,service) %>%
  arrange(desc(Gross)) %>%mutate(ratio = Gross.Opening.Weekend/Gross)
gross  <- na.omit(gross)
head(gross, 10)

# # 不同平台的電影的票房分布
# ggplot(data = gross, mapping = aes(x = service, y = Gross, fill = service)) + 
#   geom_boxplot() +
#   theme(axis.text.x  = element_blank()) +
#   labs(fill = "Service", title = "Service of gross")
# 
# # 票房與話題度
# gross1 <- gross[which((gross$Number.of.Ratings>300)),]
# ggplot(gross1, aes(x = Gross, y = Number.of.Ratings, color = service)) +
#   geom_point(position = "identity") +
#   labs(title = "IMDb Gross vs. Number.of.Ratings") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1))
# 
# correlation <- gross1 %>%
#   group_by(service) %>%
#   summarise(correlation = cor(Number.of.Ratings, Gross))
# correlation <- rbind(data.frame(service = "all", correlation = cor(gross1$Gross,gross1$Number.of.Ratings)), correlation)
# correlation
# 
# # 首周票房與話題度
# ggplot(gross1, aes(x = Gross.Opening.Weekend, y = Number.of.Ratings, color = service)) +
#   geom_point(position = "identity") +
#   labs(title = "IMDb Gross.Opening.Weekend vs. Number.of.Ratings") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1))
# 
# correlation <- gross1 %>%
#   group_by(service) %>%
#   summarise(correlation = cor(Number.of.Ratings, Gross.Opening.Weekend))
# correlation <- rbind(data.frame(service = "all", correlation = cor(gross1$Gross.Opening.Weekend,gross1$Number.of.Ratings)), correlation)
# correlation
# 
# # 首周票房與總票房
# ggplot(gross, aes(x = Gross.Opening.Weekend, y = Gross, color = service)) +
#   geom_point(position = "identity") +
#   labs(title = "IMDb Gross ratio vs. Gross") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1))
# 
# correlation <- gross %>%
#   group_by(service) %>%
#   summarise(correlation = cor(Gross.Opening.Weekend, Gross))
# correlation <- rbind(data.frame(service = "all", correlation = cor(gross$Gross,gross$Gross.Opening.Weekend)), correlation)

###### score & Number.of.Rating & Gross & Gross.Opening.Weekend ######
# 極端值
head(score %>% arrange(desc(score)),10) # 分數超低
head(score %>% arrange(sort(score)),10) # 分數超高
head(score %>% arrange(desc(Number.of.Ratings)),10) # 高話題
head(score %>% arrange(desc(Gross)),10) # 高票房

gross <- gross[which((gross$Number.of.Ratings>10)),] # 話題度在10k up
ggpairs(gross, columns = c("score","Number.of.Ratings", "Gross", "Gross.Opening.Weekend"), 
        mapping = aes(color = service), 
        lower = list(continuous = "points", mapping = aes(alpha = 0.4, fill = service)),
        title = "Pairs plot of Number.of.Rating Gross by service") +
  scale_colour_manual(values = c("#619CFF", "#00BFC4", "#F8766D")) +
  scale_fill_manual(values = c("#619CFF", "#00BFC4", "#F8766D"))

rm(gross,gross1)
rm(score,score1,score2,score3)

####### country #######
dat$country[is.na(dat$country)] <- 'Other'
# Amazon_prime
country_a <- dat %>% select(service, country)%>% filter(service == "Amazon prime")
country_list <- list()
for(i in 1:length(country_a$country)){
  country_list[[i]] <- strsplit(country_a$country[i], ",")
}
country_a <- data.frame(country = unlist(country_list))
country_a$country <- trimws(country_a$country)
country_a <- country_a %>% count(country, sort = T)

# Disney+
country_d <- dat %>% select(service, country)%>% filter(service == "Disney+")
country_list <- list()
for(i in 1:length(country_d$country)){
  country_list[[i]] <- strsplit(country_d$country[i], ",")
}
country_d <- data.frame(country = unlist(country_list))
country_d$country <- trimws(country_d$country)
country_d <- country_d %>% count(country, sort = T)

# Netflix
country_n <- dat %>% select(service, country)%>% filter(service == "Netflix")
country_list <- list()
for(i in 1:length(country_n$country)){
  country_list[[i]] <- strsplit(country_n$country[i], ",")
}
country_n <- data.frame(country = unlist(country_list))
country_n$country <- trimws(country_n$country)
country_n <- country_n %>% count(country, sort = T)

country <- merge(merge(country_a, country_d, by = 'country', all = TRUE), country_n, by = 'country', all = TRUE)
country[is.na(country)] <- 0
colnames(country) <- c('country', 'Amazon_prime', 'Disney+', 'Netflix')
country$sum = country$Amazon_prime+country$Disney+country$Netflix
country <- head(country[order(-country$sum),],6)
country <- country[-which(country$country=='Other'),-5]


country <- tidyr::gather(country , key = "serivce", value = "Count", -country)

ggplot(country, aes(x = reorder(country, Count), y = Count, fill = serivce)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Count by Country", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  coord_flip() +
  scale_fill_manual(values = c("#619CFF", "#00BFC4", "#F8766D"))

rm(country,country_a,country_d,country_n,country_list)

####### release_year #######
year <-  dat %>%
  select(service, year = release.year) %>%
  group_by(service, year) %>%
  summarise(count = n())
year <- na.omit(year)
colnames(year) <- c("service","year", "count")
year$year <- as.numeric(as.character(year$year))
year <- year[order(year$year), ]
year$year <- as.Date(paste0(year$year, "-01-01"))
year <- year %>%
  group_by(service, year) %>%
  summarise(count = sum(count)) %>%
  ungroup()


ggplot(year, aes(x = year, y = count, color = service)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "Number of service", x = "Year", y = "Count", color = "service") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("Amazon prime" = "#619CFF", "Disney+" = "#00BFC4", "Netflix" = "#F8766D"),
                     labels = c("Amazon prime", "Disney+", "Netflix"))


rm(year,year_sum);gc()
####### rating #######
rating <-  dat %>%
  select(service, rating) 
rating <- na.omit(rating)

# 修正成美國電影分級
rating$rating <- case_when(
  rating$rating == "13+" ~ "PG-13",  rating$rating == "16+" ~ "PG-13",
  rating$rating == "18+" ~ "R",  rating$rating == "7+" ~ "G",  rating$rating == "AGES_16_" ~ "PG-13",
  rating$rating == "ALL" ~ "G",  rating$rating == "G" ~ "G",  rating$rating == "NR" ~ "NR",
  rating$rating == "PG" ~ "PG",  rating$rating == "PG-13" ~ "PG-13",
  rating$rating == "R" ~ "R",  rating$rating == "UNRATED" ~ "NR",
  rating$rating == "TV-14" ~ "PG-13",  rating$rating == "TV-G" ~ "G",
  rating$rating == "TV-PG" ~ "PG",  rating$rating == "TV-Y" ~ "G",
  rating$rating == "TV-MA" ~ "NC-17",  rating$rating == "TV-Y7" ~ "PG",
  rating$rating == "UR" ~ "NR",  TRUE ~ "Other")
rating <- rating %>% group_by(service, rating) %>% summarise(count = n())
colnames(rating) <- c("service","rating", "count")


ggplot(rating, aes(x = reorder(rating, count), y = count, fill = service)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count by Rating", x = "Rating", y = "Count") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  scale_fill_manual(values = c("Amazon prime" = "#619CFF", "Disney+" = "#00BFC4", "Netflix" = "#F8766D"),
                    labels = c("Amazon prime", "Disney+", "Netflix"))

rm(rating);gc()
####### listed in ######
dat %>%
  select(listed.in, service) %>%
  mutate(listed.in = str_split(listed.in, ',')) %>%
  unnest(listed.in) %>%
  group_by(listed.in, service) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(30) %>%
  ggplot() +
  geom_col(aes(y = reorder(listed.in, n), x = n, fill = service)) +
  labs(title = 'Themes / Genre of Titles',
       x = '# of Titles',
       y = 'Theme / Genre',
       fill = 'Service') +
  scale_fill_manual(values = c("Amazon prime" = "#619CFF", "Disney+" = "#00BFC4", "Netflix" = "#F8766D"),
                    labels = c("Amazon prime", "Disney+", "Netflix")) +
  theme_minimal()


####### duration #######
dat$duration <- as.numeric(gsub(paste(c('min'), collapse = "|"), "", dat$duration))

# Amazon prime
p1 <- dat %>%
  select(duration, service) %>%
  filter(service == "Amazon prime") %>%
  ggplot() +
  geom_histogram(aes(x = duration), binwidth = 1, fill = "#619CFF") +
  labs(title = "Duration Distribution on Amazon prime",
       x = "Duration") +
  theme_minimal()

# Disney+
p2 <- dat %>%
  select(duration, service) %>%
  filter(service == "Disney+") %>%
  ggplot() +
  geom_histogram(aes(x = duration), binwidth = 1, fill = "#00BFC4") +
  labs(title = "Duration Distribution on Disney+",
       x = "Duration") +
  theme_minimal()

# Netflix
p3 <- dat %>%
  select(duration, service) %>%
  filter(service == "Netflix") %>%
  ggplot() +
  geom_histogram(aes(x = duration), binwidth = 1, fill = "#F8766D") +
  labs(title = "Duration Distribution on Netflix",
       x = "Duration") +
  theme_minimal()

grid.arrange(p1,p2,p3,nrow =1, ncol = 3)

ggplot(data = dat, mapping = aes(x = service, y = duration, fill = service)) +
  geom_boxplot() +
  theme(axis.text.x  = element_blank()) +
  labs(fill = "Service", title = "Service of duration") +
  scale_fill_manual(values = c("Amazon prime" = "#619CFF", "Disney+" = "#00BFC4", "Netflix" = "#F8766D"),
                    labels = c("Amazon prime", "Disney+", "Netflix"))

rm(p1,p2,p3)

# 極端值
dat[which(dat$duration>180),c(1,7,17)]

####### description #######
dat$description <- paste(dat$description, dat$description.IMDb, sep = "")
words <- dat %>% select(description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
count_word <- words %>%count(word, sort = TRUE)
wordcloud(words = count_word$word,freq = count_word$n, 
          min.freq = 50,max.words = nrow(count_word), 
          random.order = FALSE,rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2"))

rm(words,count_word)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(GGally)
library(caret)
data <- data.o <- read.csv('"D:\\nagisa\\NAGISA\\求職\\作品資料\\Student Performance\\StudentsPerformance.csv"')

# head
head(data)

#### 增加 row 總分做為評分依據 ####
data <- data %>% mutate(score = (math.score + reading.score + writing.score)/3)%>%
  mutate(Rank = ifelse(score>=90,'A',
                ifelse(score>=85,'A',
                ifelse(score>=80,'A',
                ifelse(score>=77,'B',
                ifelse(score>=73,'B',
                ifelse(score>=70,'B',
                ifelse(score>=67,'C',
                ifelse(score>=63,'C',
                ifelse(score>=60,'C',
                ifelse(score < 60,'F',NA)))))))))))

#### variable ####
col <- colnames(data)

#### data discribe ####
summary(data)
table(data$gender)
table(data$race.ethnicity)
table(data$parental.level.of.education)
table(data$lunch)
table(data$test.preparation.course)


#### ggplot ####
##### 不同學科的表現 #####
girl_data<-data%>%filter(gender=='female')
boy_data<-data%>%filter(gender=='male')

ggpairs(data, columns = c("math.score", "reading.score", "writing.score"), 
        mapping = aes(color = gender), 
        lower = list(continuous = "points", mapping = aes(alpha = 0.5)),
        title = "Pairs plot of Math, Reading, and Writing scores by gender")

# 平均分
ggplot(data, aes(x = score)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Score") +
  theme(plot.background = element_rect(fill = "transparent", color = NA))

# rank
ggplot(data, aes(x = Rank,fill = Rank)) +
  geom_bar() +
  labs(title = "Rank")

# 各組間的分數
data%>%
  group_by(race.ethnicity)%>%
  summarize(avg_score=round(sum(score)/n(),1))%>%
  ggplot(aes(race.ethnicity,avg_score,fill=race.ethnicity))+geom_bar(stat='identity')+
  geom_text(aes(label = avg_score))+coord_flip()+labs(title='Average score of each group')+guides(fill=F)

##### 分數與變數之間的關係 #####
p1 <- ggplot(data = data, mapping = aes(x = gender, y = score, fill = gender)) + 
  geom_boxplot() +
  theme(axis.text.x  = element_blank()) +
  labs(title = "Gender")

p2 <- ggplot(data = data, mapping = aes(x = race.ethnicity, y = score, fill = race.ethnicity)) + 
  geom_boxplot() +
  theme(axis.text.x  = element_blank()) +
  labs(title = "Group")

p3 <- ggplot(data = data, mapping = aes(x = parental.level.of.education, y = score, fill = parental.level.of.education)) + 
  geom_boxplot() +
  # theme(axis.text.x  = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
  theme(axis.text.x  = element_blank()) +
  labs(fill = "level of education", title = "Parental level of education")

p4 <- ggplot(data = data, mapping = aes(x = test.preparation.course, y = score, fill = test.preparation.course)) + 
  geom_boxplot() +
  theme(axis.text.x  = element_blank()) +
  labs(fill = "course preparation", title = "Test preparation course")

p5 <- ggplot(data = data, mapping = aes(x = lunch, y = score, fill = lunch)) + 
  geom_boxplot() +
  theme(axis.text.x  = element_blank()) +
  labs(title = "Lunch")

grid.arrange(p1,p2,p3,p4,p5,nrow =2, ncol = 3)

# p <- ggplot(data = data, mapping = aes(x = race.ethnicity, y = score, fill = gender)) + 
#   geom_boxplot() +
#   labs(title = "Group with gender")
# 
# p <- ggplot(data = data, mapping = aes(x = race.ethnicity, y = score, fill = test.preparation.course)) + 
#   geom_boxplot() +
#   labs(fill = "course preparation",title = "Group with course preparation")
# 
# p <- ggplot(data = data, mapping = aes(x = race.ethnicity, y = score, fill = lunch)) + 
#   geom_boxplot() +
#   labs(title = "Group with lunch")

#### t test ####
test <- data %>% select(score,race.ethnicity)%>%group_by(race.ethnicity)
test <- test[which((test$race.ethnicity=='group E') |(test$race.ethnicity=='group A')),]
GE <- test[which(test$race.ethnicity=='group E'),]
GA <- test[which(test$race.ethnicity=='group A'),]
summary(GA)
summary(GE)
t.test(GA$score,GE$score)

ggplot(data = test, mapping = aes(x = race.ethnicity, y = score, fill = race.ethnicity)) + 
  geom_boxplot() +
  theme(axis.text.x  = element_blank()) +
  labs(title = "Group")


#### fit model ####
data.fit1 <- data %>% select(G = gender,R = race.ethnicity,P = parental.level.of.education,
                              L = lunch, T = test.preparation.course,score)

data.fit2 <- data %>% select(gender,race.ethnicity,parental.level.of.education,
                            lunch,test.preparation.course,score) %>%
            mutate(Rank = ifelse(score>=90,'A',
                            ifelse(score>=85,'A',
                              ifelse(score>=80,'A',
                                ifelse(score>=77,'B',
                                  ifelse(score>=73,'B',
                                    ifelse(score>=70,'B',
                                      ifelse(score>=67,'C',
                                        ifelse(score>=63,'C',
                                          ifelse(score>=60,'C',
                                            ifelse(score < 60,'F',NA))))))))))) %>%
  select(G = gender,R = race.ethnicity,P = parental.level.of.education,
         L = lunch, T = test.preparation.course,Rank)

# formula
formula <- round(score) ~ .^2

# ANOVA
summary(aov(formula, data = data.fit1))

# GLM
fit <- glm(formula, family = poisson, data.fit1)
aic <- step(fit ,direction="both",scope=list(lower=~1,upper=~.),trace=0)
fit.final <- glm(aic, family = poisson, data.fit1)

fit <- lm(formula, data.fit1)
aic <- step(fit ,direction="both",scope=list(lower=~1,upper=~.),trace=0)
fit.final <- lm(aic, data.fit1)

summary(fit.final)


# 過度離散
sum(residuals(fit.final,type="pearson")^2)/fit.final$df.res
with(summary(fit.final), 1 - deviance/null.deviance)
with(fit.final, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


#### ML ####
train.control <- trainControl(method = "cv", number = 5)

model <- train(formula, data = data.fit1, method = "rf",trControl = train.control)
model <- train(formula, data = data.fit1, method = "svmRadial",trControl = train.control)
model <- train(formula, data = data.fit1, method = "knn",trControl = train.control)
model <- train(formula, data = data.fit1, method = "bayesglm",trControl = train.control)

print(model)

model <- train(Rank ~., data = data.fit2, method = "rf",trControl = train.control)
model <- train(Rank ~., data = data.fit2, method = "svmRadial",trControl = train.control)
model <- train(Rank ~., data = data.fit2, method = "knn",trControl = train.control)
model <- train(Rank ~., data = data.fit2, method = "knn",trControl = train.control)

print(model)



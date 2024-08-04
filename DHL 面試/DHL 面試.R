library(dplyr)
library(ggplot2)
library(forecast)

data <- read.csv("Doc 1.csv")
data2 <- read.csv("Doc 2.csv")

summary(data)
summary(data2)

class(data$Loading.date)

#### commix table ####
merged_df <- merge(data, data2, by = "File.number", all = TRUE)

#### data 整理 ####
data$Loading.date <- as.Date(data$Loading.date, format = "%d-%m-%y")
data$Unloading.date <- as.Date(data$Unloading.date, format = "%d-%m-%y")
data2$Loading.date <- as.Date(data2$Loading.date, format = "%d-%m-%y")
data2$Unloading.date <- as.Date(data2$Unloading.date, format = "%d-%m-%y")

dat <- data.frame(data$Loading.date)
order_counts <- dat %>%
  group_by(data.Loading.date) %>%
  summarise(count = n())

ggplot(order_counts, aes(x = data.Loading.date, y = count)) +
  geom_line() +          # 繪製折線圖
  geom_point() +         # 繪製數據點
  labs(title = "data.Loading Count", x = "Date", y = "Count") +
  theme_minimal()        # 使用簡約主題

##### model ####
start_date <- min(order_counts$data.Loading.date)
end_date <- max(order_counts$data.Loading.date)
date_seq <- seq.Date(from = start_date, to = end_date, by = "day")

# 轉換銷售數據為時間序列
sales_ts <- ts(order_counts$count, 
               start = c(as.numeric(format(start_date, "%Y")), as.numeric(format(start_date, "%j"))), frequency = 365)

# 構建ARIMA模型
fit <- auto.arima(sales_ts)
summary(fit)

# 預測未來30天的銷售量
forecasted_sales <- forecast(fit, h = 30)

# 繪製預測結果
autoplot(forecasted_sales) +
  labs(title = "訂單量預測", x = "日期", y = "訂單量")



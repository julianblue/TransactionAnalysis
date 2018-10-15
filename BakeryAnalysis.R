library(ggplot2)
library(tidyverse)
library(ggpubr)
library(lubridate)

data <- read.csv("/Users/julianblau/Downloads/BreadBasket_DMS.csv")

View(data)

str(data)

data <- data %>%
  mutate(
    Date = as.Date(Date),
    Time = hms(Time)
  )
data$Weekdays <- weekdays(data$Date)

aggregate(data["Date"], by = data["Date"], )

data.items <- data %>% group_by(Item) %>% summarise(count=n())
data.items <- data.items[order(-data.items$count),]
data.items

attach(data.items)

ggplot(data.items[1:10,], aes(x = reorder(Item, -count), y = count))+
  geom_bar(stat = "identity")


items <- unique(data$Item)
items
View(items)
items <- sort(items, decreasing = F)

data.days <- data %>% group_by(Date) %>% summarise(count=n())
data.days$month <- as.Date(cut(data.days$Date, breaks = "month"))
data.days

attach(data.days)
ggplot(data.days, aes(month, count))+
  geom_bar(stat = "identity")+
  scale_x_date(date_labels = "%B", date_breaks = "1 month")

data.days$weekday <- weekdays(as.Date(data.days$Date))
data.days
data.days.desc <- data.days[order(-count), ]
data.days.desc
data.weekday.rev <- aggregate(data.days.desc$count, by=list(Category=data.days.desc$weekday), FUN=sum)
data.weekday.rev <- data.weekday.rev[order(-data.weekday.rev$x), ]
data.weekday.rev
data.weekday.rev$percent <- (prop.table(data.weekday.rev$x)*100)
data.weekday.rev

sales.days <- unique(data$Date)
sales.days <- as.data.frame(sales.days)
colnames(sales.days) <- "Date"
sales.days$weekday <- weekdays(as.Date(Date))
sales.days
sales.days.sum <- sales.days %>% group_by(weekday) %>% summarise(count=n())
sales.days.sum

?colnames
ggplot(data.days.desc, aes(x="", y=count, fill=weekday))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)

ggplot(data.days.desc, aes(x=reorder(weekday, -count), y=count, fill=weekday))+
  geom_bar(stat = "identity")

coffee.sales <- aggregate(data$Item == "Coffee", by=list(Category=data$Weekdays), FUN=sum)
coffee.sales$percent <- (prop.table(coffee.sales$x)*100)  
coffee.sales

data$hour <- round_date(data$Time, unit = "hour")
data
?round_date()

ggplot(aes(x = Time, y = variable), data = data) + geom_point()


b <- data %>%
  mutate(
    Hour = as.factor(hour(Time))
  ) %>%
  group_by(Hour, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = Hour, y = Transactions, fill = Hour)
  ) +
  geom_boxplot() +
  theme(
    legend.position="none" 
  ) +
  labs(x = "Hour", y = "Items / transaction", title = "Items per transaction (per hour)")
b


a <- data %>%
  mutate(
    Hour = as.factor(hour(Time))
  ) %>%
  group_by(Hour, Transaction) %>%
  summarize(
    Transactions = n_distinct(Item)
  ) %>%
  ggplot(
    aes(x = Hour, fill = Hour)
  ) +
  geom_histogram(stat="count") +
  theme(
    legend.position="none" 
  ) +
  labs(x = "Hour", y = "Transactions", title = "Transactions per hour")

a



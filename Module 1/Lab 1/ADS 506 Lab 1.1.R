library(tidyverse)
set.seed(506)

# get our data
dgf_sales <- read_csv("Data/DGF Sales FY21-22.csv")

View(dgf_sales)


str(dgf_sales)

dgf_sales$Date <- as.Date(dgf_sales$Date, format = "%m/%d/%Y")

summary(dgf_sales)


dgf_daily <- dgf_sales %>%
  group_by(Date) %>%
  summarise(Total  = sum(Subtotal))


summary(dgf_daily)


ggplot(dgf_daily, aes(Date, Total)) +
  geom_line() +
  theme_classic()


dgf_monthly <- dgf_sales %>%
  group_by(Month = format(Date, "%Y-%m")) %>%
  summarise(Total = sum(Subtotal))

ggplot(dgf_monthly, aes(x = Month, y = Total, group = 1)) +
  geom_line(size = 1, color = "green") +
  theme_classic()


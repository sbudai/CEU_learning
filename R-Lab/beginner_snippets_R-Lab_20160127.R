library(dplyr)
library(ggplot2)

str(mtcars)
table(mtcars$carb)
table(mtcars$am)
table(mtcars$hp)

ggplot(data = mtcars, aes(x = carb*am)) + 
  geom_bar()
ggplot(data = mtcars, aes(x = carb*am)) + 
  geom_bar() + 
  facet_grid(~ am)

mtcars$am <- as.factor(mtcars$am)
ggplot(data = mtcars, aes(x = carb, fill = am), position = 'dodge') + 
  geom_bar()

mtcars$am <- as.numeric(mtcars$am)
ggplot(data = mtcars, aes(x = factor(carb), y = hp)) + 
  geom_boxplot()
ggplot(data = mtcars, aes(x = hp, y = wt, col = factor(carb))) + 
  geom_boxplot() + 
  geom_point()

library(XML)
df <- readHTMLTable(readLines('https://en.wikipedia.org/wiki/FTSE_100_Index'), which = 2, header = TRUE)
str(df)
df <- readHTMLTable(readLines('https://en.wikipedia.org/wiki/FTSE_100_Index'), which = 2, header = TRUE, stringsAsFactors = FALSE)
str(df)

names(df)
colnames(df)[4] <- 'Cap'
names(df)

df$Cap <- as.numeric(df$Cap)
df$Employees <- as.numeric(gsub(',', '', df$Employees))
str(df)

subset(df, Employees < 1000)
subset(df, Employees < 1000 & Cap < 5)

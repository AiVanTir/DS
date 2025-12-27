library(ggplot2)

# файл с доходами
input <- read.table(
  "D:\\projects\\DS\\data\\zipIncome.txt",
  header = TRUE,
  sep = "|"
)

# меняем название колоно
colnames(input) <- c("zipCode", "income")

# a) тут делаем краткий обзор данных и считаем среднее и медиану
input$zipCode <- factor(input$zipCode)

summary(input)

mean_income  <- mean(input$income, na.rm = TRUE)
median_income <- median(input$income, na.rm = TRUE)

mean_income
median_income


# b) рисуем обычный scatter plot дохода от кода
# он не очень информативный но можно увидеть выбросы
plot(
  input$zipCode, input$income
)

# через boxplot можно быстро вытащить выбросы
outliers <- boxplot(input$income, plot = FALSE)$out
outliers


# выкидываем слишком маленькие и слишком большие доходы
# оставляем только 7000 < income < 200000
income_filt <- subset(input, income > 7000 & income < 200000)


# c) считаем новое среднее уже после фильтрации
new_mean <- mean(income_filt$income, na.rm = TRUE)
new_mean


# d) строим boxplot по отфильтрованным данным
boxplot(
  income_filt$income
)


# e) тут тот же boxplot но по логарифмической шкале
# так лучше видно различия между zip кодами
boxplot(
  log10(income_filt$income)
)

# как вообще распределены логарифмы доходов
hist(
  log10(income_filt$income)
)


# f) делаем ggplot
p_scatter <- ggplot(income_filt, aes(x = zipCode, y = income)) +
  geom_point(position = "jitter", alpha = 0.2) +
  scale_y_log10()

p_scatter


# g) к этому же графику добавляем boxplot и раскрашиваем точки по zip
p_scatter_box <- ggplot(income_filt, aes(x = zipCode, y = income, color = zipCode)) +
  geom_point(position = "jitter", alpha = 0.2) +
  geom_boxplot(alpha = 0.3, outlier.size = 0.5) +
  scale_y_log10()

p_scatter_box

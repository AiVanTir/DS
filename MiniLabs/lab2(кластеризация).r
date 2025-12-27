library(ggplot2)
library(dplyr)
library(maps)

# загружаем датасет со штатами
# тут для каждого штата есть средний доход и сколько электричества они потребляют
load("D:\\projects\\DS\\MINI_LABS\\3.cluster\\data\\income_elec_state.Rdata")

data <- income_elec_state
data$state <- rownames(data)
rownames(data) <- NULL

# глянем на первые строки чтобы понять структуру
head(data)



# чтобы результаты k-means не прыгали от запуска к запуску
set.seed(1)

k <- 10
model10 <- kmeans(data[, c("income", "elec")], centers = k, nstart = 25)

cols <- rainbow(k)

# здесь каждый штат это точка
# цвет показывает к какому кластеру он относится
plot(
  data$income, data$elec,
  col = cols[model10$cluster],
  pch = 19,
  xlab = "Средний доход семьи",
  ylab = "Среднее потребление электричества",
  main = "K-means, k = 10"
)

# это центры кластеров
points(
  model10$centers[,1], model10$centers[,2],
  pch = 8, cex = 2, lwd = 2
)

# по картинке видно что штаты реально делятся на группы
# есть богатые и энергозатратные, есть бедные и более экономные



# если убрать set.seed и запустить kmeans несколько раз
# кластеры будут немного меняться
# это потому что начальные центры выбираются случайно
# set.seed и nstart помогают получить более стабильный результат



# считаем elbow чтобы понять сколько кластеров имеет смысл брать
wss <- sapply(1:15, function(x){
  kmeans(data[, c("income","elec")], centers = x, nstart = 25)$tot.withinss
})

plot(
  1:15, wss, type="b", pch=19,
  xlab = "k (число кластеров)",
  ylab = "Сумма внутрикластерных квадратов",
  main = "Elbow-график"
)

# по графику видно что после k около 5 кривая выпрямляется
# дальше увеличение числа кластеров уже почти ничего не дает
# поэтому 5 выглядит как нормальный компромисс



# теперь переводим доход и электричество в логарифмы
# так большие значения перестают доминировать
logData <- data
logData$income <- log10(data$income)
logData$elec   <- log10(data$elec)

modelLog <- kmeans(logData[, c("income","elec")], centers = 4, nstart = 25)

plot(
  logData$income, logData$elec,
  col = modelLog$cluster,
  pch = 19,
  xlab = "log10(доход)",
  ylab = "log10(электричество)",
  main = "K-means после логарифмирования"
)

points(modelLog$centers[,1], modelLog$centers[,2], pch=8, cex=2)

# в лог шкале точки распределены ровнее
# кластеры становятся более читаемыми и аккуратными



# снова считаем elbow, но уже для логарифмированных данных
wss_log <- sapply(1:15, function(x){
  kmeans(logData[, c("income","elec")], centers = x, nstart = 25)$tot.withinss
})

plot(
  1:15, wss_log, type="b", pch=19,
  xlab = "k (число кластеров)",
  ylab = "Сумма внутрикластерных квадратов",
  main = "Elbow для логарифмированных данных"
)

# здесь излом тоже примерно около 4
# это значит что логарифмирование не меняет оптимальное k,
# но делает разбиение стабильнее



# ищем сильный выброс
# почти всегда здесь вылезает Puerto Rico
subset(logData, income < 4.5 & elec < 2.85)

# убираем PR из данных
clean <- logData[logData$state != "PR", ]

# еще раз смотрим elbow уже без выброса
wss2 <- sapply(1:15, function(x){
  kmeans(clean[, c("income","elec")], centers = x, nstart = 25)$tot.withinss
})

plot(
  1:15, wss2, type="b", pch=19,
  xlab = "k (число кластеров)",
  ylab = "Сумма внутрикластерных квадратов",
  main = "Elbow без Puerto Rico"
)

# и делаем кластеризацию снова
modelClean <- kmeans(clean[, c("income","elec")], centers = 4, nstart = 25)

plot(
  clean$income, clean$elec,
  col = modelClean$cluster,
  pch = 19,
  xlab = "log10(доход)",
  ylab = "log10(электричество)",
  main = "Кластеры без Puerto Rico"
)

points(modelClean$centers[,1], modelClean$centers[,2], pch=8, cex=2)

# без Puerto Rico структура кластеров выглядит ровнее
# выбор k = 4 остается самым логичным



# раскрашиваем карту США по кластерам

mapData <- data %>%
  filter(!state %in% c("PR","AK","HI")) %>%
  mutate(
    income_log = log10(income),
    elec_log   = log10(elec)
  )

set.seed(1)
mapModel <- kmeans(mapData[, c("income_log","elec_log")], centers = 4, nstart = 25)
mapData$cluster <- mapModel$cluster

# переводим аббревиатуры штатов в полные названия
mapData$state_full <- tolower(state.name[match(mapData$state, state.abb)])
mapData$state_full[mapData$state == "DC"] <- "district of columbia"

us <- map_data("state")

joined <- us %>% left_join(mapData, by = c("region" = "state_full"))

ggplot(joined, aes(long, lat, group = group, fill = factor(cluster))) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  labs(title = "Кластеры штатов по доходу и потреблению электричества")



# иерархическая кластеризация

distM <- dist(data[, c("income","elec")])

hc_single   <- hclust(distM, method="single")
hc_complete <- hclust(distM, method="complete")
hc_average  <- hclust(distM, method="average")
hc_ward     <- hclust(distM, method="ward.D2")

par(mfrow=c(2,2))
plot(hc_single,   main="Single linkage")
plot(hc_complete, main="Complete linkage")
plot(hc_average,  main="Average linkage")
plot(hc_ward,     main="Ward linkage")

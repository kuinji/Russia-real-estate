---
title: Предсказание_цен_на_недвижимость
author: Шароватов_Даниил
date: \today
toc: true
toc-depth: 2
navigation: frame
fontenc: T2A
lang: russian
babel-lang: russian
---
  
library(ggplot2)
library(ggpubr)
library(dplyr)
library(car)
library(scales)
library(patchwork)
library(gvlma)
library(caTools)
library(caret)
library(corrplot)
library(Metrics)
library(viridis)
library(lm.beta)
library(rpart)
library(rpart.plot)
library(lmtest)
library(car)

# Импорт данных
estate_price_data <- read.csv("C:/Users/User/source/datasets/input_data.csv", sep=";")

dim(estate_price_data)
str(estate_price_data)

categorical_vars = c("object_type", "building_type")
estate_price_data[categorical_vars] <- lapply(estate_price_data[categorical_vars], as.factor)

lapply(estate_price_data, function(x) {
  value_counts <- sort(table(x), decreasing = TRUE)
  head(value_counts, 10)
})

# Доля пропущенных значений в каждом столбце
num_rows <- nrow(estate_price_data)
sapply(estate_price_data, function(x) sum(is.na(x) | x == -1 | x == -100) / num_rows)

# Удаление столбцов, не несущих ценности 
columns_to_remove <- c("street_id", "house_id", "postal_code")
estate_price_data <- estate_price_data[ , !(names(estate_price_data) %in% columns_to_remove)]

# Обработка пропущенных значений
###
### todo: Добавить модель для предсказания количества комнат и площади кухни
###
median_rooms <- median(estate_price_data$rooms[estate_price_data$rooms != -1], na.rm = TRUE)
estate_price_data$rooms[estate_price_data$rooms == -1] <- median_rooms
median_kitchen_area <- median(estate_price_data$kitchen_area[estate_price_data$kitchen_area != -100], na.rm = TRUE)
estate_price_data$kitchen_area[estate_price_data$kitchen_area == -100] <- median_kitchen_area

# Проверка, что все пропущенные значения были обработаны
sapply(estate_price_data, function(x) sum(is.na(x) | x == -100) / num_rows)

# Удаление выбросов
print_extremums <- function (data, columns, extremums_count) {
  for (col in columns) {
  max_values <- head(sort(data[[col]], decreasing = TRUE), extremums_count)
  min_values <- head(sort(data[[col]]), extremums_count)
  cat("Столбец:", col, "\n")
  cat("Максимумы:", paste(max_values, collapse = ", "), "\n")
  cat("Минимумы:", paste(min_values, collapse = ", "), "\n\n")
  }
}
columns_to_analyze <- c("level", "levels", "rooms", "area", "kitchen_area", "geo_lat", "geo_lon", "price")
print_extremums(estate_price_data, columns_to_analyze, 5)

###
### Значит ли площадь кухни == 0, что кухни нет или это ошибка в данных?
###
estate_price_data <- estate_price_data %>%
  filter(
    area >= quantile(area, 0.0001),
    area <= quantile(area, 0.999),
    level != 0,
    levels != 0,
    kitchen_area >= 0, 
    kitchen_area <= quantile(kitchen_area, 0.999), 
    price >= quantile(price, 0.001),  
    price <= quantile(price, 0.999), 
  )
print_extremums(estate_price_data, columns_to_analyze, 5)

# Подсоединим столбец с названиями регионов из другого набора данных
###
### todo: Добавить данные по регионам (например, средняя заработная плата, население)
###
Sys.setlocale("LC_CTYPE", "russian")
region_data <- read.csv2("C:/Users/User/source/datasets/regions.csv", sep = ",", encoding = "UTF-8")
estate_price_data <- merge(estate_price_data, region_data[, c("region_code", "name_with_type")], 
      by.x = "id_region", by.y = "region_code", all.x = TRUE)
colnames(estate_price_data)[colnames(estate_price_data) == "name_with_type"] <- "region_name"
# Удалим строки с неверными значениями
sapply(estate_price_data, function(x) sum(is.na(x) | x == -1 | x == -100) / num_rows)
estate_price_data <- estate_price_data[!is.na(estate_price_data$region_name), ]

# Координаты центра Нижнего Новгорода
city_center_lat <- 56.3269
city_center_lon <- 44.0075

# Функция расчета расстояния
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  delta_lat <- (lat2 - lat1) * pi / 180
  delta_lon <- (lon2 - lon1) * pi / 180
  a <- sin(delta_lat / 2)^2 + 
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# Добавление расстояния в датасет
estate_price_data$distance_from_center <- mapply(
  haversine_distance,
  lat1 = estate_price_data$geo_lat,
  lon1 = estate_price_data$geo_lon,
  lat2 = city_center_lat,
  lon2 = city_center_lon
)

data_nn <- estate_price_data %>% filter(distance_from_center <= 15)

# Регионы
sorted_data <- estate_price_data %>%
  group_by(region_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 30)
ggplot(sorted_data, aes(x = reorder(region_name, count), y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Количество наблюдений по регионам", x = "", y = "") +
  coord_flip() +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12)
  )
dim(estate_price_data[estate_price_data$id_region==52,])
# Цена
cat("Средняя цена недвижимости M(price) =", mean(estate_price_data$price), "рублей со среднем квадратическим Sd(price) =", sd(estate_price_data$price))
ggplot(estate_price_data, aes(x=price)) + 
  geom_histogram(color='white') +
  labs(title = "Гистограмма цен на недвижимость", x = "Цена (млн.руб.)", y = "Количество наблюдений (тыс.)") +
  scale_x_continuous(labels = label_comma(scale = 1e-6)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = label_comma(scale = 1e-3)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(estate_price_data, aes(x=price)) + 
  geom_boxplot() +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(estate_price_data, aes(x=price)) + 
  geom_histogram(color='white') +
  labs(title = "Гистограмма цен на недвижимость, лог. шкала", x = "Цена (млн.руб.)", y = "Количество наблюдений (тыс.)") +
  scale_x_continuous(trans = "log10", labels = label_comma(scale = 1e-6), breaks = log_breaks(n = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = label_comma(scale = 1e-3)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Сэмплирование 1% данных для построения Q-Q plot
sample_data <- estate_price_data[sample(nrow(estate_price_data), size = 0.001 * nrow(estate_price_data)), ]
ggqqplot(log10(sample_data$price))

# Площадь
area_hist <- ggplot(estate_price_data, aes(x=area)) + 
  geom_histogram(color='white') +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = label_comma(scale = 1e-3)) +
  labs(title = "Гистограмма общей площади", x = "Площадь (кв.метры)", y = "Количество наблюдений (тыс.)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
kitchen_area_hist <- ggplot(estate_price_data, aes(x=kitchen_area)) + 
  geom_histogram(color='white') +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = label_comma(scale = 1e-3)) +
  labs(title = "Гистограмма площади кухни", x = "Площадь (кв.метры)", y = "Количество наблюдений (тыс.)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
(area_hist / kitchen_area_hist) 
rm(area_hist)
rm(kitchen_area_hist)

ggplot(sample_data, aes(x = area, y = kitchen_area)) +
  geom_point(shape = 20, size = 2, fill = "black", color="black", alpha = 0.3) +
  labs(title = "Связь общей площади и площади кухни", x = "Общая площадь (кв.метры)", y = "Площадь кухни (кв.метры)") +
  theme_minimal() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5)
  )

cor.test(sample_data$area, sample_data$kitchen_area)

# Количество комнат
ggplot(estate_price_data, aes(x = rooms)) +
  geom_bar(fill = "#56B4E9") +
  labs(title = "Распределение по количеству комнат", x = "Количество комнат", y = "Количество наблюдений (тыс.)") +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), size = 5, color = "#D55E00") +
  scale_x_continuous(breaks = pretty_breaks(n = 9)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = label_comma(scale = 1e-3)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5)
  )
cor(estate_price_data$area, estate_price_data$kitchen_area)
data_kitchen_filter <- estate_price_data[estate_price_data$kitchen_area!=0,]
cor(data_kitchen_filter$area, data_kitchen_filter$kitchen_area)

# Связь цены и площади
ggplot(estate_price_data, aes(x = area, y = price)) +
  geom_hex(bins = 50, aes(fill = log(..count..))) +
  scale_fill_viridis(option = "C", discrete = FALSE) +
  labs(title = "Связь цены и площади", x = "Площадь (кв.метры)", y = "Цена (млн.руб.)", fill = "log(n)") +
  scale_y_continuous(labels = label_comma(scale = 1e-6)) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
# Разделение на регионы
filtered_data <- estate_price_data %>%
  filter(region_name %in% sorted_data[0:5, ]$region_name)
sample_filtered_data <- filtered_data %>%
  group_by(region_name) %>%
  sample_n(0.005 * nrow(filtered_data) / 5)

ggplot(sample_filtered_data, aes(x = area, y = price, color = region_name, group = region_name)) +
  geom_point(shape = 20, size = 1.5, alpha = 0.2) +
  labs(title = "Связь площади и цены", x = "Площадь (кв.метры)", y = "Цена (млн.руб.)", color = "Регион") +
  scale_y_continuous(labels = label_comma(scale = 1e-6), limits = c(0, 130*10^6)) +
  scale_x_continuous(limits = c(0, 225)) + 
  scale_color_manual(values = c("red", "#984EA3", "#4DAF4A", "#FF7F00", "blue")) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  #theme_minimal() + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5)
  )

data_nn <- estate_price_data[estate_price_data$id_region == 52,]

nrow(data_nn)

ggplot(data_nn, aes(x = area, y = price)) +
  geom_point(shape = 20, size = 1.5, alpha = 0.2, colour = "red") +
  labs(title = "Связь площади и цены", x = "Площадь (кв.метры)", y = "Цена (млн.руб.)") +
  scale_y_continuous(labels = label_comma(scale = 1e-6)) +
  scale_x_continuous(limits = c(0, 225)) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(data_nn, aes(x=distance_from_center)) + 
  geom_histogram(color='white') +
  scale_y_continuous(breaks = pretty_breaks(n = 6), labels = label_comma(scale = 1e-3)) +
  labs(title = "Гистограмма общей площади", x = "Удаленность от центра", y = "Количество наблюдений (тыс.)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


# Первые модели
numeric_cols <- c("level", "levels", "rooms", "area", "kitchen_area", "distance_from_center", "price")
corrplot(cor(data_nn[numeric_cols]), method = "color", addCoef.col = "black")
cor(data_nn)

columns_to_analyze <- c("level", "levels", "rooms", "area", "kitchen_area", "distance_from_center", "price", "building_type", "object_type")
data_for_model <- data_nn[columns_to_analyze]

data_for_model$building_type <- as.factor(data_for_model$building_type)
data_for_model$object_type <- as.factor(data_for_model$object_type)

# Создание dummy-переменных
dummies <- dummyVars(~ building_type + object_type, data = data_for_model)

# Применение преобразования
dummy_data <- predict(dummies, newdata = data_for_model)

# Добавление dummy-переменных обратно в датасет
data_for_model <- cbind(data_for_model, dummy_data)
data_for_model <- data_for_model %>% select(-building_type, -object_type, -building_type.6, -object_type.2)

split <- sample.split(data_for_model$price, SplitRatio = 0.8)
train_data <- subset(data_for_model, split == TRUE)
test_data <- subset(data_for_model, split == FALSE)

linear_model_area <- lm(price ~ area, data=train_data)
summary(linear_model_area)

prediction_data <- data.frame(
  area = seq(min(train_data$area), max(train_data$area), length.out = 240)
)
prediction_data$price <- predict(linear_model_area, newdata = prediction_data)

ggplot(test_data, aes(area, price)) +
  geom_point(shape = 21, size = 2, fill = "grey", alpha = 0.66) + 
  geom_line(data = prediction_data, aes(x = area, y = price), color = 'red', size = 0.8) +
  labs(
    title = "Связь площади и цены",
    x = "Площадь (кв.метры)", 
    y = "Цена (млн.руб.)"
  ) +
  scale_y_continuous(
    labels = label_comma(scale = 1e-6), 
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5)
  )

pred <- predict(linear_model_area, newdata = test_data)

train_mse <- mse(test_data$price, pred)
train_r2 <- cor(test_data$price, pred)^2
cat("Train MSE:", train_mse, "\nTrain R²:", train_r2, "\n")

gvlma(linear_model_area)

log_model_area <- lm(log(price) ~ area, data=data_for_model)
summary(log_model_area)
gvlma(log_model_area)
ggplot(data_for_model, aes(area, log(price))) +
  geom_point(shape = 21, size = 2, fill = "grey", alpha = 0.8) + 
  geom_smooth(method = "lm", se = FALSE, color = 'red', size = 0.5)

linear_model <- lm(price ~ ., data = train_data)
summary(linear_model)

plot(linear_model$fitted.values, residuals(linear_model))
abline(h = 0, col = "red")

qqnorm(residuals(linear_model))
qqline(residuals(linear_model), col = "red")

dwtest(linear_model)

vif_results <- data.frame(vif(linear_model))
vif_df <- data.frame(Variable = rownames(vif_results), VIF = vif_results$GVIF)
high_vif_threshold <- 3
ggplot(vif_df, aes(x = Variable, y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = high_vif_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, max(vif_df$VIF) + 1)) +
  labs(title = "Variance Inflation Factor (VIF) for Regression Model",
       y = "VIF",
       x = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pred <- predict(linear_model, newdata = test_data)

train_mse <- mse(test_data$price, pred)
train_r2 <- cor(test_data$price, pred)^2
cat("Train MSE:", train_mse, "\nTrain R²:", train_r2, "\n")

gvlma(linear_model)

beta <- lm.beta(linear_model)
beta_df <- data.frame(
  Predictor = names(beta$standardized.coefficients)[-1], # Исключаем интерсепт
  StandardizedCoefficient = beta$standardized.coefficients[-1]
)

model <- lm(log(price) ~ ., data = train_data)
summary(model)
gvlma(model)

ggplot(beta_df, aes(x = StandardizedCoefficient, y = reorder(Predictor, StandardizedCoefficient))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Важность признаков",
    x = "Стандартизированный коэффициент",
    y = "Признаки"
  ) +
  theme_minimal()


# Регрессионное дерево
columns_to_analyze <- c("level", "levels", "rooms", "area", "kitchen_area", "distance_from_center", "price", "building_type", "object_type")
data_for_model <- data_nn[columns_to_analyze]
split <- sample.split(data_for_model$price, SplitRatio = 0.8)
train_data <- subset(data_for_model, split == TRUE)
test_data <- subset(data_for_model, split == FALSE)

tree_model <- rpart(price ~ ., data = train_data, method = "anova")
rpart.plot(tree_model, digits = 3)

importance <- tree_model$variable.importance
importance_sorted <- sort(importance, decreasing = FALSE)
par(mar = c(5, 10, 4, 2))
barplot(importance_sorted, main = "Важность признаков", horiz = TRUE, las = 1, col = "lightblue")

test_pred <- predict(tree_model, newdata = test_data)
test_mse <- mse(test_data$price, test_pred)
test_r2 <- cor(test_data$price, test_pred)^2
cat("Test MSE:", test_mse, "\nTest R²:", test_r2, "\n")

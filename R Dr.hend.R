library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggridges)
library(patchwork)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(randomForest)

# ****************  🎨 Heart Disease Theme Colors ****************

heart_colors <- c("#8B0000", "#B22222", "#DC143C", "#FF6F61", "#FFC1C1")

heart_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face="bold", color="#8B0000", size=16),
    axis.title = element_text(face="bold", color="#B22222"),
    axis.text = element_text(color="black"),
    legend.position = "right",
    legend.title = element_text(color="#8B0000", face="bold")
  )


# ****************  1) Load & Inspect Data ****************

df <- read.csv("D:/Applied multivariate analysis/heart_disease_uci.csv")

print(head(df))
View(df)
str(df)
summary(df)
colSums(is.na(df))


# **************** 2) Data Cleaning ****************

df<- subset(df, select=-dataset)
df <- df %>% select(-id)
df <- df %>% select(-ca)

df <- na.omit(df)

df$sex <- ifelse(df$sex == "Female", 1, 2)

table(df$sex)
head(df)
str(df)
summary(df)


# **************** 3) Group Summary ****************

df %>% 
  group_by(num) %>% 
  summarise(
    mean_age = mean(age, na.rm=TRUE),
    median_age = median(age, na.rm=TRUE),
    mean_chol = mean(chol, na.rm=TRUE),
    median_chol = median(chol, na.rm=TRUE),
    mean_trestbps = mean(trestbps, na.rm=TRUE),
    median_trestbps = median(trestbps, na.rm=TRUE)
  )


# **************** 4) Categorical Analysis ****************

table(df$sex)
prop.table(table(df$sex)) * 100
table(df$cp)
table(df$thal)


# **************** 5) Visualizations ****************

# 1️⃣ Age Distribution (Histogram)
p1 <- ggplot(df, aes(x = age, fill = as.factor(num))) +
  geom_histogram(position = "identity", alpha = 0.8, bins = 20, color="black") +
  scale_fill_manual(values = heart_colors) +
  labs(title="Age Distribution by Disease", fill="Disease (num)") +
  heart_theme


# 2️⃣ Chest Pain Type Circular Plot
data_pie <- df %>%
  count(cp, num) %>%
  arrange(num, cp) %>%
  mutate(id = row_number())

number_of_bar <- nrow(data_pie)
angle <- 90 - 360 * (data_pie$id-0.5)/number_of_bar
data_pie$hjust <- ifelse(angle < -90, 1, 0)
data_pie$angle <- ifelse(angle < -90, angle+180, angle)

p2_circular <- ggplot(data_pie, aes(x=as.factor(id), y=n, fill=as.factor(num))) +
  geom_bar(stat="identity", color="black") +
  ylim(-max(data_pie$n)*0.4, max(data_pie$n)*1.2) +
  scale_fill_manual(values = heart_colors) +
  heart_theme +
  coord_polar(start=0) +
  geom_text(aes(x=id, y=n+max(data_pie$n)*0.05, label=cp, hjust=hjust),
            color="black", fontface="bold", size=3, angle=data_pie$angle)


# 3️⃣ Violin Plot Cholesterol
p3 <- ggplot(df, aes(x = as.factor(num), y = chol, fill = as.factor(num))) +
  geom_violin(trim=FALSE, alpha=0.8) +
  geom_jitter(width=0.15, size=1.5, alpha=0.7, color="black") +
  scale_fill_manual(values = heart_colors) +
  labs(title="Cholesterol Levels vs Heart Disease",
       x="Disease (num)", y="Cholesterol") +
  heart_theme


# 4️⃣ Ridge Plot - Blood Pressure
p4 <- ggplot(df, aes(x = trestbps, y = as.factor(num), fill = as.factor(num))) +
  geom_density_ridges(alpha=0.8, scale=1.2) +
  scale_fill_manual(values = heart_colors) +
  labs(title="Resting Blood Pressure Distribution by Disease",
       x="Resting Blood Pressure", y="Disease (num)") +
  heart_theme


# 5️⃣ Scatter Plot
p5 <- ggplot(df, aes(x = thalch, y = oldpeak, color = as.factor(num))) +
  geom_point(size=3, alpha=0.8) +
  scale_color_manual(values = heart_colors) +
  labs(title="Exercise Heart Rate vs ST Depression",
       x="Maximum Heart Rate Achieved",
       y="Oldpeak (ST Depression)") +
  heart_theme

p1; p2_circular
p3
p4
p5


# **************** 6) Correlation Matrix ****************

numeric_df <- df %>% select_if(is.numeric)

cor_matrix <- cor(numeric_df, use = "complete.obs")

corrplot(
  cor_matrix,
  method = "color",
  col = colorRampPalette(heart_colors)(20),
  type = "upper",
  tl.col = "#8B0000",
  tl.srt = 45,
  addCoef.col = "black",   # اللون اللي هيظهر بيه الرقم
  number.cex = 0.7         # حجم الرقم
)


# **************** Modeling Visualization Section ****************

# **************** 7) Modeling & Prediction (Final Fixed) ****************


# تعريف الثيم والألوان
heart_colors <- c("#8B0000", "#B22222", "#DC143C", "#FF6F61", "#FFC1C1")

heart_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face="bold", color="#8B0000", size=16),
    axis.title = element_text(face="bold", color="#B22222"),
    axis.text = element_text(color="black"),
    legend.position = "right",
    legend.title = element_text(color="#8B0000", face="bold")
  )

# --- 1. Data Preparation ---
model_df <- df

# حذف الفئة النادرة
model_df <- model_df[model_df$restecg != "st-t abnormality", ]

# تحويل الأعمدة
model_df$num <- as.factor(model_df$num)
char_cols <- sapply(model_df, is.character)
model_df[char_cols] <- lapply(model_df[char_cols], as.factor)

# تقسيم الداتا
set.seed(123) 
train_index <- sample(1:nrow(model_df), 0.7 * nrow(model_df))
train_data <- model_df[train_index, ]
test_data <- model_df[-train_index, ]


# =====================================================
# Model 1: Decision Tree 🌳
# =====================================================
cat("\n--- 1. Decision Tree Results ---\n")
dt_model <- rpart(num ~ ., data = train_data, method = "class")

# الرسم (التصحيح هنا: استخدام as.list للألوان)
rpart.plot(dt_model, 
           main = "Decision Tree for Heart Disease",
           box.palette = as.list(heart_colors), # ✅ الحل: تحويل الألوان لقائمة
           shadow.col = "gray",
           nn = TRUE,
           tweak = 1.2)

# التوقع
dt_pred <- predict(dt_model, test_data, type = "class")
print(table(Actual = test_data$num, Predicted = dt_pred))


# =====================================================
# Model 2: Random Forest 🌲
cat("\n--- 2. Random Forest Results ---\n")

rf_model <- randomForest(num ~ ., data = train_data, ntree = 100, importance = TRUE)
# التوقع
rf_pred <- predict(rf_model, test_data)
print(table(Actual = test_data$num, Predicted = rf_pred))
# رسم أهمية المتغيرات
varImpPlot(rf_model, main = "Feature Importance", col = "#B22222")


# =====================================================
# Model 3: Logistic Regression 📈
# =====================================================
cat("\n--- 3. Logistic Regression Results ---\n")

train_data$binary_target <- ifelse(train_data$num == "0", 0, 1)
test_data$binary_target <- ifelse(test_data$num == "0", 0, 1)

log_model <- glm(binary_target ~ . -num, data = train_data, family = "binomial")

log_probs <- predict(log_model, test_data, type = "response")
log_pred_class <- ifelse(log_probs > 0.5, 1, 0)

print(table(Actual = test_data$binary_target, Predicted = log_pred_class))

# **************** Statistical Tests ****************

df$num_bin <- ifelse(df$num > 0, 1, 0)
df$num_bin <- factor(df$num_bin, labels = c("NoDisease", "Disease"))

t_test_age <- t.test(age ~ num_bin, data = df)
t_test_age

ggplot(df, aes(x = num_bin, y = age, fill = num_bin)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  labs(title="Age Distribution by Disease Status") +
  scale_fill_manual(values=heart_colors) +
  heart_theme


chisq_cp <- chisq.test(table(df$cp, df$num_bin))
chisq_cp

ggplot(df, aes(x = cp, fill = num_bin)) +
  geom_bar(position = "fill") +
  labs(title="Chest Pain Type vs Disease") +
  scale_fill_manual(values=heart_colors) +
  heart_theme


anova_thalach <- aov(thalch ~ num_bin, data = df)
summary(anova_thalach)

ggplot(df, aes(x = num_bin, y = thalch, fill = num_bin)) +
  geom_boxplot(alpha = 0.7) +
  labs(title="Maximum Heart Rate (Thalch) by Disease Status") +
  scale_fill_manual(values=heart_colors) +
  heart_theme


wilcox_age <- wilcox.test(age ~ num_bin, data = df)
wilcox_age

ggplot(df, aes(x = age, fill = num_bin)) +
  geom_density(alpha = 0.4) +
  labs(title="Age Distribution Density by Disease") +
  scale_fill_manual(values=heart_colors) +
  heart_theme




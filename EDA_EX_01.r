
print("NAME: K.Shobika")
print("ROLL NUMBER: 23BAD113")

library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("/content/1.student_performance.csv")

str(data)
colSums(is.na(data))

data <- na.omit(data)

data$Department <- as.factor(data$Department)
data$Subject <- as.factor(data$Subject)
data$Final_Grade <- as.factor(data$Final_Grade)

data$Internal_Test1 <- as.numeric(data$Internal_Test1)
data$Internal_Test2 <- as.numeric(data$Internal_Test2)
data$Assignment_Marks <- as.numeric(data$Assignment_Marks)
data$Attendance_Percentage <- as.numeric(data$Attendance_Percentage)

data <- data %>%
  mutate(Average_Internal = (Internal_Test1 + Internal_Test2) / 2)

head(data)
summary(data)

avg_marks <- data %>%
  group_by(Subject) %>%
  summarise(Average_Marks = mean(Average_Internal))

ggplot(avg_marks, aes(x = Subject, y = Average_Marks, fill = Subject)) +
  geom_bar(stat = "identity") +
  labs(title = "Subject-wise Average Internal Marks",
       x = "Subject",
       y = "Average Marks") +
  theme_minimal()

trend_data <- data %>%
  group_by(Subject) %>%
  summarise(
    Test1_Avg = mean(Internal_Test1),
    Test2_Avg = mean(Internal_Test2)
  ) %>%
  pivot_longer(
    cols = c(Test1_Avg, Test2_Avg),
    names_to = "Test",
    values_to = "Average_Marks"
  )

ggplot(trend_data, aes(x = Test, y = Average_Marks,
                        group = Subject, color = Subject)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Performance Trend Across Internal Tests",
       x = "Internal Test",
       y = "Average Marks") +
  theme_minimal()

grade_data <- data %>%
  group_by(Final_Grade) %>%
  summarise(Count = n())

ggplot(grade_data, aes(x = "", y = Count, fill = Final_Grade)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Final Grade Distribution") +
  theme_void()

#caret คือ ML software สำหรับเทรนโมเดล Classification and ด้วยภาษา R พัฒนาโดย Max Kuhn
#จริงๆแล้วใน R มีหลายๆ frameworks ให้เราเลือกใช้งาน เช่น caret, mlr, tidymodels ส่วนตัวแอดชอบ caret ที่สุดเลย เพราะเป็น template ใช้งานง่าย เทรนโมเดลได้หลายแบบ และมีฟีเจอร์สำคัญครบ เช่น การทำ cross validation, resampling, model evaluation เป็นต้น

#install.packages("caret")
library("caret")

#split data
mtcars


train_test_split <- function(data) {
set.seed(42)
n <- nrow(mtcars)
id <- sample(n,size = 0.8*n)
train_data <- mtcars[id,]
test_data <- mtcars[-id,]
return(list(train_data,test_data))
}

split_data <- train_test_split(mtcars)
split_data[[1]]
split_data[[2]]


#train model
lm_model <- train(mpg ~ hp ,data = split_data[[1]] ,
      method = "lm")
#score and evaluate
p <- predict(lm_model, newdata =  split_data[[2]])
p
error <- split_data[[2]]$mpg - p
rmse <- sqrt(mean(error**2))
rmse


#Classification Example

library(caret)

# load clean data
data("mtcars")
#ตัวอย่างนี้มาลองสร้าง classification model กันบ้าง โดยเราจะใช้คอลัมน์ mpg ทำนายคอลัมน์ am ก่อนที่เราจะเริ่มเทรนโมเดล ต้องเปลี่ยนคอลัมน์ am เป็น factor ก่อนนะครับ 0=Auto, 1=Manual
# prepare data เปลี่ยนcol ให้เป็นfactorก่อน
mtcars$am <- factor(mtcars$am, levels=c(0,1), labels=c("Auto", "Manual"))

# split data
split_data <- train_test_split(mtcars)

# train model
glm_model <- train(am ~ mpg, # classification 
                   data = split_data[[1]],
                   method = "glm")

predict <- predict(glm_model, newdata=split_data[[2]])

acc <- mean(p == split_data[[2]]$am)

glm_model
acc

#ถ้า train accuracy สูงกว่า test accuracy เยอะๆ เช่น 80% vs. 60% แปลว่าตอนนี้โมเดลของเรากำลังเจอปัญหา Overfitting เป็นปัญหาใหญ่ที่สุดของการเทรนโมเดล ML ในปัจจุบันเลย

#Overfitting คือการที่ algorithm ของเราเรียนรู้ training data ดีเกินไป จนไม่สามารถนำไปประยุกต์หรือทำนาย new data ได้ i.e. test accuracy จะมีค่าต่ำลงเยอะเลย (หรือ test error มีค่าสูงขึ้นมาก)




#ในวิดีโอ
library("caret")
#linear
model1 <- lm(mpg ~ hp+wt ,data = mtcars) 
model1


#caret train()
model2 <- train(mpg ~ hp+wt ,
      data = mtcars,
      method = "lm") #ML algorithm
model2$finalModel


#ML basic pipeline
# 1.prepare / split data
set.seed(42)
n <- nrow(mtcars)
id <- sample(1:n ,size = n*0.8)
train_data <- mtcars[id,]
test_data <- mtcars[-id,]

# 2.train model
model <- train(mpg ~ hp+wt ,
                data = train_data,
                method = "lm")
model
?trainControl
# 3.score/prediction
p_mpg <- predict(model, newdata=test_data)

# 4.evaluate model
error <- p_mpg - test_data$mpg
test_rmse <- sqrt(mean(error**2))
model
test_rmse
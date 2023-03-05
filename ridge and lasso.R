#logistic regression ; lasso,ridge
set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5, 
  verboseIter = TRUE #เก็บlogในการtrainแต่ละครั้ง
)

grid <- expand.grid(alpha = c(0,1),
                    lambfa=seq(0,1,by=0.03)
                    ) # 0 คือ Ridge 1 is lasso


glm_model <- train(diabetes ~ . ,
                   data = train_data,
                   method = "glmnet", #ลงLibraryเพิ่ม
                   metric = "Accuracy",
                   trControl = ctrl)


#if alpha = 0 is ridge regression , alpha = 1 is Lasso regression



#evaluate model
p <- predict(glmnet_model , newdata = test_data)

confusionMatrix(p,
                test_data$diabetes,
                positive = "pos",
                mode = "prec_recall")




#GLM net model:lasso,ridge[Lasso Regression]
#Train glmnet with custom trainControl and tuning:model
#predict diabeete
install.packages('glmnet')
set.seed(42)
ctrl <- trainControl(Method = "cv",
                     number = 5,
                     verboseIter = TRUE)
#จูนกริด
#alpha มีได้2ค่า ถ้าเกิก = 1 เป็นridge ถ้า=0จะเป็นlasso
grid <- expand.grid(alpha=(0,1),lambda = seq(0,1,by=0.03))


glmnet_model <- train(diabetes ~ .,
                      data = train_data,
                      method = "glm",
                      metric = "Accuracy",
                      trControl = ctrl,
                      turnGird = grid)

#evaluate model
p <- predict(glmnet_model,newdata = test_data)

confusionMatrix(p,
                test_data$diabetes,
                positive = "pos"
                mode = "prec_recall")


#elastic net
set.seed(42)
ctrl <- trainControl(Method = "cv",
                     number = 5,
                     verboseIter = TRUE)
#จูนกริด
#ต่างแค่alpha
grid <- expand.grid(alpha=(0.25,0.5,0.75),lambda = seq(0,1,by=0.01))


glmnet_model <- train(diabetes ~ .,
                      data = train_data,
                      method = "glm",
                      metric = "Accuracy",
                      trControl = ctrl,
                      turnGird = grid)

#evaluate model
p <- predict(glmnet_model,newdata = test_data)

confusionMatrix(p,
                test_data$diabetes,
                positive = "pos"
                mode = "prec_recall")





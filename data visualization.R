library(tidyverse)

#basics plot base R ใช้ค่าสถิติคู่กราฟ


#analyzing horse power
#hist - One quantitative variable
hist(mtcars$mpg)
mean(mtcars$mpg)
median(mtcars$mpg)


#เปลี่ยนตัวเลขเป็นอักษร คือเปลี่ยนเป็นfactor
str(mtcars)
mtcars$am <- factor(mtcars$am ,
                    levels = c(0,1),
                    labels = c("Auto","Manual"))
#bar plot - One quantitative variable
table(mtcars$am)  #สร้างตารางความถ่ี
barplot(table(mtcars$am))


#box plot ใช้หาoutlier
boxplot(mtcars$hp)
fivenum(mtcars$hp) #ตัวเลข5ตัว ได้แก่ Min,Q1,Q2,Q3,Max

min(mtcars$hp)
quantile(mtcars$hp , probs = c(.25,.5,.75))
max(mtcars$hp)


#whisker calculation หาoutlier
Q3 <-quantile(mtcars$hp , probs = .75)
Q1 <-quantile(mtcars$hp , probs = .25)
IQR <- Q3 - Q1 
IQR

Q3 + 1.5*IQR
Q1 - 1.5*IQR
#https://www.rdocumentation.org



#coef คือ ค่า1.5 ที่เราแทนในสูตร บางที่เลข3ก็มี
boxplot.stats(mtcars$hp ,coef = 1.5)
#$statsค่าstat 5ตัว
#$n จำนวนค่าสังเกต
#ค่า $conf ที่มาจาก2สูตรQ3 + 1.5*IQR,Q1 - 1.5*IQR
# $out คือตัว outlier


#filter outlier
mtcars_no_out <- mtcars %>%
                     filter(hp<335)

boxplot(mtcars_no_out$hp)

#box plot 2 variables
#quanlitative x quantitative ตัวเลขเเละตัวอัหษร
data("mtcars") # load original dataframe
mtcars$am  <- factor(mtcars$am,
                     levels = c(0,1),
                     labels = c("Auto","Manual"))
boxplot(mpg ~ am , data = mtcars ,col = c("gold","red"))

#scatter plot
#2 x quantitative

plot(mtcars$hp,mtcars$mpg,pch = 16 ,col = 'salmon',main= "Test" , xlab ='x' , ylab = "y")


#ggplot
#library(tidyverse)
#https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf

ggplot(data = mtcars ,mapping =aes(x = hp , y = mpg)) +
  geom_point() + 
  geom_smooth() +
  geom_rug() #ถ้าข้างๆขีดเยอะ เเสดงว่าdata point กระจุกตรงนี้ค่อนข้างเยอะ

#alpha ปรับความจาง
ggplot(data = mtcars ,mapping =aes(x = hp , y = mpg)) +
  geom_point(size=3 , col = "blue",alpha =0.2)


#ggplot onevariable
ggplot(mtcars ,aes(hp)) +
  geom_histogram(bins = 10 , fill = "red" ,alpha =0.2 ) 



ggplot(mtcars ,aes(hp)) +
  geom_boxplot() 

#saveเกบเปนตัวแแปรจะได้ไม่ต้องพิมเยอะ point คือ ระบุตัวเบสให้ได้
p <- ggplot(mtcars ,aes(hp)) 

P +
  geom_boxplot() 


#box plot by group 

diamonds %>%
  count(cut)

ggplot(diamonds ,aes(cut))+
  geom_bar(fill = "#0366fc")

#mapping คือ การดึงคอลัมภ์ที่อยุ่ในdfของเราไปเมพที่aes คืิอความสวยงามของกราฟของเรา
ggplot(diamonds ,mapping = aes(cut , fill  = cut))+
  geom_bar()

ggplot(diamonds ,aes(cut , fill  = color))+
  geom_bar()

ggplot(diamonds ,aes(cut , fill  = color))+
  geom_bar(position = "dodge")


ggplot(diamonds ,aes(cut , fill  = color))+
  geom_bar(position = "fill")

#สุ่มตัวอย่าง
set.seed(100)
small <- sample_n(diamonds , 5000)

ggplot(small,aes(carat , price ))+
         geom_point()
       
# facet : small multiple
set.seed(100)
small <- sample_n(diamonds , 5000)

ggplot(small,aes(carat , price ))+
  geom_point()+
  facet_wrap(~ color , ncol = 4)




ggplot(small,aes(carat , price ))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ color , ncol = 2)



ggplot(small,aes(carat , price ))+
  geom_point()+
  geom_smooth(method = "lm" , col = "red")+
  facet_wrap(~ color , ncol = 2)


#เปลี่ยนลบสีพื้นหลัง

ggplot(small,aes(carat , price ))+
  geom_point()+
  geom_smooth(method = "lm" , col = "red")+
  facet_wrap(~ color , ncol = 2)+
  theme_classic()


ggplot(small,aes(carat , price ))+
  geom_point()+
  geom_smooth(method = "lm" , col = "red")+
  facet_wrap(~ color , ncol = 2)+
  theme_minimal()

ggplot(small,aes(carat , price ))+
  geom_point()+
  geom_smooth(method = "lm" , col = "red")+
  facet_wrap(~ color , ncol = 2)+
  theme_minimal()+
  labs(title =  "test",
       x = "carat",
       y = "price",
       caption = "source:data diamond from ggplot2 package")


# final example
ggplot(small,aes(carat , price , col = cut ))+
  geom_point(size = 3 , alpha = 0.3)+
  facet_wrap(~ color , ncol = 2)+
  theme_minimal()



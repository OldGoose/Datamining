require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret)     # for train(), tune parameters
require(datasets)
require(Metrics)
library(readxl)
Data=DM_Assignment_4_NN
t1<-Sys.time()
# 確認數據架構
str(Data)

# 剔除離群值--------------------------------------------------------------------------------------
scale_data <- scale(Data, center = TRUE, scale = TRUE) # 標準化表單中的數值
scale_data <- as.data.frame(scale_data) # 轉成Data frame型態
out_up<-which(scale_data> 2, arr.ind = TRUE) # 找2個標準差外面的數據
row_up<-out_up[,1]
out_down<-which(scale_data< -2, arr.ind = TRUE) # 找出Z值小於-2的資料
row_down<-out_up[,1]
row_out <-rbind(row_up,row_down) # 把大於2於-2的列合併在一起
Data_outlier <-Data[-row_out,] # 刪除z值>2 & <-2的值
# 再次標準化剔除離群值後之Data--------------------------------------------------------------------
Data_outlier1 <- subset(Data_outlier,select = -c(1))
scale_data1 <- scale(Data_outlier1, center = TRUE, scale = TRUE) # 標準化表單中的數值
scale_data2 <- as.data.frame(scale_data1) # 轉成Data frame型態
No. <- subset(Data_outlier,select = c(1))
final_Data <- cbind(No.,scale_data2) # 合併No. 及標準化數據
# 將NA值與資料本體分開----------------------------------------------------------------------------
nacolumn<-which(is.na(final_Data), arr.ind = TRUE) # mark NA#
Data1<-final_Data[-nacolumn,] # 砍Class中含有NA的列#

# 建立Test & Train --------------------------------------------------------------------------------
set.seed(-55688) # 固定取樣
train.index <- sample(x=1:nrow(Data1), size=ceiling(0.8*nrow(Data1) ))

train = Data1[train.index, ]
test = Data1[-train.index, ]

# 建構formula--------------------------------------------------------------------------------------
formula.bpn <- y1 + y2 ~ x1 + x2 + x3 + x4
# prototype確認參數是否收斂
bpn <- neuralnet(formula = formula.bpn, 
                 data = train,
                 hidden = c(2),       # 一個隱藏層：2個node
                 learningrate = 0.01, # learning rate 影響演算法的收斂速度 越小越久，越大w就會發生震盪(可能永遠無法收斂)
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 500000        # 最大的ieration數
                 
)
# bpn模型輸出
plot(bpn)

# tune parameters----------------------------------------------------------------------------------
model <- train(form=formula.bpn,     # formula
               data=train,           # 資料
               method="neuralnet",   # 類神經網路(bpn)
               
               # 最重要的步驟：觀察不同排列組合(第一層1~4個nodes ; 第二層0~4個nodes)
               # 看何種排列組合(多少隱藏層、每層多少個node)，會有最小的RMSE
               tuneGrid = expand.grid(.layer1=c(1:8), .layer2=c(0:4), .layer3=c(0)),               
               
               # 以下的參數設定，和上面的neuralnet內一樣
               learningrate = 0.01, # learning rate 影響演算法的收斂速度 越小越久，越大w就會發生震盪(可能永遠無法收斂)
               threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
               stepmax = 50000        # 最大的ieration數 = 50000
)
model
plot(model) # 輸出Model 結果建議第一層使用4個node，

# node(4)找出MSE最小模型----------------------------------------------------------------------------------
{
  node4 <- neuralnet(formula = formula.bpn, 
                     data = train,
                     hidden = c(4),     # 第一隱藏層4個node
                     learningrate = 0.01, # learning rate
                     threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                     stepmax = 5000000        # 最大的ieration數 
  )
  #預測----------------------------------------------------------------------------------------------
  pred <- compute(node4, test[, 2:5])  
  
  # 預測結果-----------------------------------------------------------------------------------------
  pred$net.result
  pred.result <- as.data.frame(pred$net.result)
  
  #計算Test Data的MSE--------------------------------------------------------------------------------
  a<- mse(test$y1, pred.result$V1)
  b<- mse(test$y2, pred.result$V2)
  print(a) # 0.0003238535875
  print(b) # 0.0004044849801
}
plot(node4) # 輸出圖形 use node4

# 實際值估計----------------------------------------------------------------------------------------
Question <- final_Data[!complete.cases(final_Data), ]   # source from 已標準化的data 並取出含有NA的欄位      
# 預測
{
pred_Q <- compute(node4, Question[, 2:5])  

# 預測結果------------------------------------------------------------------------------------------
pred_Q$net.result
pred_Q.result <- as.data.frame(pred_Q$net.result)

# 題目y1 & y2 輸出----------------------------------------------------------------------------------
Anti_Data <- Data_outlier[complete.cases(Data_outlier), ] # source from rawdata 去除NA 
#y1
y1_M<- mean(Anti_Data$y1) # y1平均值
y1_S<- sd(Anti_Data$y1)  # y1標準差
#y2
y2_M<- mean(Anti_Data$y2) # y2平均值
y2_S<- sd(Anti_Data$y2)  # y2標準差
# 反標準化
y1_output <- (pred_Q.result$V1 * y1_S) + y1_M
y1_output <- as.data.frame(y1_output)
y2_output <- (pred_Q.result$V2 * y2_S) + y2_M
y2_output <- as.data.frame(y2_output)
answer_out <-cbind(y1_output,y2_output) 
# 輸出答案
print(answer_out)
}
# 轉成DataFrame
Database <- Data[!complete.cases(Data), ]
Question1 <- subset(Database,select = -c(6,7))
answer_out1 <-cbind(Question1,answer_out) 

t2<-Sys.time()
t1-t2

require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret)     # for train(), tune parameters
require(datasets)
require(Metrics)
library(readxl)
Data=DM_Assignment_4_NN
t1<-Sys.time()
# �T�{�ƾڬ[�c
str(Data)

# �簣���s��--------------------------------------------------------------------------------------
scale_data <- scale(Data, center = TRUE, scale = TRUE) # �зǤƪ��椤���ƭ�
scale_data <- as.data.frame(scale_data) # �নData frame���A
out_up<-which(scale_data> 2, arr.ind = TRUE) # ��2�ӼзǮt�~�����ƾ�
row_up<-out_up[,1]
out_down<-which(scale_data< -2, arr.ind = TRUE) # ��XZ�Ȥp��-2�����
row_down<-out_up[,1]
row_out <-rbind(row_up,row_down) # ��j��2��-2���C�X�֦b�@�_
Data_outlier <-Data[-row_out,] # �R��z��>2 & <-2����
# �A���зǤƭ簣���s�ȫᤧData--------------------------------------------------------------------
Data_outlier1 <- subset(Data_outlier,select = -c(1))
scale_data1 <- scale(Data_outlier1, center = TRUE, scale = TRUE) # �зǤƪ��椤���ƭ�
scale_data2 <- as.data.frame(scale_data1) # �নData frame���A
No. <- subset(Data_outlier,select = c(1))
final_Data <- cbind(No.,scale_data2) # �X��No. �μзǤƼƾ�
# �NNA�ȻP��ƥ�����}----------------------------------------------------------------------------
nacolumn<-which(is.na(final_Data), arr.ind = TRUE) # mark NA#
Data1<-final_Data[-nacolumn,] # ��Class���t��NA���C#

# �إ�Test & Train --------------------------------------------------------------------------------
set.seed(-55688) # �T�w����
train.index <- sample(x=1:nrow(Data1), size=ceiling(0.8*nrow(Data1) ))

train = Data1[train.index, ]
test = Data1[-train.index, ]

# �غcformula--------------------------------------------------------------------------------------
formula.bpn <- y1 + y2 ~ x1 + x2 + x3 + x4
# prototype�T�{�ѼƬO�_����
bpn <- neuralnet(formula = formula.bpn, 
                 data = train,
                 hidden = c(2),       # �@�����üh�G2��node
                 learningrate = 0.01, # learning rate �v�T�t��k�����ĳt�� �V�p�V�[�A�V�jw�N�|�o�;_��(�i��û��L�k����)
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 500000        # �̤j��ieration��
                 
)
# bpn�ҫ���X
plot(bpn)

# tune parameters----------------------------------------------------------------------------------
model <- train(form=formula.bpn,     # formula
               data=train,           # ���
               method="neuralnet",   # �����g����(bpn)
               
               # �̭��n���B�J�G�[��P�ƦC�զX(�Ĥ@�h1~4��nodes ; �ĤG�h0~4��nodes)
               # �ݦ�رƦC�զX(�h�����üh�B�C�h�h�֭�node)�A�|���̤p��RMSE
               tuneGrid = expand.grid(.layer1=c(1:8), .layer2=c(0:4), .layer3=c(0)),               
               
               # �H�U���ѼƳ]�w�A�M�W����neuralnet���@��
               learningrate = 0.01, # learning rate �v�T�t��k�����ĳt�� �V�p�V�[�A�V�jw�N�|�o�;_��(�i��û��L�k����)
               threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
               stepmax = 50000        # �̤j��ieration�� = 50000
)
model
plot(model) # ��XModel ���G��ĳ�Ĥ@�h�ϥ�4��node�A

# node(4)��XMSE�̤p�ҫ�----------------------------------------------------------------------------------
{
  node4 <- neuralnet(formula = formula.bpn, 
                     data = train,
                     hidden = c(4),     # �Ĥ@���üh4��node
                     learningrate = 0.01, # learning rate
                     threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                     stepmax = 5000000        # �̤j��ieration�� 
  )
  #�w��----------------------------------------------------------------------------------------------
  pred <- compute(node4, test[, 2:5])  
  
  # �w�����G-----------------------------------------------------------------------------------------
  pred$net.result
  pred.result <- as.data.frame(pred$net.result)
  
  #�p��Test Data��MSE--------------------------------------------------------------------------------
  a<- mse(test$y1, pred.result$V1)
  b<- mse(test$y2, pred.result$V2)
  print(a) # 0.0003238535875
  print(b) # 0.0004044849801
}
plot(node4) # ��X�ϧ� use node4

# ��ڭȦ��p----------------------------------------------------------------------------------------
Question <- final_Data[!complete.cases(final_Data), ]   # source from �w�зǤƪ�data �è��X�t��NA�����      
# �w��
{
pred_Q <- compute(node4, Question[, 2:5])  

# �w�����G------------------------------------------------------------------------------------------
pred_Q$net.result
pred_Q.result <- as.data.frame(pred_Q$net.result)

# �D��y1 & y2 ��X----------------------------------------------------------------------------------
Anti_Data <- Data_outlier[complete.cases(Data_outlier), ] # source from rawdata �h��NA 
#y1
y1_M<- mean(Anti_Data$y1) # y1������
y1_S<- sd(Anti_Data$y1)  # y1�зǮt
#y2
y2_M<- mean(Anti_Data$y2) # y2������
y2_S<- sd(Anti_Data$y2)  # y2�зǮt
# �ϼзǤ�
y1_output <- (pred_Q.result$V1 * y1_S) + y1_M
y1_output <- as.data.frame(y1_output)
y2_output <- (pred_Q.result$V2 * y2_S) + y2_M
y2_output <- as.data.frame(y2_output)
answer_out <-cbind(y1_output,y2_output) 
# ��X����
print(answer_out)
}
# �নDataFrame
Database <- Data[!complete.cases(Data), ]
Question1 <- subset(Database,select = -c(6,7))
answer_out1 <-cbind(Question1,answer_out) 

t2<-Sys.time()
t1-t2
#Install packages
# install.packages("ggpubr")
# install.packages("GGally")
# install.packages("rpart")
# install.packages("caret")
# install.packages("forcats")
# install.packages("rattle")
# install.packages("rpart.plot")
# install.packages("RColorBrewer")
# install.packages("pROC")
# install.packages("dplyr")
# install.packages("Rmisc")
# install.packages("FactoMineR")
# install.packages("factoextra")
# install.packages("e1071")

#Load packages
  library(ggpubr)
  library(GGally) #pairs

  library(rpart) #DecisionTree
  library(ggplot2)
  library(forcats) #ordering bars in barplot 
  library(caret) #featureplot

  library(rattle) #DecisionTree representation
  library(rpart.plot)
  library(RColorBrewer)

  library(pROC)
  
  library(dplyr)
  
  library(FactoMineR)
  library(factoextra)
  
  library(e1071)

#Load the methods file
source("C:/Users/Stefano/Desktop/MLProject/ML-Project/AndroniStefano845811/methods.R")

#Colors-------------------------------------------------------------------------
p.col="#003f5c"
s.col="#bc5090"
axis.text.col="#7E7E7B"
axis.title.col="#474747"
graph.borde.col="#DADAD5"
#-------------------------------------------------------------------------------

#[DATA ACQUISATION]___________________________________________________________
path = "C:/Users/Stefano/Desktop/MLProject/ML-Project/AndroniStefano845811/dataset.csv"
df = read.csv(path, header=TRUE, row.names = NULL, stringsAsFactors=TRUE) 

#First view
str(df)
head(df)
dim(df) #size of df
sapply(df, class) #type of the attribute


#[PREPROCESSING/DATA TRANSFROMATION]____________________________________________

#>New column name (Employment.Type --> GovernmentEmployment )
colnames(df)[3] = "GovernmentEmployment"
#>New column name (GraduateOrNot --> Graduate)
colnames(df)[4] = "Graduate"

#>Graduate values: From {"Yes","No"} to {0,1}
levels(df$Graduate) = c(levels(df$Graduate), 1, 0) 
df$Graduate[df$Graduate=="Yes"] = 1
df$Graduate[df$Graduate=="No"] = 0
df$Graduate = factor(df$Graduate) #remove unused levels 

#>FrequentFlyer values: From {"Yes","No"} to {0,1}
levels(df$FrequentFlyer) = c(levels(df$FrequentFlyer), 1, 0) 
df$FrequentFlyer[df$FrequentFlyer=="Yes"] = 1
df$FrequentFlyer[df$FrequentFlyer=="No"] = 0
df$FrequentFlyer = factor(df$FrequentFlyer) #remove unused levels 

#>EverTravelledAbroad values: From {"Yes","No"} to {0,1}
levels(df$EverTravelledAbroad) = c(levels(df$EverTravelledAbroad), 1, 0) 
df$EverTravelledAbroad[df$EverTravelledAbroad=="Yes"] = 1
df$EverTravelledAbroad[df$EverTravelledAbroad=="No"] = 0
df$EverTravelledAbroad = factor(df$EverTravelledAbroad) #remove unused levels 

#>GovernmentEmployment values: From {"Private Sector/Self Employed", "Government Sector"}	to {0,1}
levels(df$GovernmentEmployment) = c(levels(df$GovernmentEmployment), 1, 0) 
df$GovernmentEmployment[df$GovernmentEmployment=="Government Sector"] = 1
df$GovernmentEmployment[df$GovernmentEmployment=="Private Sector/Self Employed"] = 0
df$GovernmentEmployment = factor #remove unused levels 

#>From int to factor
df$ChronicDiseases = factor(df$ChronicDiseases)
#>From int to factor
df$TravelInsurance = factor(df$TravelInsurance)

sapply(df, class) #check type of the attribute

#>First column removal (id-col --> irrelevant)
df = df[,(2:10)]

#[SPLIT THE DATA]_______________________________________________________________
#allset = split.data(df)
#df.train = allset$train #trainingset
#df.test = allset$tes #testset

#[EXPLORATORY ANALYSIS]_________________________________________________________
df.input = df[,1:8]#input attributes (input space)
df.target = df[,9] #target variable (output space) 

#A-Univariate*******************************************************************
p.col="#2596be"
s.col="#F1CD3D"

#>Distribution of the target variable
  table.sub = table(df$TravelInsurance )
  prop.table.sub = round(prop.table(table.sub), digits = 3)*100
  #<<Graf.A12>>
  ggplot(df, aes(x="",  fill=TravelInsurance))+
    geom_bar(width = 1, stat = "count")+ 
    coord_polar("y", start=0) + 
    scale_fill_manual(values=c(p.col, s.col), labels=c("No","Yes"))+ 
    #blank_theme +
    theme(axis.text.x=element_blank())+
    labs(y="", x="", title="")
  #Comment: Balanced

#>Age
  #<<Graf.A1>>
  ggplot(df, aes(Age)) +
    geom_density(stat = "count", fill=p.col, color=graph.borde.col, alpha=1)+ 
    labs(y="No. of pearsons")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col))
  #<<Graf.A2>>
  ggplot(df, aes(x=Age)) + 
    geom_boxplot(fill=p.col, color=s.col, lwd=0.7)+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col))

  mean(df$Age)
  median(df$Age)
  moda(df$Age)
  var(df$Age)
  sd(df$Age)
  min(df$Age)
  max(df$Age)

#>GovernmentEmployment
  table = table(df$GovernmentEmployment)
  prop.table = round(prop.table(table), digits = 3)*100
  #<<Graf.A3>>
  ggplot(df, aes(x=fct_infreq(GovernmentEmployment), fill=GovernmentEmployment))+
    geom_bar(stat="count", width=0.7, show.legend = FALSE)+
    labs(y="No. of pearsons", x="GovernmentEmployment")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col)) +
    scale_fill_manual(values=c(p.col, s.col))+ 
    geom_text(aes(label=paste0((format(round((after_stat(prop*100) ), 2), nsmall = 2)),"%"), group=1),
              stat='count', colour = 'white',  size=8, vjust = +1.5, fontface="bold")
 
#>Graduate
  table = table(df$Graduate)
  prop.table = round(prop.table(table), digits = 3)*100
  #<<Graf.A4>>
  ggplot(df, aes(x=fct_infreq(Graduate), fill=Graduate))+
    geom_bar(stat="count", width=0.7, show.legend = FALSE)+
    labs(y="No. of pearsons", x="Graduate")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col)) +
    scale_fill_manual(values=c(s.col, p.col))+ 
    geom_text(aes(label=paste0((format(round((after_stat(prop*100) ), 2), nsmall = 2)),"%"), group=1),
              stat='count', colour = 'white',  size=8, vjust = +1.5, fontface="bold")

#>AnnualIncome
  #<<Graf.A8>>
  ggplot(df, aes(AnnualIncome)) +
    geom_density(stat = "count", fill=p.col, color=graph.borde.col, alpha=1)+ 
    labs(y="No. of pearsons")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col))
  #<<Graf.A9>>
  ggplot(df, aes(x=AnnualIncome)) + 
    geom_boxplot(fill=p.col, color=s.col, lwd=1)+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col))

  mean(df$AnnualIncome)
  median(df$AnnualIncome)
  var(df$AnnualIncome)
  sd(df$AnnualIncome)
  min(df$AnnualIncome)
  max(df$AnnualIncome)

#>FamilyMembers
  table = table(df$FamilyMembers)
  prop.table = round(prop.table(table), digits = 3)*100
  #<<Graf.A10>>
  ggplot(df, aes(x=as.factor(FamilyMembers)))+
    geom_bar(stat="count", width=0.8, show.legend = FALSE, fill=create.alterning.colors.list(p.col,s.col,8))+
    labs(y="No. of pearsons", x="FamilyMembers")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col)) +
    scale_fill_manual(values=c(p.col, s.col))+ 
    geom_text(aes(label=paste0((format(round((after_stat(prop*100) ), 2), nsmall = 2)),"%"), group=1),
              stat='count', colour = 'white',  size=4, vjust = +1.5,fontface = "bold")
  #<<Graf.A11>>
  ggplot(df, aes(x=FamilyMembers)) + 
    geom_boxplot(fill=p.col, color=s.col)+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col))
  
  mean(df$FamilyMembers)
  median(df$FamilyMembers)
  var(df$FamilyMembers)
  sd(df$FamilyMembers)
  min(df$FamilyMembers)
  max(df$FamilyMembers)

#>ChronicDiseases
  table = table(df$ChronicDiseases)
  prop.table = round(prop.table(table), digits = 3)*100
  #<<Graf.A5>>
  ggplot(df, aes(x=fct_infreq(ChronicDiseases), fill=ChronicDiseases))+
    geom_bar(stat="count", width=0.7, show.legend = FALSE)+
    labs(y="No. of pearsons", x="ChronicDiseases")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col)) +
    scale_fill_manual(values=c(s.col, p.col))+ 
    geom_text(aes(label=paste0((format(round((after_stat(prop*100) ), 2), nsmall = 2)),"%"), group=1),
              stat='count', colour = 'white',  size=8, vjust = +1.5, fontface="bold")

#>FrequentFlyer
  table = table(df$FrequentFlyer)
  prop.table = round(prop.table(table), digits = 3)*100
  #<<Graf.A6>>
  ggplot(df, aes(x=fct_infreq(FrequentFlyer), fill=FrequentFlyer))+
  geom_bar(stat="count", width=0.7, show.legend = FALSE)+
    labs(y="No. of pearsons", x="FrequentFlyer")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col)) +
    scale_fill_manual(values=c(p.col, s.col))+ 
    geom_text(aes(label=paste0((format(round((after_stat(prop*100) ), 2), nsmall = 2)),"%"), group=1),
              stat='count', colour = 'white',  size=8, vjust = +1.5, fontface="bold")

#>EverTravelledAbroad
  table = table(df$FrequentFlyer)
  prop.table = round(prop.table(table), digits = 3)*100
  #<<Graf.A7>>
  ggplot(df, aes(x=fct_infreq(EverTravelledAbroad), fill=EverTravelledAbroad))+
    geom_bar(stat="count", width=0.7, show.legend = FALSE)+
    labs(y="No. of pearsons", x="EverTravelledAbroad")+
    theme(axis.text=element_text(size=12, colour=axis.text.col),
          axis.title=element_text(size=16, colour=axis.title.col)) +
    scale_fill_manual(values=c(p.col, s.col))+ 
    geom_text(aes(label=paste0((format(round((after_stat(prop*100) ), 2), nsmall = 2)),"%"), group=1),
              stat='count', colour = 'white',  size=8, vjust = +1.5, fontface="bold")

#B-Multivariate*****************************************************************
  
p.col="#003f5c"
s.col="#bc5090"

#>TravelInsurance X Age
  #<<Graf.B1>>
  ggplot(df, aes(Age, fill = TravelInsurance)) +  
    geom_density( alpha = 0.6)+ 
   # ggtitle("Age distribution") + 
    labs(y="No. of pearsons")+
    scale_fill_manual(values=c(p.col, s.col))#sovrapposte
  #<<Graf.B2>>
  ggplot(df, aes(x=TravelInsurance, y=Age, fill=TravelInsurance)) + 
    scale_fill_manual(values = c(p.col, s.col))+
    geom_boxplot(width=0.9, color=graph.borde.col,show.legend = FALSE) + theme_minimal()
  
#>Scatterplot variabili numeriche 
  df.numeric = df[, c("Age", "AnnualIncome", "FamilyMembers")]
  #>#<<Graf.B16>>
  featurePlot(x=df.numeric, y=df$TravelInsurance, jitter = TRUE, plot="pairs", 
              scales=list(x=list(relation="free"),  y=list(relation="free")), 
              col = c(p.col, s.col), auto.key=list(columns=2))
  featurePlot(x=df.numeric, y=df$TravelInsurance, jitter = TRUE, plot="box", 
              scales=list(x=list(relation="free"),  y=list(relation="free")), 
              col = c(p.col, s.col), auto.key=list(columns=2))

#>TravelInsurance X GovernmentEmployment
  #<<Graf.B3>>
  ggplot(data=df, aes(GovernmentEmployment))+
    geom_bar(aes(fill=as.factor(TravelInsurance)), position="fill", width=0.5,) +
    scale_fill_manual(values=c(p.col, s.col)) +
    labs(y="% of pearsons", fill="TravelInsurance")
  #<<Graf.B4>>
  ggplot(df,  aes(x=GovernmentEmployment, fill=TravelInsurance)) +
    geom_bar(aes(y = (..count..)/sum(..count..)) ,position = position_dodge(0.9)) + 
    labs(y="No. of pearsons") +
    scale_fill_manual(values=c(p.col, s.col))

#>TravelInsurance X Graduate
  #<<Graf.B5>>
  ggplot(data=df, aes(Graduate))+
    geom_bar(aes(fill=as.factor(TravelInsurance)), position="fill", width=0.5) +
    scale_fill_manual(values=c(p.col, s.col)) +
    labs(y="% of pearsons", fill="TravelInsurance")
  #<<Graf.B6>>
  ggplot(df,  aes(x=Graduate, fill=TravelInsurance)) +
    geom_bar(aes(y = (..count..)/sum(..count..)) ,position = position_dodge(0.9)) + 
    labs(y="No. of pearsons") +
    scale_fill_manual(values=c(p.col, s.col))

#>TravelInsurance X AnnualIncome
  #<<Graf.B7>>
  ggplot(df, aes(AnnualIncome, fill = TravelInsurance)) +  
    geom_density( alpha = 0.6)+ 
    labs(y="No. of pearsons")+
    scale_fill_manual(values=c(p.col, s.col))#sovrapposte
  
  #<<Graf.B17>>
  ggplot(df, aes(x=TravelInsurance, y=AnnualIncome, fill=TravelInsurance)) + 
    scale_fill_manual(values = c(p.col, s.col))+
    geom_boxplot(width=0.9, color=graph.borde.col,show.legend = FALSE) 

#featurePlot(df$AnnualIncome, df.target, plot="density", scales=list(x=list(relation="free"), 
#            y=list(relation="free")), auto.key=list(columns=2))

#>TravelInsurance X FamilyMembers
  ggplot(data=df, aes(FamilyMembers))+
    geom_bar(aes(fill=as.factor(TravelInsurance)), position="fill") +
    scale_fill_manual(values=c(p.col, s.col)) +
    labs(y="% of pearsons", fill="Subscribed")
  #<<Graf.B8>>
  ggplot(df,  aes(x=factor(FamilyMembers), fill=TravelInsurance)) +
    geom_bar(aes(y = (..count..)/sum(..count..)) ,position = position_dodge(0.9)) + 
    labs(y="No. of pearsons") +
    scale_fill_manual(values=c(p.col, s.col))  
  #<<Graf.B9>>
  ggplot(df, aes(FamilyMembers, fill = TravelInsurance)) +  
    geom_density( alpha = 0.6)+ 
    #ggtitle("FamilyMembers distribution") + 
    labs(y="% of pearsons")+
    scale_fill_manual(values=c(p.col, s.col))#sovrapposte

#>TravelInsurance X ChronicDiseases
  #<<Graf.B10>>
  ggplot(data=df, aes(ChronicDiseases))+
    geom_bar(aes(fill=as.factor(TravelInsurance)), position="fill", width=0.5) +
    scale_fill_manual(values=c(p.col, s.col)) +
    labs(y="% of pearsons", fill="TravelInsurance")
  #<<Graf.B11>>
  ggplot(df,  aes(x=ChronicDiseases, fill=TravelInsurance)) +
    geom_bar(aes(y = (..count..)/sum(..count..)) ,position = position_dodge(0.9)) + 
    labs(y="No. of pearsons") +
    scale_fill_manual(values=c(p.col, s.col))

#TravelInsurance X FrequentFlyer
  #<<Graf.B12>>
  ggplot(data=df, aes(FrequentFlyer))+
    geom_bar(aes(fill=as.factor(TravelInsurance)), position="fill", width=0.5) +
    scale_fill_manual(values=c(p.col, s.col)) +
    labs(y="% of pearsons", fill="TravelInsurance")
  #<<Graf.B13>>
  ggplot(df,  aes(x=FrequentFlyer, fill=TravelInsurance)) +
    geom_bar(aes(y = (..count..)/sum(..count..)) ,position = position_dodge(0.9)) + 
    labs(y="No. of pearsons") +
    scale_fill_manual(values=c(p.col, s.col))

#TravelInsurance X EverTravelledAbroad
  #<<Graf.B14>>
  ggplot(data=df, aes(EverTravelledAbroad))+
    geom_bar(aes(fill=as.factor(TravelInsurance)), position="fill", width=0.5) +
    scale_fill_manual(values=c(p.col, s.col)) +
    labs(y="% of pearsons", fill="TravelInsurance")
  #<<Graf.B15>>
  ggplot(df,  aes(x=EverTravelledAbroad, fill=TravelInsurance)) +
    geom_bar(aes(y = (..count..)/sum(..count..)) ,position = position_dodge(0.9)) + 
    labs(y="No. of pearsons") +
    scale_fill_manual(values=c(p.col, s.col))
  
#C-FAMD***************************************************************************
  p.col = "#5BF8CA"
  s.col = "#7140FC"
  df.active = df[,1:9]
  df.active = toBinary(df.active, TRUE)
  
  res.famd  =  FAMD(df.active, graph=FALSE, ncp=ncol(df.active)-1, sup.var = 9)
  
  #FAMD VARIABLES--------------------------------------------------------------
  #[1] Explained variances for dimensions
    eig.val = get_eigenvalue(res.famd)
    #<<Graf.C1>>
    fviz_screeplot(res.famd, addlabels=TRUE, barcolor=s.col, barfill=p.col,
                   linecolor="red", ylim=c(0,26))
  
  #[2]Variance contributions of variables for dimensions
    var = get_famd_var(res.famd) # this function returns a list containing the coordinates, the cos2 and the contribution of all variables:
    var$coord # Coordinates of variables
    var$cos2 # Cos2: quality of representation on the factor map
    var$contrib# Contributions % of varaivles to the  dimensions
   
    # Plot of variables
    fviz_famd_var(res.famd, repel = TRUE) #Contributo delle varaibili rispetto alle prime 2 componenti
    #<<Graf.C2>>     # Contribution to the first dimension
    fviz_contrib(res.famd, "var", axes = 1, color=s.col, fill=p.col) 
    #<<Graf.C3>>      # Contribution to the second dimension 
    fviz_contrib(res.famd, "var", axes = 2, color=s.col, fill=p.col)

  #Quantitative variables
    quanti.var = get_famd_var(res.famd, "quanti.var")
    #<<Graf.C4>>     
    fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",  #filled by cos2 (quality of rapresentation) 
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE)
  #Qualitative variables --> Vedere MCA su internetttttt
    quali.var = get_famd_var(res.famd, "quali.var")
    #<<Graf.C5>>     
    fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
    
  #FAMD INDIVIDUALS-------------------------------------------------------------
    ind = get_famd_ind(res.famd)
    #<<Graf.C6>>    
    fviz_famd_ind(res.famd, col.ind = "cos2", 
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE, invisible = "quali.var")
    #<<Graf.C7>>    
    fviz_mfa_ind(res.famd, 
                 habillage = "TravelInsurance", # color by groups 
                 palette = c("#00AFBB", "#FC4E07"),
                 alpha.ind = 0.5,
                 addEllipses = TRUE,
                 repel = TRUE, # Avoid text overlapping
                 geom="point"
    ) 
  
#[DECISION TREE]________________________________________________________________

#Model1,Model2 --> Trivial/Baseline
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>MODELLO 1: 0R<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
df$Prediction = rep(0, nrow(df))
df$Prediction = factor(df$Prediction)

confusion.matrix = table(df$TravelInsurance, df$Prediction)
accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix) #diag-->TP e TN
#0,64 -> Not so bad, cosidering the dummy prediction!

#>>>>>>>>>>>>>>>>>>>>>>>>MODEL 2; BETTER BASELINE<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
prop.table(table(df$EverTravelledAbroad, df$TravelInsurance),1)

df$Prediction = 0
df$Prediction[df$EverTravelledAbroad == 1] = 1
df$Prediction = factor(df$Prediction)

confusion.matrix = table(df$TravelInsurance, df$Prediction)
accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix) #diag-->TP e TN
#0,75 -> Not so bad considering the "dummy" prediction!

df = df[,1:9]

#>>>>>>>>>>>>>>>>>>>>>>MODELLO 3: DECISION TREE<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>Preparazione del dataset (solo per migliore visualizzazione dell'albero)
  #TravelInsurance values: From {"0","1"} to {"No", "Yes"}
    levels(df$TravelInsurance) = c(levels(df$TravelInsurance), "Yes", "No") 
    df$TravelInsurance[df$TravelInsurance==1] = "Yes"
    df$TravelInsurance[df$TravelInsurance==0] = "No"
    #remove unused levels 
    df$TravelInsurance = factor(df$TravelInsurance)
  
  #Modello GINI   
      rpart.model = rpart(TravelInsurance ~ ., data=df, method="class")

      #<<Graf.A1>>    
      fancyRpartPlot(rpart.model, main="", sub="", palettes="BuGn", type=2, yesno=1)
    
      summary(rpart.model)
      #Variable Importance (from above command)
        name=c("AnnualIncome","EverTravelledAbroad", "FamilyMembers","Age")
        value=c(49,28,14,9)
        data <- data.frame(
          name,  
          value
        )
        #<<Graf.A2>>  
        ggplot(data,  aes(x=value,y=name,fill=value)) + 
          geom_bar(stat="identity", width = 0.5) +
          labs(y="", x="") +
          scale_fill_gradient(low = "#FF4D26",  high = "#61E341")+
          theme(text = element_text(size=18)) 

    #But are you sure that this is the optimal 'Decision Tree' for this data? Avoid overfitting
      printcp(rpart.model)
      #<<Graf.A3>>  
      plotcp(rpart.model,  minline = TRUE, lty = 5, col = s.col) #0.025
      #Nessun taglio è già il minimo

  #Performance Evaluation    
  #Repeated k-CROSS-VALIDATION--------------------------------------------------
        set.seed(12)
        k = 10
        dataset = df
        repeats = 3
        
        count=1
        vector=vector(mode = "list", length = k*repeats)
        auc.list = list()

        for (j in 1:repeats){
        dataset$id = 1:dim(dataset)[1]
        folds = createFolds(dataset$id, k = k, list = TRUE, returnTrain = FALSE)
        x=0
          for(i in folds){
            #Split
              x=x+1
              testset = dataset[dataset$id %in% i, ]
              trainset = dataset[!dataset$id %in% i,]
              #dim(train.set)
              #dim(test.set)
            #Training
              model = rpart(TravelInsurance ~ ., data = trainset, method = "class")
            #Inference
              pred = predict(model, testset, type="class", probability=TRUE)
            #Save partial confusion matrix
              vector[[count]] <- table(pred, testset$TravelInsurance)
            #AUC (only positive)  
              pred = predict(model, testset, type="prob")
              rpart.ROC = roc(response = testset$TravelInsurance, predictor =pred[,1],
                              levels = levels(testset$TravelInsurance))
              auc.list[count] =  rpart.ROC$auc
            #Plotting
              pred.rocr = prediction(pred[,1], testset$TravelInsurance)
              perf.tpr.rocr = performance(pred.rocr, "tnr","fnr") #FUNZIONA ANCHE CON I NEGATIVIIII MA NON LA MEDIA!!!!!!!!!!!!!!!
              perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")

              #<<Graf.A5>>  
              if(count==1){
                plot(perf.tpr.rocr,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
              }else{
                plot(perf.tpr.rocr, add=TRUE,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
              }
              count = count + 1
          }
        }
    #-----------------------------------------------------------------------------    
        
        #Get overall confusion matrix
        confusion.matrix = getOverallConfusionMatrix(vector)
        
        #>>Evaluation measure for classification
        accuracy = getAccuracy(confusion.matrix) #accuracy = 0,83
        accuracy.CI = getCI(vector, "accuracy") #ci=0,95
        #BoxPlot Accuracy
        perf.list = getPerfList(vector, "accuracy") 
        data=data.frame(unlist(perf.list))

       #Accuracy Distribution
        p.col = "#657DB8"
        s.col = "#494949"
        #<<Graf.A4>>  
        #ggplot(data = data, aes(x = "", y = unlist.perf.list.)) + 
         # geom_boxplot(color=s.col, fill=p.col) +
          #geom_jitter(color="#FF7700", shape=4, stroke = 1.5)+
          #labs(y="Accuracy", x="")+
          #theme(axis.title=element_text(size=16))

        #> Misure per classe
        precision.yes = getPrecision(confusion.matrix, "y")
        precision.yes.CI = getCI(vector, "precision.yes") #ci=0,95
        precision.no = getPrecision(confusion.matrix, "n")
        precision.no.CI = getCI(vector, "precision.no") #ci=0,95
        
        recall.yes = getRecall(confusion.matrix, "y")
        recall.yes.CI = getCI(vector, "recall.yes") #ci=0,95
        recall.no = getRecall(confusion.matrix, "n")
        recall.no.CI = getCI(vector, "recall.no") #ci=0,95
        
        fmeasure.yes = getFmeasure(confusion.matrix, "y")
        fmeasure.yes.CI = getCI(vector, "fmeasure.yes") #ci=0,95
        fmeasure.no = getFmeasure(confusion.matrix, "n")
        fmeasure.no.CI = getCI(vector, "fmeasure.no") #ci=0,95
        
        #> Aggregrazione per avere misura di performance globale (Micro Average)
        precision = getMicroAverage("precision", confusion.matrix)
        recall = getMicroAverage("recall", confusion.matrix)
        fmeasure = getMicroAverage("fmeasure", confusion.matrix)
        
        #AUC (from auc.list)
        auc = getAuc(auc.list)
        auc.CI = getCI(auc.list, "auc") #ci=0,95
      
        title(main=paste("mean-AUC:",auc))
        
  #Modello INFORMATION GAIN   
    rpart.model = rpart(TravelInsurance ~ ., data=df, method="class",parms =
                          list(split = 'information'))
    #<<Graf.A6>>    
    fancyRpartPlot(rpart.model, main="", sub="", palettes="BuGn", type=2, yesno=1)
    #equal
    
    
#[SVM]__________________________________________________________________________
    
    #Preprocessing
    df$EverTravelledAbroad = as.numeric(df$EverTravelledAbroad)-1
    df$GovernmentEmployment = as.numeric(df$GovernmentEmployment)-3
    df$Graduate = as.numeric(df$Graduate)-1
    df$FamilyMembers = as.numeric(df$FamilyMembers)
    df$ChronicDiseases = as.numeric(df$ChronicDiseases)-1
    df$FrequentFlyer = as.numeric(df$FrequentFlyer)-1
        
    str(df)
    
   #KERNEL = POLYNOMIAL #Notare che sono utilizzati degree,gamma,coef() di defautl!!
    #Scelta del costo C --------------------------------------------------------
    set.seed(NULL)
    costs=c(0.001, 0.01, 0.1, 1,5,10,100)
    
    acc.matrix = matrix(nrow = 2, ncol = length(costs))
    acc.matrix[1,] = costs
    nc=1
    
    for(c in costs){
      k = 10
      dataset = df
      repeats = 3
      
      count=1
      vector=vector(mode = "list", length = k*repeats)
      auc.list = list()
      for (j in 1:repeats){
        dataset$id = 1:dim(dataset)[1]
        folds = createFolds(dataset$id, k = k, list = TRUE, returnTrain = FALSE)
        x=0
        for(i in folds){
          x=x+1
          testset = dataset[dataset$id %in% i, ]
          trainset = dataset[!dataset$id %in% i,]
          model = svm(TravelInsurance ~ ., data=trainset, kernel='polynomial', cost=c, scale=TRUE)
          pred = predict(model, testset)
          vector[[count]] <- table(pred, testset$TravelInsurance)
          count = count + 1
        }
      }
      confusion.matrix = getOverallConfusionMatrix(vector)
      accuracy = getAccuracy(confusion.matrix) 
      acc.matrix[2,nc]=accuracy
      nc=nc+1
    }
    
    dd <- data.frame(factor(acc.matrix[1,]), acc.matrix[2,])
    colnames(dd) <- c('Cost','Accuracy')
    
    #<<Graf.A10>>  
    ggplot(dd, aes(x=Cost, y=Accuracy)) + 
      geom_bar(stat="identity", width=0.4, fill=p.col, color=s.col) +
      geom_hline(yintercept=max(dd$Accuracy),  linetype="dashed", color = "#2C51FC",  size=0.8)  
    

    #Performance Evaluation    
    #Repeated k-CROSS-VALIDATION--------------------------------------------------
    set.seed(12)
    k = 10
    dataset = df
    repeats = 3
    
    count=1
    vector=vector(mode = "list", length = k*repeats)
    auc.list = list()
    
    for (j in 1:repeats){
      dataset$id = 1:dim(dataset)[1]
      folds = createFolds(dataset$id, k = k, list = TRUE, returnTrain = FALSE)
      x=0
      for(i in folds){
        #Split
          x=x+1
          testset = dataset[dataset$id %in% i, ]
          trainset = dataset[!dataset$id %in% i,]
          #dim(train.set)
          #dim(test.set)
        #Training
          model = svm(TravelInsurance ~ ., data=trainset, kernel='polynomial', cost=5, scale=TRUE,probability=TRUE)
          #Inference
          pred = predict(model, testset)
          #Save partial confusion matrix
          vector[[count]] <- table(pred, testset$TravelInsurance)
        #AUC (only positive)  
          pred.all = predict(model, testset, probability=TRUE)
          pred=attr(pred.all, "probabilities")
          rpart.ROC = roc(response = testset$TravelInsurance, predictor =pred[,1],
                          levels = levels(testset$TravelInsurance))
          auc.list[count] =  rpart.ROC$auc
          #Plotting
          pred.rocr = prediction(pred[,2], testset$TravelInsurance)
          perf.tpr.rocr = performance(pred.rocr, "tnr","fnr")
          perf.rocr = performance(pred.rocr, measure = "auc")
          #<<Graf.A12>>  
          if(count==1){
            plot(perf.tpr.rocr,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
          }else{
            plot(perf.tpr.rocr, add=TRUE,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
          }
          count = count + 1
        }
    }
    #-----------------------------------------------------------------------------    
    
    #Get overall confusion matrix
    confusion.matrix = getOverallConfusionMatrix(vector)
    
    #>>Evaluation measure for classification
    accuracy = getAccuracy(confusion.matrix) #accuracy = 0,83
    accuracy.CI = getCI(vector, "accuracy") #ci=0,95
    #BoxPlot Accuracy
    perf.list = getPerfList(vector, "accuracy") 
    data=data.frame(unlist(perf.list))
    
    #Accuracy Distribution
    p.col = "#7596E3"
    s.col = "#4C4C4C"
    #<<Graf.A11>>  
    #ggplot(data = data, aes(x = "", y = unlist.perf.list.)) + 
    # geom_boxplot(color=s.col, fill=p.col) +
    #geom_jitter(color="#2C51FC", shape=8, stroke = 1)+
    #labs(y="Accuracy", x="")+
    #theme(axis.title=element_text(size=16))
    
    #> Misure per classe
    precision.yes = getPrecision(confusion.matrix, "y")
    precision.yes.CI = getCI(vector, "precision.yes") #ci=0,95
    precision.no = getPrecision(confusion.matrix, "n")
    precision.no.CI = getCI(vector, "precision.no") #ci=0,95
    
    recall.yes = getRecall(confusion.matrix, "y")
    recall.yes.CI = getCI(vector, "recall.yes") #ci=0,95
    recall.no = getRecall(confusion.matrix, "n")
    recall.no.CI = getCI(vector, "recall.no") #ci=0,95
    
    fmeasure.yes = getFmeasure(confusion.matrix, "y")
    fmeasure.yes.CI = getCI(vector, "fmeasure.yes") #ci=0,95
    fmeasure.no = getFmeasure(confusion.matrix, "n")
    fmeasure.no.CI = getCI(vector, "fmeasure.no") #ci=0,95
    
    #> Aggregrazione per avere misura di performance globale (Micro Average)
    precision = getMicroAverage("precision", confusion.matrix)
    recall = getMicroAverage("recall", confusion.matrix)
    fmeasure = getMicroAverage("fmeasure", confusion.matrix)
    
    #AUC (from auc.list)
    auc = getAuc(auc.list)
    auc.CI = getCI(auc.list, "auc") #ci=0,95
    
    title(main=paste("mean-AUC:",auc))

    
    
    #KERNEL = LINEAR 
    #Scelta del costo C --------------------------------------------------------
    set.seed(NULL)
    costs=c(0.001, 0.01, 0.1, 1,5,10,100)
    
    acc.matrix = matrix(nrow = 2, ncol = length(costs))
    acc.matrix[1,] = costs
    nc=1
    
    for(c in costs){
      k = 10
      dataset = df
      repeats = 3
      
      count=1
      vector=vector(mode = "list", length = k*repeats)
      auc.list = list()
      for (j in 1:repeats){
        dataset$id = 1:dim(dataset)[1]
        folds = createFolds(dataset$id, k = k, list = TRUE, returnTrain = FALSE)
        x=0
        for(i in folds){
          x=x+1
          testset = dataset[dataset$id %in% i, ]
          trainset = dataset[!dataset$id %in% i,]
          model = svm(TravelInsurance ~ ., data=trainset, kernel='linear', cost=c, scale=TRUE)
          pred = predict(model, testset)
          vector[[count]] <- table(pred, testset$TravelInsurance)
          count = count + 1
        }
      }
      confusion.matrix = getOverallConfusionMatrix(vector)
      accuracy = getAccuracy(confusion.matrix) 
      acc.matrix[2,nc]=accuracy
      nc=nc+1
    }
    
    dd <- data.frame(factor(acc.matrix[1,]), acc.matrix[2,])
    colnames(dd) <- c('Cost','Accuracy')
    
    #<<Graf.A7>>    
    ggplot(dd, aes(x=Cost, y=Accuracy)) + 
      geom_bar(stat="identity", width=0.4, fill="#7596e3", color=s.col) +
      geom_hline(yintercept=max(dd$Accuracy),  linetype="dashed", color = "red",  size=0.8)  
    
    
    
    
    #Performance Evaluation    
    #Repeated k-CROSS-VALIDATION--------------------------------------------------
    set.seed(12)
    k = 10
    dataset = df
    repeats = 3
    
    count=1
    vector=vector(mode = "list", length = k*repeats)
    auc.list = list()
    
    for (j in 1:repeats){
      dataset$id = 1:dim(dataset)[1]
      folds = createFolds(dataset$id, k = k, list = TRUE, returnTrain = FALSE)
      x=0
      for(i in folds){
        #Split
        x=x+1
        testset = dataset[dataset$id %in% i, ]
        trainset = dataset[!dataset$id %in% i,]
        #dim(train.set)
        #dim(test.set)
        #Training
        model = svm(TravelInsurance ~ ., data=trainset, kernel='linear', cost=0.001, scale=TRUE,probability=TRUE)
        #Inference
        pred = predict(model, testset)
        #Save partial confusion matrix
        vector[[count]] <- table(pred, testset$TravelInsurance)
        #AUC (only positive)  
        pred.all = predict(model, testset, probability=TRUE)
        pred=attr(pred.all, "probabilities")
        rpart.ROC = roc(response = testset$TravelInsurance, predictor =pred[,2],
                        levels = levels(testset$TravelInsurance))
        auc.list[count] =  rpart.ROC$auc
        #Plotting
        pred.rocr = prediction(pred[,2], testset$TravelInsurance)
        perf.tpr.rocr = performance(pred.rocr, "tnr","fnr")
        perf.rocr = performance(pred.rocr, measure = "auc")
        #<<Graf.A9>>  
        if(count==1){
          plot(perf.tpr.rocr,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
        }else{
          plot(perf.tpr.rocr, add=TRUE,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
        }
        count = count + 1
      }
    }
    #-----------------------------------------------------------------------------    
    
    #Get overall confusion matrix
    confusion.matrix = getOverallConfusionMatrix(vector)
    
    #>>Evaluation measure for classification
    accuracy = getAccuracy(confusion.matrix) #accuracy = 0,83
    accuracy.CI = getCI(vector, "accuracy") #ci=0,95
    #BoxPlot Accuracy
    perf.list = getPerfList(vector, "accuracy") 
    data=data.frame(unlist(perf.list))
    
    #Accuracy Distribution
    p.col = "#7596E3"
    s.col = "#4C4C4C"
    #<<Graf.A8>>  
    #ggplot(data = data, aes(x = "", y = unlist.perf.list.)) + 
    # geom_boxplot(color=s.col, fill=p.col) +
    #geom_jitter(color="#2C51FC", shape=8, stroke = 1)+
    #labs(y="Accuracy", x="")+
    #theme(axis.title=element_text(size=16))
    
    #> Misure per classe
    precision.yes = getPrecision(confusion.matrix, "y")
    precision.yes.CI = getCI(vector, "precision.yes") #ci=0,95
    precision.no = getPrecision(confusion.matrix, "n")
    precision.no.CI = getCI(vector, "precision.no") #ci=0,95
    
    recall.yes = getRecall(confusion.matrix, "y")
    recall.yes.CI = getCI(vector, "recall.yes") #ci=0,95
    recall.no = getRecall(confusion.matrix, "n")
    recall.no.CI = getCI(vector, "recall.no") #ci=0,95
    
    fmeasure.yes = getFmeasure(confusion.matrix, "y")
    fmeasure.yes.CI = getCI(vector, "fmeasure.yes") #ci=0,95
    fmeasure.no = getFmeasure(confusion.matrix, "n")
    fmeasure.no.CI = getCI(vector, "fmeasure.no") #ci=0,95
    
    #> Aggregrazione per avere misura di performance globale (Micro Average)
    precision = getMicroAverage("precision", confusion.matrix)
    recall = getMicroAverage("recall", confusion.matrix)
    fmeasure = getMicroAverage("fmeasure", confusion.matrix)
    
    #AUC (from auc.list)
    auc = getAuc(auc.list)
    auc.CI = getCI(auc.list, "auc") #ci=0,95
    
    title(main=paste("mean-AUC:",auc))
    
    
    #KERNEL = POLYNOMIAL #Tuning su Degree, Gamma, coef(
    
    #tuned = tune.svm(TravelInsurance~., data=df, kernel="polynomial",
    #                 degree=c(3,4,5), coef0=c(0.1,0.5,1,2,3,4),
    #                cost=c(0.001, 0.01, 0.1, 1,5,10,100))
    
    #OUT: degree 4     gamma 1 cost 0.001
    
    #Performance Evaluation    
    #Repeated k-CROSS-VALIDATION--------------------------------------------------
    set.seed(12)
    k = 10
    dataset = df
    repeats = 3
    
    count=1
    vector=vector(mode = "list", length = k*repeats)
    auc.list = list()
    
    for (j in 1:repeats){
      dataset$id = 1:dim(dataset)[1]
      folds = createFolds(dataset$id, k = k, list = TRUE, returnTrain = FALSE)
      x=0
      for(i in folds){
        #Split
        x=x+1
        testset = dataset[dataset$id %in% i, ]
        trainset = dataset[!dataset$id %in% i,]
        #dim(train.set)
        #dim(test.set)
        #Training
        model = svm(TravelInsurance ~ ., data=trainset, kernel='polynomial', cost=0.001, gamma=1, degree=4, scale=TRUE,probability=TRUE)
        #Inference
        pred = predict(model, testset)
        #Save partial confusion matrix
        vector[[count]] <- table(pred, testset$TravelInsurance)
        #AUC (only positive)  
        pred.all = predict(model, testset, probability=TRUE)
        pred=attr(pred.all, "probabilities")
        rpart.ROC = roc(response = testset$TravelInsurance, predictor =pred[,2],
                        levels = levels(testset$TravelInsurance))
        auc.list[count] =  rpart.ROC$auc
        #Plotting
        pred.rocr = prediction(pred[,2], testset$TravelInsurance)
        perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
        perf.rocr = performance(pred.rocr, measure = "auc")
        #<<Graf.A12>>  
        if(count==1){
          plot(perf.tpr.rocr,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
        }else{
          plot(perf.tpr.rocr, add=TRUE,  colorize=T)#,main=paste("AUC:",(perf.rocr@y.values)))
        }
        count = count + 1
      }
    }
    #-----------------------------------------------------------------------------    
    
    #Get overall confusion matrix
    confusion.matrix = getOverallConfusionMatrix(vector)
    
    #>>Evaluation measure for classification
    accuracy = getAccuracy(confusion.matrix) #accuracy = 0,83
    accuracy.CI = getCI(vector, "accuracy") #ci=0,95
    #BoxPlot Accuracy
    perf.list = getPerfList(vector, "accuracy") 
    data=data.frame(unlist(perf.list))
    
    #Accuracy Distribution
    p.col = "#7596E3"
    s.col = "#4C4C4C"
    #<<Graf.A11>>  
    #ggplot(data = data, aes(x = "", y = unlist.perf.list.)) + 
    # geom_boxplot(color=s.col, fill=p.col) +
    #geom_jitter(color="#2C51FC", shape=8, stroke = 1)+
    #labs(y="Accuracy", x="")+
    #theme(axis.title=element_text(size=16))
    
    #> Misure per classe
    precision.yes = getPrecision(confusion.matrix, "y")
    precision.yes.CI = getCI(vector, "precision.yes") #ci=0,95
    precision.no = getPrecision(confusion.matrix, "n")
    precision.no.CI = getCI(vector, "precision.no") #ci=0,95
    
    recall.yes = getRecall(confusion.matrix, "y")
    recall.yes.CI = getCI(vector, "recall.yes") #ci=0,95
    recall.no = getRecall(confusion.matrix, "n")
    recall.no.CI = getCI(vector, "recall.no") #ci=0,95
    
    fmeasure.yes = getFmeasure(confusion.matrix, "y")
    fmeasure.yes.CI = getCI(vector, "fmeasure.yes") #ci=0,95
    fmeasure.no = getFmeasure(confusion.matrix, "n")
    fmeasure.no.CI = getCI(vector, "fmeasure.no") #ci=0,95
    
    #> Aggregrazione per avere misura di performance globale (Micro Average)
    precision = getMicroAverage("precision", confusion.matrix)
    recall = getMicroAverage("recall", confusion.matrix)
    fmeasure = getMicroAverage("fmeasure", confusion.matrix)
    
    #AUC (from auc.list)
    auc = getAuc(auc.list)
    auc.CI = getCI(auc.list, "auc") #ci=0,95
    
    title(main=paste("mean-AUC:",auc))
    
    
    
#COMPARING MODELS______________________________________________________________
    #AUC
    x=c("Model3a","Model4a","Model4b")
    y=c(0.78,0.75,0.78)
    lower=c(0.769,0.728,0.769)
    upper=c(0.784,0.797,0.835)
    data=data.frame(x,y,lower,upper)

    
    ggplot(data, aes(x, y)) +        # ggplot2 plot with confidence intervals
      geom_point(size = 3.5) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
      labs(y="AUC", x="Models")+
      theme(axis.text=element_text(size=12, colour=axis.text.col),
            axis.title=element_text(size=16, colour=axis.title.col)) 
    
    #Accuracy
    x=c("Model3a","Model4a","Model4b")
    y=c(0.83,0.75,0.80)
    lower=c(0.818,0.736,0.788)
    upper=c(0.84,0.764,0.814)
    data=data.frame(x,y,lower,upper)
    
    
    ggplot(data, aes(x, y)) +        # ggplot2 plot with confidence intervals
      geom_point(size = 3.5) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)+
      labs(y="Accuracy", x="Models")+
      theme(axis.text=element_text(size=12, colour=axis.text.col),
            axis.title=element_text(size=16, colour=axis.title.col))
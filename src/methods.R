library(Rmisc)

#Function: splits our dataset in train and test
#Parameters: 
# >data: input dataset 
# >p: proportion of generated subset from the input dataset 
# >s: random seed
split.data = function(data, p = 0.7, s = 1){
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train=train, test=test)) 
}

create.alterning.colors.list = function(color1, color2, size){
  list = NULL
  for (i in 1:size){
    if((i %% 2) == 0) {#Even
      list = append(list, color1)
    } else {
      list = append(list, color2)
    }
  }
  return(list)
}

moda = function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getAccuracy = function(table){
  return(round((table[1,1]+table[2,2])/(table[1,1]+table[1,2]+table[2,1]+table[2,2]), 10))
}

getPrecision = function(table, feature){ # # of instances correctly predicted as l /
                                         # # of instances predicted as l
  if(feature=="y"){
    return(round(table[1,1]/(table[1,1]+table[1,2]),2))
  }else if(feature=="n"){
    return(round(table[2,2]/(table[2,2]+table[2,1]),2)) 
  }else{
    result="error"
  }
}

getRecall = function(table, feature){ # # of instances correctly predicted as l /
                                      # # of instances of class l
  if(feature=="y"){
    return(round(table[1,1]/(table[1,1]+table[2,1]),2))
  }else if(feature=="n"){
    return(round(table[2,2]/(table[2,2]+table[1,2]),2)) 
  }else{
    result="error"
  }
}

getFmeasure = function(table, feature){ # 2 x P(l) x R(l)/
                                        # P(l)+ R(l)
  if(feature=="y"){
    return(round(2*getPrecision(table,"y")*getRecall(table,"y")/(getPrecision(table,"y")+getRecall(table,"y")),2))
  }else if(feature=="n"){
    return(round(2*getPrecision(table,"n")*getRecall(table,"n")/(getPrecision(table,"n")+getRecall(table,"n")),2))
  }else{
    result="error"
  }
}

getMicroAverage = function(measure, table){ 
  if(measure=="precision"){
    return(round((getPrecision(table, "y")*(table[1,1]+table[2,1])/(table[1,1]+table[2,1]+table[1,2]+table[2,2]))+(getPrecision(table, "n")*(table[1,2]+table[2,2])/(table[1,1]+table[2,1]+table[1,2]+table[2,2])) ,2))
  }else if(measure=="recall"){
    return(round((getRecall(table, "y")*(table[1,1]+table[2,1])/(table[1,1]+table[2,1]+table[1,2]+table[2,2]))+(getRecall(table, "n")*(table[1,2]+table[2,2])/(table[1,1]+table[2,1]+table[1,2]+table[2,2])) ,2))
  }else if(measure=="fmeasure"){
    return(round((getFmeasure(table, "y")*(table[1,1]+table[2,1])/(table[1,1]+table[2,1]+table[1,2]+table[2,2]))+(getFmeasure(table, "n")*(table[1,2]+table[2,2])/(table[1,1]+table[2,1]+table[1,2]+table[2,2])) ,2))
  }else{
    result="error"
  }
}

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

getOverallConfusionMatrix = function(vector){
  tab = matrix(c(0,0,0,0), ncol=2, byrow=TRUE)
  colnames(tab) <- c('Yes','No')
  rownames(tab) <- c('Yes','No')
  tab = as.table(tab)
  
  for(i in 1:length(vector)){
      tab = tab + vector[[i]]
  }
  
  return(tab)
}

getCI = function(vector, perf){
  return(CI(as.numeric(getPerfList(vector,perf)), ci=0.95))
}

getPerfList = function(vector, perf){
  list = list()
  
  if(perf=="accuracy"){
    for(i in 1:length(vector)){
      list[i]= getAccuracy(vector[[i]])
    }
  }else if(perf=="precision.yes"){
    for(i in 1:length(vector)){
      list[i]= getPrecision(vector[[i]],"y")
    }
  }else if(perf=="precision.no"){
    for(i in 1:length(vector)){
      list[i]= getPrecision(vector[[i]],"n")
    }
  }else if(perf=="recall.yes"){
    for(i in 1:length(vector)){
      list[i]= getRecall(vector[[i]],"y")
    }
  }else if(perf=="recall.no"){
    for(i in 1:length(vector)){
      list[i]= getRecall(vector[[i]],"n")
    }
  }else if(perf=="fmeasure.yes"){
    for(i in 1:length(vector)){
      list[i]= getFmeasure(vector[[i]],"y")
    }
  }else if(perf=="fmeasure.no"){
    for(i in 1:length(vector)){
      list[i]= getFmeasure(vector[[i]],"n")
    }
  }else if(perf=="auc"){
    return(vector)
  }else{
    return("error")
  }
  return(list)
}

getAuc = function(vector){
  sum = 0
  for(i in 1:length(vector)){
    sum = sum + vector[[i]]
  }
  return(round(sum/length(vector),2))
}
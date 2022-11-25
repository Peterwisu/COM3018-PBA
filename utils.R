
#####################################
#
# TreeCoeff_plot()
#
# Plot importance fields of Decsion Tree model on barchart
#
# Inputs : DT - Decsion Tree Model
###################################
TreeCoeff_plot<-function(DT){
    
    library(RColorBrewer)
    coul <- brewer.pal(5, "Set2")
    importance<-C50::C5imp(DT, metric = "usage")
    names(importance)<-"Strength"

    importance<-importance[order(importance$Strength,decreasing=FALSE),,drop=FALSE]

    print(formattable::formattable(importance))
    # Plot the importance fields

    barplot(t(importance),las=2,border = 0, cex.names =0.7, main="Decision Tree", col=coul ,horiz=T)
}

#########################################################
# ForestCoeff_plot()
#
# Plot importance fields of a Random Forest Classifier model 
#
# Inputs : model -  Random Forest Model
#########################################################
ForestCoeff_plot<-function(model){
    

    library(RColorBrewer)
    coul <- brewer.pal(5, "Set2")

    importance<-randomForest::importance(model,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=FALSE),,drop=FALSE]

    colnames(importance)<-"Strength"

    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main='Random Forest',
           horiz=T,
           col=coul)
    
}



#######################################################################
# LogisticCoeff_plot()
#
# Plot the weight or value of coefficient in logisitic regression model
#
# Inputs :  model - Logsitic Regression model 
#######################################################################
LogisticCoeff_plot<-function(model){
    
# Plotting sorted model coefficient in barplot 
par(las=2) # make label text perpendicular to axis
par(mar=c(10,2,2,2)) # increase y-axis margin.
options(repr.plot.width = 10, repr.plot.height = 10)
coeff<-model$coeff[-which(names(model$coeff)%in%c('(Intercept)'))]
barplot(unname(sort(coeff)), main="Distribution of Logistic Regression Coefficient", names.arg=names(sort(coeff)), cex.names=.9, col=rgb(0.2,0.4,0.6,0.6) )

}


LogisticCurve_plot<-function(model, label){
   # probability=
    
    label = ifelse(label==1,"satisfied(1)","neutral or dissatisfied(0)")
    print("Logistic Plot")
    df<-data.frame(probability = model$fitted.values,
                   satisfy = label)
 
    df <- df[order(df$probability, decreasing=FALSE),]
                                           
    
    df$rank <- 1:nrow(df)
                                           
    ggplot(data=df, aes(x=rank, y=probability)) +
      geom_point(aes(color=satisfy), alpha=1, shape=4, stroke=2) +
      xlab("Index of dataset") +
      ylab("Predicted probability of passenger satisfaction")+
      labs( title = "\n Logistic Regression Curve \n") +
        theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        title = element_text(face="bold", color="black", size= 15),
        legend.title = element_text(face="bold", size = 12))
    
    
}

##################################################################
#
# ConfusionMatrix_plot()
#
# Plot a confusion matrix containing a values of TP, TN, FP and FN of a model
#
# inputs : gt - ground truth
#          pred_labels - predicted labels
#
###################################################################
ConfusionMatrix_plot<-function(gt,pred_labels){
    
    cm<-table(gt,pred_labels)

    c_matplot<-confusionMatrix(factor(gt,levels=1:0),factor(pred_labels,levels=1:0,),
                          dnn= c('Prediction','Reference'))
    plt <- data.frame(c_matplot$table)
        plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

    print(ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
            geom_tile() + geom_text(aes(label=Freq)) +
            scale_fill_gradient(low="white", high="#009194") +
            labs(x = "Reference",y = "Prediction",  title = "\n Confusiuon Matrix\n") +
            scale_x_discrete(labels=c("neutral or dissatisfied(0)","satisfied(1)")) +
            scale_y_discrete(labels=c("satisfied(1)","neutral or dissatisfied(0)"))+
           theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        title = element_text(face="bold", color="black", size= 15),
        legend.title = element_text(face="bold", size = 12))
      )
    }


####################################################
# F1_score()
#
#Calculate a F1 score 
#
#inputs : precsion - precison values of a model
#          recll    -  recall value of a model
#
#return : F1 score -  F1 score of a model
#
####################################################
F1_score<-function(precision,recall){
    
        score<-2*((precision*recall)/(precision+recall))
    
        return(score)
    }


###################################################
# BCE_loss()
#
# Calculate Binary Cross Entropy loss 
#
# input : y - grouth truth labels
#         y_hat - predicted probabilites
#
# return : loss -  loss calculated from BCE loss
##################################################
BCE_loss<-function(y,y_hat){
    

    term1<- (y*log(y_hat+1e-7)) # =1e-7 is to prevent the value in side a log function becoming 0
    term2<- (1-y)*log(1-y_hat+1e-7)
    
    loss<- -1* mean(term1+term2)
    
    return(loss)
}





##########################################  From Lab


# ************************************************
# allocateFoldID() :
#
# Append a column called "foldID" that indicates the fold number
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
allocateFoldID<-function(dataset){
  recordsPerFold<-ceiling(nrow(dataset)/KFOLDS)

  foldIds<-rep(seq(1:KFOLDS),recordsPerFold)

  foldIds<-foldIds[1:nrow(dataset)]

  dataset$foldId<-foldIds

  return(dataset)
} #endof allocateFoldID()


# ************************************************
# stratifiedDataset() :
#
# Split dataset by the class (assume 2-class)
# Calculate the number of records that will appear in each fold
# Give each of these blocks a unique foldID
# combine the datasets & randomise
# The dataset now has a foldID from which we can select the data
# for the experiments
#
# INPUT   :   data frame         - dataset        - dataset
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
stratifiedDataset<-function(originalDataset){

  positionClassOutput=which(names(originalDataset)==OUTPUT_FIELD)

  # Get the unique class values
  classes<-unique(originalDataset[,positionClassOutput])

  # Split dataset into the two classes (so as to keep the class balance the same in the datasets)
  indexClass1<-which(originalDataset[,positionClassOutput]==classes[1])
  split1<-originalDataset[indexClass1,]
  split2<-originalDataset[-indexClass1,]

  # Append a column that indicates the fold number for each class
  split1<-allocateFoldID(split1)
  split2<-allocateFoldID(split2)

  # Combine the two datasets

  newDataset<-rbind(split1,split2)

  #Randomise the classes
  newDataset<-newDataset[order(runif(nrow(newDataset))),]

  return(newDataset)
}


# ************************************************
# stratifiedSplit() :
#
# Generate the TRAIN and TEST dataset based on the current fold
#
# INPUT   :   data frame         - dataset
#
# OUTPUT  :   list               - train & test datasets
# ************************************************

stratifiedSplit<-function(newDataset,fold){

  test<-subset(newDataset, subset= foldId==fold, select=-foldId)
  train<-subset(newDataset, subset= foldId!=fold,select=-foldId)

  return(list(
    train=train,
    test=test))
}



# ************************************************
# runExperiment() :
#
#
# INPUT   :   data frame         - dataset        - dataset
#             object function    - FUN            - name of function
#             ...                - optional        - parameters are passed on
#
# OUTPUT  :   data frame         - dataset        - dataset with foldID added
#
# ************************************************
runExperiment<-function(dataset,FUN, ...){

  allResults<-data.frame()

  for (k in 1:KFOLDS){

    

    splitData<-stratifiedSplit(newDataset=dataset,fold=k)
    print(paste("FOLD : ,",k))
    out<-FUN(training_data=splitData$train,
                  testing_data=splitData$test,
             plot=FALSE,
                  ...)
    measures<-out$result
    measures <-measures[-which(names(measures) %in%c("pred_labels","gt","proba"))]


    

    allResults<-rbind(allResults,data.frame(measures))
  } #endof for()

  # Return the means from all the experiments back as a list
  # 260221NRT round metrics to 2 decimal places
  getMeans<-round(colMeans(allResults),digits=2)

  getMeans[1:4]<-as.integer(getMeans[1:4])  # TP, FN, TN, FP are rounded to ints


  #290520NRT return the above with the rounded values
  return(list("means" = as.list(getMeans), "allresults"= allResults))
} #endof runExperiment()


# ************************************************
# NConvertClass() :
#
# In original dataset, $Status is the classification label
# We need to convert this to give the minority class a value of 0
# this just makes it easiert to define the confusioon matrix!
# for the UCI-G this is a class of {0,1} being {bad, good}
#
# INPUT   :
#             Data Frame        - dataset
#
# OUTPUT  :
#             Data Frame        - dataset
#
# ************************************************
NConvertClass<-function(dataset,OUTPUT_FIELD){
  positionClassOutput<-which(names(dataset)==OUTPUT_FIELD)
  classes<-sort(table(dataset[,positionClassOutput])) #smallest class will be first
  minority<-names(classes[1])
  indexToStatus2<-which(dataset[positionClassOutput]==minority)
  dataset[positionClassOutput][indexToStatus2,]<-0
  dataset[positionClassOutput][-indexToStatus2,]<-1
  return(dataset)
}



# ************************************************
# Nauroc() :
#
# Calculate the Area Under Curve (AUC) for ROC
#
# INPUT   :   vector double     - score            - probability of being class 1
#             vector double     - bool             - Expected class of 0 or 1
#
# OUTPUT  :   double   - AUC
#
# ************************************************
# By Miron Kursa https://mbq.me
# See https://stackoverflow.com/questions/4903092/calculate-auc-in-r

auroc <- function(score, bool) {
  bool<-as.numeric(bool)
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}


# ************************************************
# NdetermineThreshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - test_predicted   - probability of being class 1
#         :   vector double  - test_expected    - dataset to evaluate
#         :   boolean        - plot             - TRUE=output charts
#         :   string         - title            - chart title
#
# OUTPUT  :   List       - Named evaluation measures from confusion matrix
#                        - Threshold at min Euclidean distance
#                        - AUC - area under the ROC curve
#                        - Predicted class probability
#
# 241019NRT - added plot flag and title for charts
# 311019NRT - added axis bound checks in abline plots
# 191020NRT - Updated to use own ROC plot & calculate AUC
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
 
  toPlot<-data.frame()

  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
  
    results<-eval_model(proba=test_predicted,
                                gt_label=test_expected,
                                threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }

  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  # Youdan = sensitivty + specificity -1
  #        = TPR + (1-FPR) -1

  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1

  # 121020NRT - max Youdan
  # use which.max() to return a single index to the higest value in the vector
  maxYoudan<-toPlot$x[which.max(toPlot$youdan)]

  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))

  # 121020NRT - Euclidean distance to "perfect" classifier (smallest the best)
  # use which.min() to return a single index to the lowest value in the vector
  minEuclidean<-toPlot$x[which.min(toPlot$distance)]

  # ************************************************
  # Plot threshold graph

  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))+
      theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        title = element_text(face="bold", color="black", size= 15),
        legend.title = element_text(face="bold", size = 12))

    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)

    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]

    if (!is.na(crosspoint)){
      if ((crosspoint<1) & (crosspoint>0))
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }

    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)

    # Plot the min distance, as might be more (311019NRT check it is within range)
    if ((minEuclidean<1) & (minEuclidean>0))
      abline(v=minEuclidean,col="green",lty=3,lwd=2)

    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)

    if ((maxYoudan<1) & (maxYoudan>0))
      abline(v=maxYoudan,col="purple",lty=3,lwd=2)

    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
    text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nEuclidean=",minEuclidean,"\nYoudan=",maxYoudan))

    # ************************************************
    # 121020NRT ROC graph

    sensitivityROC<-toPlot$tpr[which.min(toPlot$distance)]
    specificityROC<-100-toPlot$fpr[which.min(toPlot$distance)]
    auc<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC

    # Set origin point for plotting
    toPlot<-rbind(toPlot,data.frame(x=0,fpr=0,tpr=0, youdan=0,distance=0))

    plot(toPlot$fpr/100,toPlot$tpr/100,type="l",lwd=3, col="red",
         main=paste("ROC:",title),
         xlab="Specificity (1-FPR) %",
         ylab="Sensitivity (TPR) %",
    )
    

    axis(1, seq(0.0,100,10))
    axis(2, seq(0.0,100,10))

    #Add crosshairs to the graph
    abline(h=sensitivityROC/100,col="green",lty=3,lwd=2)
    abline(v=1-(specificityROC/100),col="green",lty=3,lwd=2)
    abline(c(0,0),c(1,1),col="black",lty=3,lwd=2)

    annotate<-paste("Threshold: ",round(minEuclidean,digits=4L),
                    "\nTPR: ",round(sensitivityROC,digits=2L),
                    "%\n1-FPR: ",round(specificityROC,digits=2L),
                    #"%\nAUC: ",round(auc,digits=2L),
                    sep="")

    text(x=1-(specificityROC/100), y=(sensitivityROC/100), adj = c(-0.2,1.2),cex=1, col="red",annotate)
    auc_legend = paste("AUC = ", round(auc,digits=2L))
      
      
    legend("bottom",c(auc_legend,'Random Classifier'), col=c("red",'black'),lty=c('solid','dashed') ,ity=1:2, lwd=2)

  } # endof if plotting

  # Select the threshold - I have choosen distance

  myThreshold<-minEuclidean      # Min Distance should be the same as analysis["threshold"]

  #Use the "best" distance threshold to evaluate classifier
  results<-eval_model(proba=test_predicted,
                                gt_label=test_expected,
                                threshold=myThreshold)

  results$threshold<-myThreshold
  results$AUC<-auroc(score=test_predicted,bool=test_expected) # Estimate the AUC

  return(results)
} #endof myPerformancePlot()






######################################################################################
# modified function from lab
#
# eval_model()
#
# evaluate the performance of a model by using a certain threshold
#
# inputs : proba - predicted probabilites
#
#          gt_label - grounf truth labels
#
#          threshold - The threshold value use from converting a probabilites to labels
#
# returns:  list - list containing an evalution metrics and predicted outputs
##########################################################################################

eval_model<-function(proba, gt_label,threshold){
    
        
    #loss<-BCE_loss(gt_label,proba)
    pred_label<-ifelse(proba<threshold,0,1)
    # Confusion matrix table
    cm<-table(factor(pred_label,levels=0:1), factor(gt_label, levels=0:1))
    # True positive
    TP<-as.double(cm[2,2])
    # False Negative 
    FN<-as.double(cm[1,2])
    # False Positive 
    FP<-as.double(cm[2,1])
    # True Negative 
    TN<-as.double(cm[1,1])
    
    # accuracy
    acc<- 100.0 *((TP+TN)/(TP+FP+FN+TN))
    
    # precision
    pgood<-100.0*(TP/(TP+FP))
    pbad<-100.0*(TN/(FN+TN))
    # False positive rate
    FPR<-100.0*(FP/(FP+TN))
    # True positive rate OR RECALL
    TPR<-100.0*(TP/(TP+FN))
    # True negative rate 
    TNR<-100.0*(TN/(FP+TN))
    #  Mathhew's Correlation Coefficient
    MCC<-((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    
    F1<-F1_score(pgood,TPR)
                 
    output<- list("TP"=TP,
                  "FN"=FN, 
                  "FP"=FP,
                  "TN"=TN,
                  "F1"=F1,
                  "acc"=acc,
                  "pgood"=pgood,
                  "pbad"=pbad,
                  "FPR"=FPR,
                  "TPR"=TPR,
                  "TNR"=TNR,
                  "MCC"=MCC,
                  "pred_labels"=pred_label,
                   "gt"=gt_label,
                  "proba" = proba
                 )
    
    
    return(output)
}






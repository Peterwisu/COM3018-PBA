
roc_plot<-function(probs,gt){
    
    library(pROC)
    
    
    roc_score=roc(gt, probs) #AUC score
    auc = (roc_score$auc) #  area under curve 
    sensitivity <-roc_score$sensitivities
    specificity <-roc_score$specificities
    thresholds <-roc_score$thresholds
    
    plot(1-specificity,sensitivity,type='l', lwd=3, main ="ROC curve -- Logistic Regression ", col="red",xlab="False Positive Rate", ylab="True Positive Rate")
    abline(c(0,0),c(1,1),col="black",lty=3,lwd=2)

    auc_legend = paste("AUC = ", auc)
   legend("bottom",c(auc_legend,'Random Classifier'), col=c("red",'black'),lty=c('solid','dashed') ,ity=1:2, lwd=2)


    
   return(auc) 
}

LogisticCoeff_plot<-function(model){
    
# Plotting sorted model coefficient in barplot 
par(las=2) # make label text perpendicular to axis
par(mar=c(10,2,2,2)) # increase y-axis margin.
options(repr.plot.width = 10, repr.plot.height = 10)
coeff<-model$coeff[-which(names(model$coeff)%in%c('(Intercept)'))]
barplot(unname(sort(coeff)), main="Distribution of Logistic Regression Coefficient", names.arg=names(sort(coeff)), cex.names=.9, col=rgb(0.2,0.4,0.6,0.6) )

}


LogisticCurve_plot<-function(model, label){probability=
    
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

F1_score<-function(precision,recall){
    
        score<-2*((precision*recall)/(precision+recall))
    
        return(score)
    }

BCE_loss<-function(y,y_hat){
    
    print(y_hat)
    term1<- (y*log(y_hat+1e-7))
    term2<- (1-y)*log(1-y_hat+1e-7)
    
    com<- -1* mean(term1+term2)
    
    return(com)
}
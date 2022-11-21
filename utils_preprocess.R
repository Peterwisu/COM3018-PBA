
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

######
# 
# Rescale a satisfaction scale column in range of 0.0-1.0
# Input:  columns contain a rating of satisfaction of passenger 
# Return: Normalize value in a range of 0-1
#
######
Preprocess_satscale<-function(input){
    
    
    # max and min is  5 and 0 respectively since the scale is from 0 to 5
    minscale<-0.0
    maxscale<-5.0
    
    rescale<- ((input-minscale)/(maxscale-minscale))
    
    return(rescale)
}
######
#
# Apply linear normalization for rescale a value in a fields into range 0-1
# Inputs : columns containig ordinal values
# Return : Normalize value in a range of 0-1
#
####
Preprocess_ordinalscale<-function(input){
    
     minscale<-min(input)
     maxscale<-max(input)
    
    rescale<- ((input-minscale)/ (maxscale-minscale))
    
    return(rescale)

}

########
#
# Normailze an entire dataframe into range of 0-1
# Inputs : dataframe containig ordinal values
# Return : Normalize dataframe
#
######
Preprocess_dataframe<-function(dataset,sat){
    
    # if this dataframe containing satisfaction rating then apply Preproccess_satscale
    if(sat==TRUE){
  
        fields<-sapply(as.data.frame(dataset),Preprocess_satscale)
   
    }else{
   
        fields<-sapply(as.data.frame(dataset),Preprocess_ordinalscale)
    }
    
    return(fields)
}


#######
#
# Preprocess Ordinal Value
# Inputs : Dataset , fieldtype of dataset
# Return : dataframe containing processed ordinal values
#
#######
Preprocess_ordinal<-function(dataset,fieldtype){
    
   
   sat_position<-which(fieldtype[,'is_sat']=='SAT')

   sat_fields=dataset[,sat_position]
  
   sat_fields<-Preprocess_dataframe(sat_fields,TRUE)
    
   ordinal<-which(fieldtype[,'types']=='ORDINAL')
  
   other_position<- ordinal[!(ordinal %in% sat_position)]
    
   colname = names(dataset[other_position])
   other_ordinal<-dataset[,other_position]
   
 
   other_ordinal<-Preprocess_dataframe(other_ordinal, FALSE)
    
   colnames(other_ordinal) <- colname
    
   processed<- cbind(other_ordinal,sat_fields)
  
   return(processed)
     
  
}



#######
#
#Convert a time delay in minutes into 3 numbers represent 3 categories
#
# 1. No Delays
# 2. 0-30 minutes delays
# 3. 30 and above delays
#
#Inputs : columns containig time delays in minutes
#Output : columns containd symbolic values of time delays
#
#######
Convert_minuteDelay<-function(data){
    
   
    out<-ifelse(data==0,'0',
         ifelse(data<30,"1",
         ifelse(data<60,"2","3")))
    
    return((out))
}

Convert_FlightDistance<-function(data){
    
    out<-ifelse(data<800,"short",
         ifelse(data>2200,"long","medium"))
                
    return(out)
}

########
#
#Convert a dataframe containing time delay in minutes to symbolic values
#Inputs : dataframe containing a time in minutes
#Output : dataframe contatinig symbolic values of time delay
#
########
Process_timeDelay<-function(dataset){
    
    out<-sapply(dataset,Convert_minuteDelay)
  
    return(out)
}

Process_distance<-function(dataset){
    
    out<-sapply(dataset,Convert_FlightDistance)
    
    return(out)
    
    }
####
#
# Function from lab 3
#
####
Preprocess_categorical<-function(dataset){

  catagorical<-data.frame()

  field_name<-names(dataset)

  for (field in field_name){

    # Convert into factors. A level for each unique string
    ffield<-factor(dataset[,field])

 

    # 1-hot encoding. A new column for each unique "level"
    xx<-data.frame(model.matrix(~ffield+0, data=ffield))

    names(xx)<-gsub("ffield",field,names(xx))

    # If 2 unique values, then can encode as a single "binary" column
    if (ncol(xx)==2){
      xx<-xx[,-2,drop=FALSE]
      names(xx)<-field  # Field name without the value appended
    }

  catagorical<-as.data.frame(append(catagorical,xx))

  } #endof for()
  return (catagorical)

} # endof categorical_encoding()


# Function from lab
myModelFormula<-function(dataset,fieldNameOutput,selectField){
     
  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput )],collapse = "+")

  output<-paste(fieldNameOutput,"~")

  formular=as.formula(paste(output,inputs))

  return(formular)

} 




######################################## Function from a lab without modificaiton ############################################


# Pre-Processing a Dataset functions
# To manually set a field type
# This will store $name=field name, $type=field type
manualTypes <- data.frame()


# ************************************************
# NPREPROCESSING_outlier() :
# Determine if a value of a record is an outlier for each field
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
# https://cran.r-project.org/web/packages/outliers/outliers.pdf

NPREPROCESSING_outlier<-function(ordinals,confidence){

  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){

    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])

    #If found records with outlier values
    if ((length(outliers>0))){

      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }

  }
  return(ordinals)
}


# ************************************************
# NplotOutliers() :
# Scatter plot of field values and colours outliers in red
# INPUT: Vector - sorted    -  points to plot as literal values
#        Vector - outliers  - list of above points that are considered outliers
#        String - fieldName - name of field to plot
# OUTPUT : None
# ************************************************
NplotOutliers<-function(sorted,outliers,fieldName){

  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}


# ************************************************
# NPREPROCESSING_discreteNumeric() :
# Test NUMERIC field if DISCRETE or ORDINAL
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discrete (1-10)
# OUTPUT : vector strings - Updated with types per field {DISCRETE, ORDINAL}
# ************************************************
# Plots histogram for visulisation
# ************************************************
NPREPROCESSING_discreteNumeric<-function(dataset,field_types,cutoff){

  #For every field in our dataset
  for(field in 1:(ncol(dataset))){

    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {

      #191020NRT use R hist() function to create 10 bins
      histogramAnalysis<-hist(dataset[,field], breaks = 10, plot=FALSE)
      bins<-histogramAnalysis$counts/length(dataset[,field])*100  # Convert to %

      graphTitle<-"AUTO:"

      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discrete value

      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCRETE
      else
        field_types[field]<-TYPE_ORDINAL

      #Type of field is the chart name
      hist(dataset[,field], breaks = 10, plot=TRUE,
           main=paste(graphTitle,field_types[field]),
           xlab=names(dataset[field]),ylab="Number of Records",
           yaxs="i",xaxs="i",border = NA)

    } #endif numeric types
  } #endof for
  return(field_types)
}

# ************************************************
# NPREPROCESSING_initialFieldType() :
# Test each field for NUMERIC or SYNBOLIC
# INPUT: Data Frame - dataset - data
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
NPREPROCESSING_initialFieldType<-function(dataset){

  field_types<-vector()
  for(field in 1:(ncol(dataset))){

    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }

    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}

# ************************************************
# NreadDataset() :
# Read a CSV file from working directory
# INPUT: string - csvFilename - CSV filename
# OUTPUT : data frame - contents of the headed CSV file
# ************************************************
NreadDataset<-function(csvFilename){
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))

  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# ************************************************
# NPREPROCESSING_removePunctuation()
# INPUT: String - fieldName - name of field
# OUTPUT : String - name of field with punctuation removed
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}

# ************************************************
# NPREPROCESSING_prettyDataset()
# Output simple dataset field analysis results as a table in "Viewer"
# REQUIRES: formattable
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
# OUTPUT : none
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
NPREPROCESSING_prettyDataset<-function(dataset,...){

  params <- list(...)

  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)

  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }

  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }

  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}
                          
                          
                          
                          
# ************************************************
# NPLOT_correlagram() :
# Plots PLOT_correlagram
# INPUT: data frame - cr - n x n frame of correlation coefficients
# OUTPUT : None
# 221019 - plot absolute values only
# ************************************************
NPLOT_correlagram<-function(cr){

  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))

  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)

  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n")
}

# ************************************************
# NPREPROCESSING_redundantFields() :
# Determine if an entire field is redundant
# Uses LINEAR correlation,
# so use with care as information will be lost
# INPUT: data frame - dataset - numeric values only
#        double     - cutoff  - Value above which is determined redundant [0,1]
# OUTPUT : Frame - dataset with any fields removed
# ************************************************

NPREPROCESSING_redundantFields<-function(dataset,cutoff){

  print(paste("Before redundancy check Fields=",ncol(dataset)))

  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1

  if (length(xx)>0L)
    dataset<-dataset[,-xx]

  #Kendall is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  #cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  NPLOT_correlagram(cr)

  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]

  if (nrow(list_fields_correlated)>0){

    print("Following fields are correlated")
    print(list_fields_correlated)

    # 240220nrt print list of correlated fields as namesß
    for (i in 1:nrow(list_fields_correlated)){
      print(paste(names(dataset)[list_fields_correlated[i,1]],"~", names(dataset)[list_fields_correlated[i,2]]))
    }

    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])

    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}
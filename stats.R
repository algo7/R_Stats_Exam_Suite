#!/usr/bin/env Rscript
rm(list=ls())
library(ggplot2);
library(ggpubr);
library(digest);
library(argparser);
library(cli);
library(utils);
library(stats);
library(openintro);
library(ggfortify);


#import the data from the file browser
# data <- read.csv(file.choose(),stringsAsFactors=FALSE);

# Welcome Message
welcomeMsg<-'Hi Welcome to the Stats Terminal'
cli::cat_boxx(welcomeMsg)

# Topic 1
# Main Menu List
menuListT1<-c(
  'Indicators (Location: Avg.,Median,Mode | Variability: Stdev,IQR,Range,CV)',
  'Relationship',
  'Histogram (Continous Data)',
  'Histogram (Custom Data: No Percentage, Only Numeric Format)',
  'Bar (Categorical Data)',
  'Frequency Table',
  'Frequency Table with Custom Breaks',
  'Back'
);

# Main Menu Selection Function
topicI<-function(){
  choice<-menu(menuListT1,title='What do you need?')
  switch (choice,
    '1' = {indicators();topicI()},
    '2' = relationship(),
    '3' = {histo();topicI()},
    '4' = {histoCustom();topicI()},
    '5' = {bar();topicI()},
    '6' = {freqTableM();topicI()},
    '7' = {CustomfreqTableM();topicI()},
    '8' = topicSelect()
  )
}

# Location Indicators Menu List
subMenu<-c(
 'User Input',
 'File',
 'File Summary',
 'Back'
)

# Location Indicators Selection Function
indicators<-function(){
  choice<-menu(subMenu,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = {cat(indicatorsCal(toInt(inpSplit('Enter Values Separated by a Comma: ')),'user'));cat('\n')},
          '2' = {cat(indicatorsCal(read.csv(file.choose(),stringsAsFactors=FALSE),'csv'));cat('\n')},
          '3' = {cat(summary(read.csv(file.choose(),stringsAsFactors=FALSE)),sep='\n');cat('\n')},
          '4' = topicI()
  )
}

# Stats Basic Func.
indicatorsCal<-function(input,type){
 if(identical(type,'user')){
   u <- sd(input,na.rm = TRUE)
   v <- max(input,na.rm = TRUE) - min(input,na.rm = TRUE)
   m <- min(input,na.rm = TRUE)
   mx <- max(input,na.rm = TRUE)
   r <- IQR(input,na.rm = TRUE)
   w <- mean(input, na.rm = TRUE)
   x <- median(input, na.rm = TRUE)
   y <- getmode(input)
   cv <- u/w
   z <- c('Mean:',w,'Median:',x,'Mode:',y,'Stdev:',u,'IQR:',r,'Range:',v,'Min:',m,'Max:',mx,'CV:',cv)
   return(z);
 }else{
   u <- sd(unlist(input),na.rm = TRUE)
   v <- max(unlist(input),na.rm = TRUE) - min(input,na.rm = TRUE)
   m <- min(unlist(input),na.rm = TRUE)
   mx <- max(unlist(input),na.rm = TRUE)
   r <- IQR(unlist(input),na.rm = TRUE)
   w <- mean(unlist(input),na.rm = TRUE)
   x <- median(unlist(input),na.rm = TRUE)
   y <- getmode(unlist(input))
   cv <- u/w
   z <- c('Mean:',w,'Median:',x,'Mode:',y,'Stdev:',u,'IQR:',r,'Range:',v,'Min:',m,'Max:',mx,'CV:',cv)
   return (z)
 }
}

# Relationship Menu List
subMenu1<-c(
  'User Input',
  'File',
  'Back'
)

# Relationship Menu Selection Function
relationship<-function(){
  choice<-menu(subMenu1,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = {cat(relationshipCal(toInt(inpSplit('2-Enter Values Separated by a Comma: ')),toInt(inpSplit('1-Enter Values Separated by a Comma: ')),'user'));cat('\n');relationship()},
          '2' = {cat(relationshipCal(read.csv(file.choose(),stringsAsFactors=FALSE),'x','csv'));cat('\n');relationship()},
          '3' = topicI()
  )
}

# Relationship var. cal. func.
relationshipCal<-function(input,input1,type){
  if(identical(type,'user')){
    covar <- cov(input,input1)
    correl <- cor(input,input1)
    z <- c('Covariance:',covar,'Correlation:',correl)
    return(z)
  }else{
    covar <- cov(input[,1],input[,2])
    correl <- cor(input[,1],input[,2])
    z <- c('Covariance:',covar,'Correlation:',correl)
    return(z)
  }
}

# Histogram Function
histo<-function(){
  x <- read.csv(file.choose())
  cn <-colnames(x)
  un <- unlist(x)
  hn<-hist(un, breaks = 'Sturges',plot = FALSE)
  plot(hn,xlim=c(0,hn$breaks[length(hn$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,hn$counts[1]+round(sum(hn$counts)/length(hn$counts))),xlab = cn,main = paste('Graph of',cn),col = 'blue',border='red',labels = TRUE)
  h<-hist(un, breaks = 'Sturges',plot = FALSE)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE, xlab = cn,ylab='RF',xlim=c(0,h$breaks[length(h$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,100),main = paste('Graph of',cn,'percentage version'),col = 'red',border='blue',labels = TRUE)
}

histoCustom<-function(){
  x <- read.csv(file.choose())
  minVal<-as.vector(summary(x))[1]
  cli_alert_warning(paste('Watch Out for the Minimum Value',minVal))
  cn <-colnames(x)
  un <- unlist(x)
  info<-toInt(inpSplit('Enter Interval (Start,End,Step) in CSV: '))
  br<-seq(info[1],info[2],by=info[3])
  hn<-hist(un[un>info[1]&un<info[2]], breaks =br ,plot = FALSE)
  plot(hn,xlim=c(hn$breaks[1],hn$breaks[length(hn$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,max(hn$counts)+round(sum(hn$counts)/length(hn$counts))),xlab = cn,main = paste('Graph of',cn),col = 'blue',border='red',labels = TRUE)
  h<-hist(un[un>info[1]&un<info[2]], breaks = br,plot = FALSE)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE, xlab = cn,ylab='RF',xlim=c(h$breaks[1],h$breaks[length(h$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,max(h$density)+round(sum(h$density)/length(h$density))),main = paste('Graph of',cn,'percentage version'),col = 'red',border='blue',labels = TRUE)

}

# Barchart Function
bar<-function(){
  x <- read.csv(file.choose())
  df<-data.frame(x[!is.na(x)])
  lvs <- levels(sort(unlist(x)))
  count <- character()
  for (variable in lvs) {
    filter<-df[colnames(df)[1]]==variable
    count<-c(count,length(df[filter,]))
  }
  xs <- data.frame('Levels'=lvs,'Count'=as.integer(count))
  p <- ggplot(xs, aes(x=Levels, y=Count))+geom_bar(stat='identity')+
    geom_text(aes(label=Count), vjust=-0.3, size=3.5)
  print(p)
}

# FreqTable Menu List
subMenuF<-c(
  'Right Open',
  'Right Closed',
  'Back'
)

# Frequency Table Selection Function
freqTableM<-function(){
  choice<-menu(subMenuF,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = freqTable(FALSE),
          '2' = freqTable(TRUE),
          '3' = topicI()
  )
}
# Frequency Table
freqTable<-function(openSide){
  raw<-read.csv(file.choose())
  summary(raw)
  raw<-data.frame(raw[!is.na(raw)])
  unlistraw<-unlist(raw)
  hist <- hist(unlist(raw),breaks="Sturges", plot=FALSE,include.lowest=TRUE,right=openSide)
  br=hist$breaks
  cf = cbind(cumsum(table(cut(unlistraw,br,right=openSide))))
  rf = cbind(table(cut(unlistraw,br,right=openSide)) /nrow(raw))
  crf = cbind(cumsum(table(cut(unlistraw,br,right=openSide))))/nrow(raw)
  df<-data.frame(bin=rownames(crf),AbsFreq_ni=hist$count, CumuFreq=cf, RelativeFreq_fi=rf, Cumu_RelativeFreq_Fi=crf)
  rownames(df)<-NULL
  print(df)
}

# Custom FreqTable Menu List
subMenuFC<-c(
  'Right Open',
  'Right Closed',
  'Right Open (UpperLimitOnly)',
  'Right Closed (UpperLimitOnly)',
  'Back'
)

# Custom Frequency Table Selection Function
CustomfreqTableM<-function(){
  choice<-menu(subMenuFC,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = CustomfreqTable(FALSE,TRUE), #(openSide,include.lowest)
          '2' = CustomfreqTable(TRUE,TRUE),
          '3' = CustomfreqTable(FALSE,FALSE),
          '4' = CustomfreqTable(TRUE,FALSE),
          '5' = topicI()
  )
}
# Custom Frequency Table
CustomfreqTable<-function(openSide,lowest){
  raw<-read.csv(file.choose())
  minVal<-as.vector(summary(raw))[1]
  cli_alert_warning(paste('Watch Out for the Minimum Value',minVal))
  raw<-data.frame(raw[!is.na(raw)])
  info<-toInt(inpSplit('Enter Interval (Start,End,Step) in CSV: '))
  unlistraw<-unlist(raw)
  unlistraw<-unlistraw[unlistraw>info[1]&unlistraw<info[2]]
  br<-seq(info[1],info[2],by=info[3])
  hist <- hist(unlistraw,breaks=br, plot=FALSE,include.lowest=lowest,right=openSide)
  cf = cbind(cumsum(table(cut(unlistraw,br,right=openSide))))
  rf = cbind(table(cut(unlistraw,br,right=openSide)) /nrow(raw))
  crf = cbind(cumsum(table(cut(unlistraw,br,right=openSide))))/nrow(raw)
  df<-data.frame(bin=rownames(crf),AbsFreq_ni=hist$count, CumuFreq=cf, RelativeFreq_fi=rf, Cumu_RelativeFreq_Fi=crf)
  rownames(df)<-NULL
  print(df)
}

# Topic II
menuListT2<-c(
  'Probability Table',
  'Back'
);

# Main Menu Selection Function
topicII<-function(){
  choice<-menu(menuListT2,title='What do you need?')
  switch (choice,
          '1' = {probTable();probTable()},
          '2' = topicSelect(),
  )
}

probTable<-function(){
  # Import the file
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex,header = TRUE)
  df<-data.frame(x)
  # Row/col names
  rownames(df)<-df[,1]
  df[-c(1),-c(1)]
  df<-df[,-1]

  # Vertical Sum
  verticalTotal<-integer()
  for (coln in colnames(df)) {
    verticalTotal<-c(verticalTotal,sum(df[coln]))
  }
  # Update the df
  df<-rbind(df,verticalTotal)
  # Horizontal Sum
  horizontalTotal<-integer()
  for (rown in rownames(df)) {
    horizontalTotal<-c(horizontalTotal,sum(df[rown,]))
  }
  # Update the df
  df<-cbind(df,horizontalTotal)
  # Get the grand total
  ro<-length(rownames(df))
  co<-length(colnames(df))
  grandSum<-df[ro,co]
  # Calculate the percentage table
  per<-data.frame(df/grandSum)
  # Replace the column names
  colnames(df)[co]<-'Total'
  colnames(per)[co]<-'Total'
  # The Table with the sum
  print(df)
  cat('\n')
  # The percentage table
  print(per)
  cat('\n')
  # Remove the Total column
  x<-per[length(rownames(per)),]
  x[,length(colnames(x))]<-NULL
  # Determin the event type
  i<-0
  filter<-character()
  for (cols in x) {
    i<-i+1
    # filter<-c(filter,cols*per[,length(colnames(per))]==per[,i])
    x<-cols*per[,length(colnames(per))]
    y<-per[,i]
    filter<-c(filter,as.double(x)==as.double(y))
  }

  roo<-length(rownames(per))
  coo<-length(colnames(per))-1
  eventTable<-matrix(filter,nrow=roo,ncol =coo)
  cat('Event Table: (True = Independent | False = Dependent)',sep = '\n')
  cat('The Last Column will Always Equal True Because it is the Sum',sep = '\n')
  cat('Note:If there are more than or equal to 2 trues then all is true regardless of the display',sep = '\n')
  print(eventTable)
}

# Topic III
menuListT3<-c(
  'Probability Table & Graph (No Percentage Format %) & Expected Val ...etc',
  'Back'
);

# Main Menu Selection Function
topicIII<-function(){
  choice<-menu(menuListT3,title='What do you need?')
  switch (choice,
          '1' = {discreteProbDistro();topicIII()},
          '2' = topicSelect(),
  )
}

discreteProbDistro<-function(){
  # Import the file
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex,header = TRUE)
  df<-data.frame(x)
  # Get the total
  total<-sum(df[,2])
  # Calculate the percentage table
  df<-cbind(df,data.frame('per.'=df[,2]/total))
  print(df)
  cat('\n')
  # Expected val.
  print('Expected Value: ')
  expV<-crossprod(df[,1],df[,length(colnames(df))])[1]
  print(expV)
  cat('\n')
  # Variance
  print('Variance: ')
  variance<-sum(((df[,1]-expV)^2)*df[,length(colnames(df))])
  print(variance)
  cat('\n')
  # Stdev
  print('Standard Deviation: ')
  print(sqrt(variance))

  # Plot
  x=df[,1]
  y=df[,2]
  # Determine the breaks
  hist<-hist(unlist(df),breaks = 'Sturges',plot=FALSE)$breaks
  interval<-hist[length(hist)]-hist[length(hist)-1]
  xmax<-interval+hist[length(hist)]
  ymax<-max(df[length(colnames(df))])*2
  # The plot object
  plot(x,y,ylab='Probability',main=colnames(df)[1],type='h',col='blue',xlim = c(0,xmax),ylim=c(0,ymax))+
  points(x,y,pch=16,cex=1,col="dark red")+text(x,y,labels=y,pos=3)

}

# Topic IV
menuListT4<-c(
  'Normal Distribution Graph',
  'Normal Distribution Calculation',
  'Uniform Distribution Calculation',
  'Standardized Distribution Calculation',
  'Back'
);

# Main Menu Selection Function
topicIV<-function(){
  choice<-menu(menuListT4,title='What do you need?')
  switch (choice,
          '1' = {normalDist();topicIV()},
          '2' = {normalDistCal(FALSE);topicIV()},
          '3' = {uniformDistCal();topicIV()},
          '4' = {standardizeDistCal();topicIV()},
          '5' = topicSelect(),
  )
}

normalDist<-function(){
  stdev<-toInt(readline(prompt='Enter the standard deviation: '))
  avg<-toInt(readline(prompt='Enter the mean: '))
  sample<-toInt(inpSplit('Enter Sample Info. (Start,End,Step) in CSV: '))
  p<-ggdistribution(dnorm,seq(sample[1],sample[2],by=sample[3]),mean=avg,sd=stdev)
  print(p)
}

normalDistCal<-function(sampling){
  if(identical(sampling,TRUE)){
    cli_alert_warning('The stdev of the sample and the stdev of the population is differet')
    cli_alert_warning('However, the average will be the same')
  }
    type<-readline(prompt='P[X ≤ x] (default) or P[X > x] (>) or val2<x<val1 (bt) or prob->val (p): ')

  if(identical(type,'>')){
    if(identical(sampling,TRUE)){
      info<-toInt(inpSplit('Enter (Value,Expected Value,Stderr) in CSV: '))
    }else{
      info<-toInt(inpSplit('Enter (Value,Mean,Stdev) in CSV: '))
    }
    p<-pnorm(info[1],info[2],info[3],lower.tail = FALSE)
    print('The Probability is: ')
    print(p)
    cat('\n')
  }else if(identical(type,'')){
    if(identical(sampling,TRUE)){
      info<-toInt(inpSplit('Enter (Value,Expected Value,Stderr) in CSV: '))
    }else{
      info<-toInt(inpSplit('Enter (Value,Mean,Stdev) in CSV: '))
    }
    p<-pnorm(info[1],info[2],info[3])
    print('The Probability is: ')
    print(p)
    cat('\n')
  }else if(identical(type,'p')){
    if(identical(sampling,TRUE)){
      info<-toInt(inpSplit('Enter (Prbability,Expected Value,Stderr) in CSV: '))
    }else{
      info<-toInt(inpSplit('Enter (Prbability,Mean,Stdev) in CSV: '))
    }
    val<-qnorm(info[1],info[2],info[3])
    print('The Value is: ')
    print(val)
    cat('\n')
  }else{
    if(identical(sampling,TRUE)){
      info<-toInt(inpSplit('Enter (Smaller Value, Larger Value,Expected Value,Stderr) in CSV: '))
    }else{
      info<-toInt(inpSplit('Enter (Smaller Value, Larger Value,Mean,Stdev) in CSV: '))
    }
    p1<-pnorm(info[1],info[3],info[4])
    p2<-pnorm(info[2],info[3],info[4])
    p3<-p2-p1
    print('The Probability is: ')
    print(p3)
    cat('\n')
 }
}

uniformDistCal<-function(){
  type<-readline(prompt='P[X ≤ x] (default) or P[X > x] (>) or val2<x<val1 (bt) or prob->val (p): ')
  if(identical(type,'>')){
    info<-toInt(inpSplit('Enter (Value,Min,Max) in CSV: '))
    p<-punif(info[1],info[2],info[3],lower.tail = FALSE)
    print('The Probability is: ')
    print(p)
    cat('\n')
  }else if(identical(type,'')){
    info<-toInt(inpSplit('Enter (Value,Min,Max) in CSV: '))
    p<-punif(info[1],info[2],info[3])
    print('The Probability is: ')
    print(p)
    cat('\n')
  }else if(identical(type,'p')){
    info<-toInt(inpSplit('Enter (Prbability,Min,Max) in CSV: '))
    val<-qunif(info[1],info[2],info[3])
    print('The Value is: ')
    print(val)
    cat('\n')
  }else{
    info<-toInt(inpSplit('Enter (Smaller Value, Larger Value,Min,Max) in CSV: '))
    p1<-punif(info[1],info[3],info[4])
    p2<-punif(info[2],info[3],info[4])
    p3<-p2-p1
    print('The Probability is: ')
    print(p3)
    cat('\n')
  }
}

standardizeDistCal<-function(){

  type<-readline(prompt='P[X ≤ x] (default) or P[X > x] (>) or val2<x<val1 (bt) or prob->val (p): ')
  if(identical(type,'>')){
    info<-toInt(readline(prompt='Enter the Value: '))
    p<-pnorm(info,lower.tail = FALSE)
    print('The Probability is: ')
    print(p)
    cat('\n')
  }else if(identical(type,'')){
    info<-toInt(readline(prompt='Enter the Value: '))
    p<-pnorm(info)
    print('The Probability is: ')
    print(p)
    cat('\n')
  }else if(identical(type,'p')){
    info<-toInt(readline(prompt='Enter the Probability: '))
    val<-qnorm(info)
    print('The Value is: ')
    print(val)
    cat('\n')
  }else{
    info<-toInt(inpSplit('Enter (Smaller Value, Larger Value) in CSV: '))
    p1<-pnorm(info[1])
    p2<-pnorm(info[2])
    p3<-p2-p1
    print('The Probability is: ')
    print(p3)
    cat('\n')
  }
}

# Topic V
menuListT5<-c(
  'Standard Error Calculation (Numerical)',
  'Standard Error Calculation (Categorical)',
  'Sampling Distribution Calculation (Numerical)',
  'Sampling Proportion Calculation (Categorical)',
  'Back'
);

# Main Menu Selection Function
topicV<-function(){
  cli_alert_info('Central Limit Theorem (CLT): ')
  cli_alert_info('Sample Size > 30, the Sample = Normally Distributed')
  cat('\n')
  choice<-menu(menuListT5,title='What do you need?')
  switch (choice,
          '1' = {standardErrNumeric();topicV()},
          '2' = {standardErrCategorical();topicV()},
          '3' = {normalDistCal(TRUE);topicV()},
          '4' = {normalDistCal(TRUE);topicV()},
          '5' = topicSelect(),
  )
}

standardErrNumeric<-function(){
  popStdev<-toInt(readline(prompt='Enter the Population Stdev: '))
  popSize<-toInt(readline(prompt='Enter the Population Size: '))
  print(paste('Standard Error is',popStdev/sqrt(popSize)))
  cat('\n')
}


standardErrCategorical<-function(){
  cli_alert_info('Sampling Proportion: ')
  cli_alert_info('Sample Size > 30 & Each Category >5')
  cli_alert_info('The Sample = Normally Distributed')
  expVal<-toInt(readline(prompt='Enter the Expected Value: '))
  size<-toInt(readline(prompt='Enter the Sample Size: '))
  print(paste('Standard Error (Categorical) is',sqrt(expVal*(1-expVal)/size) ))
  cat('\n')
}

# Topic VI
menuListT6<-c(
  'Confidence Interval Known Sigma Normal Distribution',
  'Standard Error Calculation (Categorical)',
  'Sampling Distribution Calculation (Numerical)',
  'Sampling Proportion Calculation (Categorical)',
  'Back'
);

# Main Menu Selection Function
topicVI<-function(){
  choice<-menu(menuListT6,title='What do you need?')
  switch (choice,
          '1' = {confIntSigKnown();topicVI()},
          '2' = {confIntSigUnKnown();topicVI()},
          '3' = {normalDistCal(TRUE);topicVI()},
          '4' = {normalDistCal(TRUE);topicVI()},
          '5' = topicSelect(),
  )
}

# Confidence Level with Known Sigma (stdev/sigma giving)
confIntSigKnown<-function(){
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex,header = TRUE)
  df<-data.frame(x)
  # Calculate the sample size and the average | get the sigma
  sig<-toInt(readline(prompt='Enter the Sigma (Given Stdev): '))
  sampleSize<-length(df[,1])
  avg<-mean(df[,1])
  # The confidence level
  cl<-toInt(readline(prompt='Enter the Confidence Level (usually 95%): '))
  sl<-1-cl
  # The z value (margin of error)
  z<-qnorm(cl+sl/2)
  # The standard error
  stderr<-sig/sqrt(sampleSize)
  # Error margin
  em<-stderr*z
  # Precision
  pres<-em/avg
  # Lower Limit
  ll<-avg-em
  # Upper Limit
  ul<-avg+em
  #The result
  cat('\n')
  cli_alert_success('The Results:')
  cat('\n')
  result<-c(paste('Sigma:',sig),paste('Sample Size:',sampleSize)
                 ,paste('Avg:',avg),paste('Confidence Level:',cl)
                 ,paste('Significance Level:',sl),paste('Z-value:',z),
                 paste('Standard Error:',stderr),paste('Error Margin:',em),
                 paste('Precision:',pres),paste('Lower Limit:',ll),
                 paste('Upper Limit:',ul))
  print(result)
  cat('\n')
  # Warning abt analysis
  cli_alert_warning('The analysis should at least include the point of estimate (avg),')
  cli_alert_warning('the margin of error, the confidence level, or the lower&upper limit')
  cat('\n')
}

# Confidence Level with Unknown Sigma (using stdev from the sample)
confIntSigUnKnown<-function(){

}
# Misc.:

# Split input func
inpSplit<-function(text){
  result<- strsplit(readline(prompt=text),",")
  return(result)
}

# Convert to integer func
toInt<-function(list){
  for (variable in list) {
    int<-as.numeric(variable)
    return(int)
  }
}

# Get mode func
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Topic Selection
topicSelect=function(){
  menuList<-c(
    'Descriptive Statisitc',
    'Probabilty',
    'Discrete Variables',
    'Continuous Variables',
    'Sampling Distributions',
    'Confidence Interval',
    'Hypothesis Testing',
    'Simple Linear Regression',
    'Multiple Linear Regression',
    'Chi2 Test'
  );

  choice<-menu(menuList, title='Please Select A Topic:');
  # Menu Selection Function
  mSelect<-function(topic){
    switch (topic,
            '1' = topicI(),
            '2' = topicII(),
            '3' = topicIII(),
            '4' = topicIV(),
            '5' = topicV(),
            '6' = topicVI()
    )
  };
  mSelect(choice);

}
topicSelect()

# # Take in arguments
# args <- commandArgs(trailingOnly = TRUE);
# if (length(args) < 1){
#   stop('Missing Arguments\n')
# }

# menu(args[1])
# p <- arg_parser('Stats Terminal')
# # Add a positional argument
# p <- add_argument(p, 'name', help='Name of the person to sort')
# p <- add_argument(p, 'namxe', help='Name of the person to sort')
# p <- add_argument(p, 'namse', help='Name of the person to sort')
# # Add a flag
# p <- add_argument(p, '--debug', help='enable debug mode', flag=TRUE)
# # Add another flag
# p <- add_argument(p, '--short', help='output only the house', flag=TRUE)
# argv <- parse_args(p)



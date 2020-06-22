#!/usr/bin/env Rscript
# rm(list=ls())
library(ggplot2);
library(ggpubr);
library(digest);
library(argparser);
library(cli);
library(utils);
library(stats);
library(openintro);
library(ggfortify);
library(PEIP);
library(corrplot);
library(ggiraphExtra)



#import the data from the file browser
# data <- read.csv(file.choose(),stringsAsFactors=FALSE);

# Welcome Message
welcomeMsg<-'Hi Welcome to the Stats Terminal by Aviv'
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
  plot(hn,xlim=c(hn$breaks[1]-((hn$breaks[2]-hn$breaks[1]))*1.5,hn$breaks[length(hn$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,max(hn$counts)+round(sum(hn$counts)/length(hn$counts))),xlab = cn,main = paste('Graph of',cn),col = 'blue',border='red',labels = TRUE)
  h<-hist(un, breaks = 'Sturges',plot = FALSE)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE, xlab = cn,ylab='RF',xlim=c(hn$breaks[1]-((hn$breaks[2]-hn$breaks[1]))*1.5,h$breaks[length(h$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,max(h$density)+round(sum(h$density)/length(h$density))),main = paste('Graph of',cn,'percentage version'),col = 'red',border='blue',labels = TRUE)
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
  plot(hn,xlim=c(hn$breaks[1]-((hn$breaks[2]-hn$breaks[1]))*1.5,hn$breaks[length(hn$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,max(hn$counts)+round(sum(hn$counts)/length(hn$counts))),xlab = cn,main = paste('Graph of',cn),col = 'blue',border='red',labels = TRUE)
  h<-hist(un[un>info[1]&un<info[2]], breaks = br,plot = FALSE)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE, xlab = cn,ylab='RF',xlim=c(hn$breaks[1]-((hn$breaks[2]-hn$breaks[1]))*1.5,h$breaks[length(h$breaks)]+(hn$breaks[2]-hn$breaks[1])),ylim=c(0,max(h$density)+round(sum(h$density)/length(h$density))),main = paste('Graph of',cn,'percentage version'),col = 'red',border='blue',labels = TRUE)

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
          '1' = {probTable();topicII()},
          '2' = topicSelect(),
  )
}

probTable<-function(){
  # Import the file
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex)
  df<-data.frame(x)
  # Row/col names
  rownames(df)<-df[,1]
  df[-c(1),-c(1)]
  df<-df[,-1]

  # Vertical Sum
  verticalTotal<-numeric()
  for (col in colnames(df)) {
    verticalTotal<-c(verticalTotal,sum(df[col]))
  }
  # Update the df
  df<-rbind(df,'Total'=verticalTotal)
  # Horizontal Sum
  horizontalTotal<-numeric()
  for (rown in rownames(df)) {
    horizontalTotal<-c(horizontalTotal,sum(df[rown,]))
  }
  # Update the df
  df<-cbind(df,'Total'=horizontalTotal)
  # Get the grand total
  ro<-length(rownames(df))
  co<-length(colnames(df))
  grandSum<-df[ro,co]
  # Calculate the percentage table
  per<-data.frame(df/grandSum)
  # The Table with the sum
  cli_alert_info('Sum Table:')
  print(df)
  cat('\n')
  # The percentage table
  cli_alert_info('Percentage Table:')
  print(per)
  cat('\n')
  # Conditional Probability
  cli_alert_info('Conditional Probability:')
  condProb<-df[!(rownames(df)=='Total'),]
  condProb<-condProb/condProb[,'Total']
  condProb<-condProb[,!(colnames(df)=='Total')]
  print(condProb)
  cat('\n')
  # Degree of Freedom
  degf<-(length(colnames(condProb))-1) * (length(rownames(condProb))-1)
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
  cli_alert_info('Event Table: (True = Independent | False = Dependent)')
  cli_alert_info('The Last Column will Always Equal True Because it is the Sum')
  cli_alert_info('Note:If there are more than or equal to 2 trues then all is true regardless of the display')
  print(eventTable)
  # Barplot
  barplotMenu<-c('True','False')
  choice<-menu(barplotMenu,title='Bar Plot? ')
  if(identical(choice,1L)){
    condPM<-t(as.matrix(condProb))
    bp<-barplot(condPM,ylab = 'Percentage',
                ylim=c(0,max(condPM*3))
                ,legend=c(rownames(condPM))
                ,col=c("red","skyblue"),
                args.legend=list(x='topright',bty="n",border=NA))
    text(bp,condPM[2,]+condPM[1,],labels=paste(round(condPM[2,],5),'%'))
    text(bp,condPM[2,],labels=paste(round(condPM[1,],5),'%'))
  }
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
  'Confidence Interval Known Sigma: Normal Distribution (Numerical)',
  'Confidence Interval Unknown Sigma: T Distribution (Numerical)',
  'Confidence Interval Proportion (Categorical)',
  'Back'
);

# Main Menu Selection Function
topicVI<-function(){
  choice<-menu(menuListT6,title='What do you need?')
  switch (choice,
          '1' = {confIntSigKnown();topicVI()},
          '2' = {confIntSigUnKnown();topicVI()},
          '3' = {confIntProportion();topicVI()},
          '4' = topicSelect(),
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
  # The z value (the critical value)
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
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex,header = TRUE)
  df<-data.frame(x)
  # Calculate the sample size and the average | calculate the sigma
  sig<-sd(df[,1])
  sampleSize<-length(df[,1])
  avg<-mean(df[,1])
  # The confidence level
  cl<-toInt(readline(prompt='Enter the Confidence Level (usually 95%): '))
  sl<-1-cl
  # The degree of freedom
  degf<-sampleSize-1
  # The t value (the critical value)
  t<-tinv(cl+sl/2,degf)
  # The standard error
  stderr<-sig/sqrt(sampleSize)
  # Error margin
  em<-stderr*t
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
            ,paste('Significance Level:',sl),paste('Degree of Freedom:',degf)
            ,paste('T-value:',t),paste('Standard Error:',stderr),paste('Error Margin:',em),
            paste('Precision:',pres),paste('Lower Limit:',ll),paste('Upper Limit:',ul))
  print(result)
  cat('\n')
  # Warning abt analysis
  cli_alert_warning('The analysis should at least include the point of estimate (avg),')
  cli_alert_warning('the margin of error, the confidence level, or the lower&upper limit')
  cat('\n')
}

# Confidence Level with Proportion
confIntProportion<-function(){
  filex<-file.choose()
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex,header = TRUE)
  df<-data.frame(x)
  # Calculate the sample size
  sampleSize<-length(df[,1])
  # Get the proportion
  pp<-toInt(readline(prompt='Enter the Proportion: '))
  # The average p_bar
  avg<- pp/sampleSize
  # 1- p_bar
  navg<-1-avg
  # The confidence level
  cl<-toInt(readline(prompt='Enter the Confidence Level (usually 95%): '))
  sl<-1-cl
  # The z value (the critical value)
  z<-qnorm(cl+sl/2)
  # The standard error
  stderr<-sqrt(avg*navg/sampleSize)
  # Error margin
  em<-stderr*z
  # Precision remains in percentage as all are in percentage already
  pres<-em
  # Lower Limit
  ll<-avg-em
  # Upper Limit
  ul<-avg+em
  #The result
  cat('\n')
  cli_alert_success('The Results:')
  cat('\n')
  result<-c(paste('Sample Size:',sampleSize),paste('Proportion:',pp)
            ,paste('Avg:',avg),paste('1-Avg:',navg),paste('Confidence Level:',cl)
            ,paste('Significance Level:',sl),paste('Z-value:',z)
            ,paste('Standard Error:',stderr),paste('Error Margin:',em),
            paste('Precision:',pres),paste('Lower Limit:',ll),paste('Upper Limit:',ul))
  print(result)
  cat('\n')
  # Warning abt analysis
  cli_alert_warning('The analysis should at least include the point of estimate (avg),')
  cli_alert_warning('the margin of error, the confidence level, or the lower&upper limit')
  cat('\n')
}


# Topic VII
menuListT7<-c(
  'Testing with Known Sigma: Normal Distribution (Numerical)',
  'Testing with Unknown Sigma: T Distribution (Numerical)',
  'Testing Proportion (Categorical)',
  'Back'
);

# Main Menu Selection Function
topicVII<-function(){
  choice<-menu(menuListT7,title='What do you need?')
  switch (choice,
          '1' = {testSigKnown();topicVII()},
          '2' = {testSigUnKnown();topicVII()},
          '3' = {testProportion();topicVII()},
          '4' = topicSelect(),
  )
}

testSigKnown<-function(){
  # Step 0: Compute the sample size
  sampleSize<-toInt(readline(prompt='Enter the Sample Size: '))
  sampleAvg<-toInt(readline(prompt='Enter the Sample Average: '))
  popStdev<-toInt(readline(prompt='Enter the Population Standard Deviation: '))
  testVal<-toInt(readline(prompt='Enter the Value to be Tested: '))
  # Step 1: Formulate the hypithesis
  H1<-readline(prompt='H1-Enter Your Hypothesis: ')
  H0<-readline(prompt='H0-Enter the Original Hypothesis: ')
  # Step 2: Conditions of Validity
  cli_alert_info('Sample size > 30 and known sigma(stdev): Normal Distro.')
  cli_alert_info('Left Tail:  H0-> P=P0 | H1-> P<P0')
  cli_alert_info('Right Tail: H0-> P=P0 | H1-> P>P0')
  cli_alert_info('Two Tail: H0-> P=P0 | H1-> P!=P0')
  cat('\n')
  # Test Menu
  testType<-function(){
    Test<-character()
    testMenu<-c(
      'Left Tail',
      'Right Tail',
      'Two Tail'
    );
    choice<-menu(testMenu,title='Select Test Type: ')
    switch (choice,
            '1' = Test<-'Left Tail',
            '2' = Test<-'Right Tail',
            '3' = Test<-'Two Tail'
    )
  }
  testType<-testType()
  # Step 3: Computation
  stderr<-popStdev/sqrt(sampleSize)
  # The test statistic (standardized)
  z_cal<-(sampleAvg-testVal)/stderr
  # Cumulative probability of z_cal
  if(identical(testType,'Two Tail')){
    p_val<-pnorm(z_cal)*2
  }else if(identical(testType,'Right Tail')){
    p_val<-pnorm(z_cal,lower.tail = FALSE)
  }else if(identical(testType,'Left Tail')){
    p_val<-pnorm(z_cal)
  }
  # The Significance Level (Alpha)
  sl<-toInt(readline(prompt='Enter the Significance Level: '))
  # The critical value
  if(identical(testType,'Two Tail')){
    z_crit<-qnorm(1-sl/2)
  }else if(identical(testType,'Right Tail')){
    z_crit<-qnorm(sl,lower.tail = FALSE)
  } else if(identical(testType,'Left Tail')){
    z_crit<-qnorm(sl)
  }

  # General Info
  ginfo<-c(paste('Sample Size:',sampleSize),paste('Average:',sampleAvg)
            ,paste('Population Stdev:',popStdev),paste('Test Value:',testVal),
            paste('H0:',H0),paste('H1:',H1),paste('Test Type:',testType)
            ,paste('Standard Error:',stderr),paste('z_cal:',z_cal),
            paste('p_val:',p_val),paste('Significance Level:',sl),paste('z_crit:',z_crit))
  print(ginfo)
  # Step 4: Decision
  if(identical(testType,'Left Tail')){
    if(z_cal>z_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(z_cal<z_crit){
      cli_alert_success(paste('Accept H1:',H1))
      cli_alert_danger(paste('Reject H0:',H0))
    }

   }
  if(identical(testType,'Right Tail')){
    if(z_cal<z_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(z_cal>z_crit){
     cli_alert_success(paste('Accept H1:',H1))
     cli_alert_danger(paste('Reject H0:',H0))
    }

 }
  if(identical(testType,'Two Tail')){
   if(abs(z_cal)<z_crit){
     cli_alert_success(paste('Accept H0:',H0))
     cli_alert_danger(paste('Reject H1:',H1))
   }else if(abs(z_cal)>z_crit){
     cli_alert_success(paste('Accept H1:',H1))
     cli_alert_danger(paste('Reject H0:',H0))
   }
  }
}

testSigUnKnown<-function(){
  # Step 0: Compute the sample size
  sampleSize<-toInt(readline(prompt='Enter the Sample Size: '))
  sampleAvg<-toInt(readline(prompt='Enter the Sample Average: '))
  sampleStdev<-toInt(readline(prompt='Enter the Sample Standard Deviation: '))
  testVal<-toInt(readline(prompt='Enter the Value to be Tested: '))
  # Step 1: Formulate the hypithesis
  H1<-readline(prompt='H1-Enter Your Hypothesis: ')
  H0<-readline(prompt='H0-Enter the Original Hypothesis: ')
  # Step 2: Conditions of Validity
  cli_alert_info('Sample size > 30 and known sigma(stdev): Normal Distro.')
  cli_alert_info('Left Tail:  H0-> P=P0 | H1-> P<P0')
  cli_alert_info('Right Tail: H0-> P=P0 | H1-> P>P0')
  cli_alert_info('Two Tail: H0-> P=P0 | H1-> P!=P0')
  cat('\n')
  # Test Menu
  testType<-function(){
    Test<-character()
    testMenu<-c(
      'Left Tail',
      'Right Tail',
      'Two Tail'
    );
    choice<-menu(testMenu,title='Select Test Type: ')
    switch (choice,
            '1' = Test<-'Left Tail',
            '2' = Test<-'Right Tail',
            '3' = Test<-'Two Tail'
    )
  }
  testType<-testType()
  # Step 3: Computation
  stderr<-sampleStdev/sqrt(sampleSize)
  # Degree of Freedom
  degf<-sampleSize-1
  # The test statistic (standardized)
  t_cal<-(sampleAvg-testVal)/stderr
  # Cumulative probability of t_cal
  if(identical(testType,'Two Tail')){
    p_val<-pt(t_cal,degf)*2
  }else if(identical(testType,'Right Tail')){
    p_val<-pt(t_cal,degf,lower.tail = FALSE)
  }else if(identical(testType,'Left Tail')){
    p_val<-pt(t_cal,degf)
  }
  # The Significance Level (Alpha)
  sl<-toInt(readline(prompt='Enter the Significance Level: '))
  # The critical value
  if(identical(testType,'Two Tail')){
    t_crit<-pt(1-sl/2,degf)
  }else if(identical(testType,'Right Tail')){
   t_crit <-pt(sl,degf,lower.tail = FALSE)
  }else if(identical(testType,'Left Tail')){
    t_crit<-pt(sl,degf)
  }

  # General Info
  ginfo<-c(paste('Sample Size:',sampleSize),paste('Average:',sampleAvg)
           ,paste('Population Stdev:',popStdev),paste('Test Value:',testVal),
           paste('H0:',H0),paste('H1:',H1),paste('Test Type:',testType)
           ,paste('Standard Error:',stderr),paste('t_cal:',t_cal),
           paste('p_val:',p_val),paste('Significance Level:',sl),paste('t_crit:',t_crit))
  print(ginfo)
  # Step 4: Decision
  if(identical(testType,'Left Tail')){
    if(t_cal>t_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(t_cal<t_crit){
      cli_alert_success(paste('Accept H1:',H1))
      cli_alert_danger(paste('Reject H0:',H0))
    }

  }
  if(identical(testType,'Right Tail')){
    if(t_cal<t_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(t_cal>t_crit){
      cli_alert_success(paste('Accept H1:',H1))
      cli_alert_danger(paste('Reject H0:',H0))
    }

  }
  if(identical(testType,'Two Tail')){
    if(abs(t_cal)<t_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(abs(t_cal)>t_crit){
      cli_alert_success(paste('Accept H1:',H1))
      cli_alert_danger(paste('Reject H0:',H0))
    }
  }
}

testProportion<-function(){
  # Step 0: Compute the sample size
  sampleSize<-toInt(readline(prompt='Enter the Sample Size: '))
  sampleProp<-toInt(readline(prompt='Enter the Sample Proportion %: '))
  testVal<-toInt(readline(prompt='Enter the Value to be Tested %: '))
  # Step 1: Formulate the hypithesis
  H1<-readline(prompt='H1-Enter Your Hypothesis: ')
  H0<-readline(prompt='H0-Enter the Original Hypothesis: ')
  # Step 2: Conditions of Validity
  cli_alert_info('Sample size > 30 and either case > 5: Normal Distro.')
  cli_alert_info('Left Tail:  H0-> P=P0 | H1-> P<P0')
  cli_alert_info('Right Tail: H0-> P=P0 | H1-> P>P0')
  cli_alert_info('Two Tail: H0-> P=P0 | H1-> P!=P0')
  cat('\n')
  # Test Menu
  testType<-function(){
    Test<-character()
    testMenu<-c(
      'Left Tail',
      'Right Tail',
      'Two Tail'
    );
    choice<-menu(testMenu,title='Select Test Type: ')
    switch (choice,
            '1' = Test<-'Left Tail',
            '2' = Test<-'Right Tail',
            '3' = Test<-'Two Tail'
    )
  }
  testType<-testType()
  # Step 3: Computation
  samp<-readline(prompt='Use Sample Proportion [y/n]?: ')
  if(identical(samp,'y')){
    avg<- sampleProp/sampleSize
    navg<-1-avg
    stderr<-(avg*navg)/sampleSize
  }else if(identical(samp,'n')){
    stderr<-sqrt((testVal*(1-testVal)/sampleSize))
  }

  # The test statistic (standardized)
  z_cal<-(sampleProp-testVal)/stderr
  # Cumulative probability of z_cal
  if(identical(testType,'Two Tail')){
    p_val<-pnorm(z_cal)*2
  }else if(identical(testType,'Right Tail')){
    p_val<-pnorm(z_cal,lower.tail = FALSE)
  }else if(identical(testType,'Left Tail')){
    p_val<-pnorm(z_cal)
  }
  # The Significance Level (Alpha)
  sl<-toInt(readline(prompt='Enter the Significance Level: '))
  # The critical value
  if(identical(testType,'Two Tail')){
    z_crit<-qnorm(1-sl/2)
  }else if(identical(testType,'Right Tail')){
    z_crit<-qnorm(sl,lower.tail = FALSE)
  } else if(identical(testType,'Left Tail')){
    z_crit<-qnorm(sl)
  }

  # General Info
  ginfo<-c(paste('Sample Size:',sampleSize),paste('Sample Proportion:',sampleProp)
          ,paste('Test Value:',testVal),paste('H0:',H0),paste('H1:',H1),paste('Test Type:',testType),
          paste('Stderr Use Sample Propotion:',samp),paste('Standard Error:',stderr),paste('z_cal:',z_cal),
           paste('p_val:',p_val),paste('Significance Level:',sl),paste('z_crit:',z_crit))
  print(ginfo)
  # Step 4: Decision
  if(identical(testType,'Left Tail')){
    if(z_cal>z_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(z_cal<z_crit){
      cli_alert_success(paste('Accept H1:',H1))
      cli_alert_danger(paste('Reject H0:',H0))
    }

  }
  if(identical(testType,'Right Tail')){
    if(z_cal<z_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(z_cal>z_crit){
      cli_alert_success(paste('Accept H1:',H1))
      cli_alert_danger(paste('Reject H0:',H0))
    }

  }
  if(identical(testType,'Two Tail')){
    if(abs(z_cal)<z_crit){
      cli_alert_success(paste('Accept H0:',H0))
      cli_alert_danger(paste('Reject H1:',H1))
    }else if(abs(z_cal)>z_crit){
      cli_alert_success(paste('Accept H1:',H1))
      cli_alert_danger(paste('Reject H0:',H0))
    }
  }
}

# Topic VIII
menuListT8<-c(
  'Simple Regression',
  'Back'
);

# Main Menu Selection Function
topicVIII<-function(){
  choice<-menu(menuListT8,title='What do you need?')
  switch (choice,
          '1' = {simpRegress();topicVIII()},
          '2' = topicSelect(),
  )
}

simpRegress<-function(){
  x<-read.csv(file.choose())
  df<-data.frame(x)
  col1<-colnames(df)[1]
  col2<-colnames(df)[2]
  print(paste('1:',col1,' 2:',col2))
  H0<-'There is no relationship & the slope = 0'
  H1<-'There is a relationship'
  # the Formula
  # Formula Selection Function
  regFormSelect<-function(){
    formlula<-character()
    regFormMenu<-c(
      opt1<-paste(col1,'=','a + b *',col2),
      opt2<-paste(col2,'=','a + b *',col1)
    );
    choice<-menu(regFormMenu,title='Select Relationship Type: ')
    switch (choice,
            '1' = formlula<-c(opt1,col1,col2),
            '2' = formlula<-c(opt2,col2,col1)
    )
  }
  formula<-regFormSelect()
  # The regression formula
  formulaF<-as.formula(paste(formula[2],formula[3],sep = '~'))
  # Generate the model
  lmod<-lm(formulaF,df)
  # The summary
  slmod<-summary(lmod)
  # The coefficients
  slmodc<-slmod$coefficients
  # The p-value
  p_val<-slmodc[formula[3],'Pr(>|t|)']
  #ANOVA
  anovaT<-anova(lmod)
  # The significance Level
  sl<-0.005
  # The final formulas
  textForm<-paste(formula[2],'=',formula[3],'*',slmodc[formula[3],'Estimate'],'+',slmodc['(Intercept)','Estimate'])
  varForm<-paste(formula[2],'=',slmodc[formula[3],'Estimate'],'x','+',slmodc['(Intercept)','Estimate'])
  # Prediction Function
  predictFunc<-function(){
    preVal<-toInt(readline(prompt='Enter the Value to be Predicted: '))
    return(preVal*slmodc[formula[3],'Estimate']+slmodc['(Intercept)','Estimate'])
  }
  # The predicted value
  predicted_val<-lmod$fitted.values
  # The residula value
  residual_val<-residuals(lmod)
  # The standard errs
  st_res_val<-residual_val/sd(residual_val) #using the formula
  st_res_val_rs<-rstandard(lmod) #using the rstandard function
  # Bind all of them in to a data frame
  finaldf<-data.frame(cbind(predicted_val,residual_val,st_res_val,st_res_val_rs))
  # The results
  # Scatter Plot
  print(ggPredict(lmod,se=TRUE,interactive = TRUE))
  cli_alert_success('The Result: ')
  cat('\n')
  if(sl>p_val){
    cli_alert_info('Hypothesis: ')
    cli_alert_success(paste('Accept H1',H1))
    cli_alert_danger(paste('Reject H0',H0))
  }else{
    cli_alert_info('Hypothesis: ')
    cli_alert_success(paste('Accept H0',H0))
    cli_alert_danger(paste('Reject H1',H1))
  }
  cat('\n')
  cli_alert_info('Formulas: ')
  print(paste('Formula (text):',textForm))
  print(paste('Formula (variable):',varForm))
  cat('\n')
  cli_alert_info('Summary: ')
  print(slmod)
  cat('\n')
  print(anovaT)
  cat('\n')
  cli_alert_info('Table: ')
  print(finaldf)
  cat('\n')
  predictResult<-predictFunc()
  cli_alert_success(paste('Prediction:', predictResult))
  cat('\n')
}

# Topic VIX
menuListT9<-c(
  'Summary Function (Descriptive Stats. Info.)',
  'Multiple Regression',
  'Back'
);

# Main Menu Selection Function
topicVIX<-function(){
  choice<-menu(menuListT9,title='What do you need?')
  switch (choice,
          '1' = {summaryM();topicVIX()},
          '2' = {multiRegress();topicVIX()},
          '3' = topicSelect(),
  )
}

# Summary Menu List
summaryMenu<-c(
  'Right Open',
  'Right Closed',
  'Right Open (UpperLimitOnly)',
  'Right Closed (UpperLimitOnly)',
  'Back'
)

# Summary  Selection Function
summaryM<-function(){
  choice<-menu(summaryMenu,title='Method? Remember the Col. Name is necessary in the CSV files)')
  switch (choice,
          '1' = summaryFunc(FALSE,TRUE), #(openSide,include.lowest)
          '2' = summaryFunc(TRUE,TRUE),
          '3' = summaryFunc(FALSE,FALSE),
          '4' = summaryFunc(TRUE,FALSE),
          '5' = topicVIX()
  )
}

# Summary Function
summaryFunc<-function(openSide,lowest){
  raw<-read.csv(file.choose(),header=TRUE)
  cli_alert_info(paste('The Summary:'))
  cat('\n')
  print(summary(raw))
  cat('\n')
  craw<-raw
  cli_alert_info(paste('The Correlation Table in %:'))
  cat('\n')
  craw.cor<-cor(craw)
  print(craw.cor*100)
  corrplot(craw.cor)
  cat('\n')
  # Loop through the columns
  for (col in colnames(craw)) {
    raw<-data.frame(craw[,col])
    raw<-data.frame(raw[!is.na(raw)])
    un<-unlist(raw)
    # Frequency Table
    hist <- hist(un,breaks="Sturges", plot=FALSE,include.lowest=lowest,right=openSide)
    br=hist$breaks
    cf = cbind(cumsum(table(cut(un,br,right=openSide,include.lowest=lowest))))
    rf = cbind(table(cut(un,br,right=openSide,include.lowest=lowest)) /nrow(raw))
    crf = cbind(cumsum(table(cut(un,br,right=openSide,include.lowest=lowest))))/nrow(raw)
    df<-data.frame(bin=rownames(crf),AbsFreq_ni=hist$count, CumuFreq=cf, RelativeFreq_fi=rf, Cumu_RelativeFreq_Fi=crf)
    rownames(df)<-NULL
    cli_alert_info(paste('Frequency Table of',col))
    cat('\n')
    print(df)
    cat('\n')
    # Graph
    cn <-col
    hn<-hist(un, breaks = 'Sturges',plot = FALSE,include.lowest=lowest,right=openSide)
    # Frequency
    plot(hn,xlim=c(hn$breaks[1]-((hn$breaks[2]-hn$breaks[1]))*1.5,hn$breaks[length(hn$breaks)]+(hn$breaks[2]-hn$breaks[1]))
         ,ylim=c(0,max(hn$counts)+round(sum(hn$counts)/length(hn$counts)))
         ,xlab = cn,main = paste('Graph of',cn),col = 'blue',border='red',labels = TRUE)
    # Density
    h<-hist(un, breaks = 'Sturges',plot = FALSE,include.lowest=lowest,right=openSide)
    h$density = h$counts/sum(h$counts)*100
    plot(h,freq=FALSE, xlab = cn,ylab='RF'
         ,xlim=c(hn$breaks[1]-((hn$breaks[2]-hn$breaks[1]))*1.5,h$breaks[length(h$breaks)]+(hn$breaks[2]-hn$breaks[1]))
         ,ylim=c(0,max(h$density)+round(sum(h$density)/length(h$density)))
         ,main = paste('Graph of',cn,'percentage version'),col = 'red',border='blue',labels = TRUE)
  }
}

# Multiple Regression
multiRegress<-function(){
  cli_alert_warning('Format Excel Data in the Genral Format and Include the Header')
  x<-read.csv(file.choose())
  df<-data.frame(x)
  colN<-character()
  regFormMenu<-character()
  for (col in 1:length(colnames(df))) {
    colN<-c(colN,colnames(df)[col])
    regFormMenu<-c(regFormMenu,paste(colnames(df)[col],'=','b1x1 + b2x2 +...bnxn'))
  }
  colNDF<-data.frame(colN)
  colnames(colNDF)<-NULL
  print(colNDF)
  H0<-'There is no relationship & the slope = 0'
  H1<-'There is a relationship'
  # Formula Selection
  choice<-menu(regFormMenu,title='Select Relationship Type: ')
  # The choice as the dependent variable
  depdVar<-colnames(df)[choice]
  # The the rest as independent variables
  indepVar<-character()
  for (col in colnames(df)) {
    if(!identical(col,depdVar)){
      indepVar<-c(indepVar,col)
    }
  }
  # The regression formula
  formula<-as.formula(paste(depdVar,paste(indepVar,collapse='+'),sep='~'))
  # Generate the model
  lmod<-lm(formula,df)
  # The summary
  slmod<-summary(lmod)
  # The coefficients
  slmodc<-slmod$coefficients
  # The p-value
  p_vals<-slmodc[,'Pr(>|t|)']
  p_vals<-data.frame(p_vals)
  print(p_vals)
  anovaT<-anova(lmod)
  # Scatter Plot
  # scatPlot<-ggPredict(lmod,se=TRUE,interactive = TRUE)
  # Optimization
  optimizationMenu<-c('True','False')
  choice<-menu(optimizationMenu,title='Optimize (Remove Larest P-value Except for the Intercept)? ')
  if(identical(choice,1L)){
    filterx<-rownames(p_vals)=='(Intercept)'#Filter out the p val. of the intercept
    p_vals<-p_vals[-filterx,,drop=FALSE]
    p_vals<-p_vals[p_vals<max(p_vals),,drop=FALSE] #Drop the largest p val
    newRow<-rownames(p_vals)
    filtery<-indepVar%in%newRow
    indepVar<-indepVar[filtery]
    formula<-as.formula(paste(depdVar,paste(indepVar,collapse='+'),sep='~'))
    lmod<-lm(formula,df)
    # The summary
    slmod<-summary(lmod)
    # The coefficients
    slmodc<-slmod$coefficients
    # The p-value
    p_vals<-slmodc[,'Pr(>|t|)']
    p_vals<-data.frame(p_vals)
    anovaT<-anova(lmod)
    # scatPlot<-ggPredict(lmod,se=TRUE,interactive = TRUE)
  }
  # The significance Level
  sl<-0.005
  # Assembling the final formula
  dispFor<-character()
  for (row in rownames(slmodc)) {
    if(!identical(row,'(Intercept)')){
      dispFor<-c( dispFor,paste(row,slmodc[row,'Estimate'],sep='*'))
    }
  }
  # The final formula
  textForm<-paste(depdVar,'=',slmodc['(Intercept)','Estimate'],'+',paste(dispFor,collapse = ' + '))
  partial1<-paste(depdVar,'=',slmodc['(Intercept)','Estimate']+slmodc[indepVar[length(indepVar)],'Estimate']*1,'+',paste(dispFor[-length(dispFor)],collapse = ' + '))
  partial0<-paste(depdVar,'=',slmodc['(Intercept)','Estimate']+slmodc[indepVar[length(indepVar)],'Estimate']*0,'+',paste(dispFor[-length(dispFor)],collapse = ' + '))
  optimizationMenu<-c('True','False')
  partialChoice<-menu(optimizationMenu,title='Partial Formula?? ')
  # Prediction Function
  predictFunc<-function(){
    info<-toInt(inpSplit(paste('Enter Values in CSV in Order: ','[',paste(indepVar,collapse = ' -> '),']:')))
    i<-0
    presum<-numeric()
    for (val in info) {
      i<-i+1
      presum<-c(presum,val*(slmodc[indepVar[i],'Estimate']))
    }
    return(sum(presum)+slmodc['(Intercept)','Estimate'])
  }
  # The predicted value
  predicted_val<-lmod$fitted.values
  # The residula value
  residual_val<-residuals(lmod)
  # The standard errs
  st_res_val<-residual_val/sd(residual_val) #using the formula
  st_res_val_rs<-rstandard(lmod) #using the rstandard function
  # Bind all of them in to a data frame
  finaldf<-data.frame(cbind(predicted_val,residual_val,st_res_val,st_res_val_rs))
  # The results
  cli_alert_success('The Result: ')
  # print(scatPlot)
  cat('\n')
  # Check the Hypothesis
  i<-0
  for (p_v in p_vals[indepVar,]) {
    i<-i+1
    if(sl>p_v){
      cli_alert_info('Hypothesis: ')
      cli_alert_success(paste('Accept H1',H1,'With:',rownames(p_vals[indepVar,,drop=FALSE])[i]))
      cli_alert_danger(paste('Reject H0',H0,'With:',rownames(p_vals[indepVar,,drop=FALSE])[i]))
    }else{
      cli_alert_info('Hypothesis: ')
      cli_alert_success(paste('Accept H0',H0,'With:',rownames(p_vals[indepVar,,drop=FALSE])[i]))
      cli_alert_danger(paste('Reject H1',H1,'With:',rownames(p_vals[indepVar,,drop=FALSE])[i]))
    }
  }
  cat('\n')
  cli_alert_info('Formulas: ')
  print(paste('Formula (Full):',textForm))
  if(identical(partialChoice,1L)){
    print(paste('Partial Formula (0):',partial0))
    print(paste('Partial Formula (1):',partial1))
  }
  cat('\n')
  cli_alert_info('Summary: ')
  print(slmod)
  cat('\n')
  print(anovaT)
  cat('\n')
  cli_alert_info('Table: ')
  print(finaldf)
  cat('\n')
  predictResult<-predictFunc()
  cli_alert_warning('Remeber the Units')
  cli_alert_success(paste('Prediction:', predictResult))
  cat('\n')
}

# Topic X
menuListT10<-c(
  'Prob Table',
  'Chi2 Test',
  'Back'
);

# Main Menu Selection Function
topicX<-function(){
  choice<-menu(menuListT10,title='What do you need?')
  switch (choice,
          '1' = {probTable();topicX()},
          '2' = {chi2Test();topicX()},
          '3' = topicSelect()
  )
}

chi2Test<-function(){
  # Import the file
  filex<-file.choose()
  H0<-'Are Two Independent Variable'
  H1<-'Are TWo Dependent Vairbale'
  # Fix newline problem
  cat("\n", file = filex, append = TRUE)
  x<-read.csv(file=filex)
  df<-data.frame(x)
  # Row/col names
  rownames(df)<-df[,1]
  df[-c(1),-c(1)]
  df<-df[,-1]
  # Vertical Sum
  verticalTotal<-numeric()
  for (col in colnames(df)) {
    verticalTotal<-c(verticalTotal,sum(df[col]))
  }
  # Update the df
  df<-rbind(df,'Total'=verticalTotal)
  # Horizontal Sum
  horizontalTotal<-numeric()
  for (rown in rownames(df)) {
    horizontalTotal<-c(horizontalTotal,sum(df[rown,]))
  }
  # Update the df
  df<-cbind(df,'Total'=horizontalTotal)
  # Get the grand total
  ro<-length(rownames(df))
  co<-length(colnames(df))
  grandSum<-df[ro,co]
  # Calculate the percentage table
  per<-data.frame(df/grandSum)
  # The Table with the sum
  cli_alert_info('Sum Table:')
  print(df)
  cat('\n')
  # The percentage table
  cli_alert_info('Percentage Table:')
  print(per)
  cat('\n')
  # Conditional Probability
  cli_alert_info('Conditional Probability:')
  condProb<-df[!(rownames(df)=='Total'),]
  condProb<-condProb/condProb[,'Total']
  condProb<-condProb[,!(colnames(df)=='Total')]
  print(condProb)
  cat('\n')
  # Degree of Freedom
  cli_alert_info('Degree of Freedom:')
  degf<-(length(colnames(condProb))-1) * (length(rownames(condProb))-1)
  print(degf)
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
  cli_alert_info('Event Table: (True = Independent | False = Dependent)')
  cli_alert_info('The Last Column will Always Equal True Because it is the Sum')
  cli_alert_info('Note:If there are more than or equal to 2 trues then all is true regardless of the display')
  print(eventTable)
  # Barplot
  barplotMenu<-c('True','False')
  choice<-menu(barplotMenu,title='Bar Plot? ')
  if(identical(choice,1L)){
    condPM<-t(as.matrix(condProb))
    bp<-barplot(condPM,ylab = 'Percentage',
                ylim=c(0,max(condPM*3))
                ,legend=c(rownames(condPM))
                ,col=c("red","skyblue"),
                args.legend=list(x='topright',bty="n",border=NA))
    text(bp,condPM[2,]+condPM[1,],labels=paste(round(condPM[2,],5),'%'))
    text(bp,condPM[2,],labels=paste(round(condPM[1,],5),'%'))
  }
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
            '6' = topicVI(),
            '7' = topicVII(),
            '8' = topicVIII(),
            '9' = topicVIX(),
            '10' = topicX()
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



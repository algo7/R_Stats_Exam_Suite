# multiRegress<-function(){
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
  # Optimization
  optimizationMenu<-c('True','False')
  choice<-menu(optimizationMenu,title='Optimize (Remove Largest P-value)? ')
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
     print(p_vals)
  }
  print(p_vals)

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
  cli_alert_info('Table: ')
  print(finaldf)
  cat('\n')
  predictResult<-predictFunc()
  cli_alert_success(paste('Prediction:', predictResult))
  cat('\n')
# }





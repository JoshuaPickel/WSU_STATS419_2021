


columnVariances = function(x, numeric = FALSE){
  if (class(x) != 'data.frame'){
    stop("Please provide a data frame")
  }
  else
  {
    cols = c()
    vals = c()
  }
    if(numeric == FALSE)
    {
      x.numeric = unlist(lapply(x,is.numeric))
      x = x[,x.numeric]
    }
    
    for(i in colnames(x))
    {
      
      variance = var(x[i])
      cols = c(cols,i)
      vals = as.numeric(c(vals,variance))
      
    }
    res = data.frame(t(vals))
    names(res) = cols
    row.names(res) = "Variance"
     return(res)
}

columnSD = function(x, numeric = FALSE)
{
  if (class(x) != 'data.frame')
  {
    stop("Please provide a data frame")
  }
  else
  {
  variance = columnVariances(x, numeric)
  SD = sqrt(variance)
  row.names(SD) = "SD"
  }
  return(SD)
}


columnMode = function(x, numeric = FALSE){
  if (class(x) != 'data.frame')
  {
    stop("Please provide a data frame")
  }
  else
  {
    cols = c()
    vals = c()
  }
    if(numeric == FALSE)
    {
      x.numeric = unlist(lapply(x,is.numeric))
      x = x[,x.numeric]
      
    }
   for (i in colnames(x))
   {
     df = as.data.frame(as.matrix(table(x[i])))
     index = which.max(df$V1)
     mode = row.names(df)[index]
  
     cols = c(cols,i)
     vals = c(vals,mode)

   }
    res = data.frame(t(vals))
    names(res) = cols
    row.names(res) = "Mode"
    return(res)
}

columnMax = function(x, numeric = FALSE)
{
  if (class(x) != 'data.frame')
  {
    stop("Please provide a data frame")
  }
  else
  {
    cols = c()
    vals = c()
  }
  if(numeric == FALSE)
  {
    x.numeric = unlist(lapply(x,is.numeric))
    x = x[,x.numeric]
    
  }
  for (i in colnames(x))
  {
   
    max = max(x[i])
    cols = c(cols,i)
    vals = c(vals,max)
    
  }
  res = data.frame(t(vals))
  names(res) = cols
  row.names(res) = "Max"
  return(res)
}

columnMin = function(x, numeric = FALSE)
{
  if (class(x) != 'data.frame')
  {
    stop("Please provide a data frame")
  }
  else
  {
    cols = c()
    vals = c()
  }
  if(numeric == FALSE)
  {
    x.numeric = unlist(lapply(x,is.numeric))
    x = x[,x.numeric]
    
  }
  for (i in colnames(x))
  {
    
    max = min(x[i])
    cols = c(cols,i)
    vals = c(vals,max)
    
  }
  res = data.frame(t(vals))
  names(res) = cols
  row.names(res) = "Min"
  return(res)
}

colMean = function(x, numeric = FALSE)
{
  if(numeric == FALSE)
  {
    x.numeric = unlist(lapply(x,is.numeric))
    x = x[,x.numeric]
  }
  Mean = t(as.data.frame(as.matrix(colMeans(x))))
  row.names(Mean) = "Mean"
  return(Mean)
}


colMedian = function(x, numeric = FALSE)
{
  cols = c()
  vals = c()
  if(numeric == FALSE)
  {
    x.numeric = unlist(lapply(x,is.numeric))
    x = x[,x.numeric]
  }
  for(i in colnames(x))
  {
    Median = median(x[,i])
    cols = c(cols,i)
    vals = c(vals,Median)
    
  }
  res = data.frame(t(vals))
  names(res) = cols
  row.names(res) = "Median"
  return(res)
}

doSummary = function(x, numeric = FALSE)
{
  Length = length(x)
  naNum = sum(is.na(x))
  variance = columnVariances(x, numeric)
  sd = columnSD(x, numeric)
  mode = columnMode(x, numeric)
  Max = columnMax(x, numeric)
  Min = columnMin(x, numeric)
  Mean = colMean(x,numeric)
  Median = colMedian(x,numeric)
  Quartiles = colQuartiles(x,numeric)
  res = rbind(variance,sd, Max, Min, mode, Mean, Median, Quartiles)
  # res = t(res)
  return(res)

}

colQuartiles = function(x, numeric = FALSE)
{
  if(numeric == FALSE)
  {
    x.numeric = unlist(lapply(x,is.numeric))
    x = x[,x.numeric]
  }
  cols = c()
  Q1 = c()
  Q3 = c()
  N = nrow(x)
  idx.Q1 = N*1/2;
  idx.Q3 = N*3/4
  for (i in colnames(x))
  {
    sorted = sort(x[,i])
    Q1Val = sorted[idx.Q1]
    Q3Val = sorted[idx.Q3]
    cols = c(cols,i)
    Q1 = c(Q1, Q1Val)
    Q3 = c(Q3, Q3Val)
    
  }
  res = data.frame(Q1,Q3)
  row.names(res) = cols
  res = t(res)
  return(res)
  
}



NormalizeDataFrame = function(x,numeric = FALSE)
  # Used formula from https://www.datanovia.com/en/blog/how-to-normalize-and-standardize-data-in-r-for-great-heatmap-visualization/
  {
  if (class(x) != 'data.frame')
  {
    stop("Please provide a data frame")
  }
  if (numeric == FALSE)
  {
    x.numeric = unlist(lapply(x,is.numeric))
    x = x[,x.numeric]
  }
  for (i in colnames(x))
  {
    x[i] = (x[i] - min(x[i]))/(max(x[i]-min(x[i])))
  }
  return (x)
}
  













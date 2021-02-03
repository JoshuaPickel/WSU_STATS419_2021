

handShake = function(n=1, plotMe=FALSE)
{
  if (n<=0 || n > 40)
  {
    print("Not a valid number, number of people must be greater than 0, and less than 41 ")
  }
  else
  {
    number_of_handShakes = n*(n-1)/2
    number_of_handShakes
    
    if (plotMe == TRUE)
    {
      people = c()
      
      for (i in 1:n)
      {
        node = paste0("Person",i)
        people = c(people,node)
      }
      
      allPeople = combn(people,2)
      graph = graph( edges = allPeople, directed = T)
      plot(graph,edge.arrow.mode = 3, edge.arrow.size = .25,vertex.size = 25,vertex.color = "grey",
           vertex.frame.color = "black", edge.color = rainbow(number_of_handShakes), layout = layout_in_circle )
    }
    sprintf("The number of handshakes between %i people is %i", n, number_of_handShakes)
  }

}



Determinant = function(matrix){
  if(class(matrix)[1]!= "matrix" || nrow(matrix) != 3 || ncol(matrix) != 3) 
  {
    print("You did not give the function a matrix, or the matrix was not a 3x3,  try again")
  }
  else
  {
    dets = c()
    for (i in 1:ncol(matrix) )
    {
      twoBytwo = matrix[-1,-i]
      detTwo = (twoBytwo[1,1]*twoBytwo[2,2]) - (twoBytwo[1,2] * twoBytwo[2,1])
      dets = c(dets,detTwo)
    }
    products = matrix[1,] * dets
    determinant = products[1] - products[2] + products[3]
  }
  return(determinant)
}


count_letters = function(file)
{
  original = paste(readLines(file),collapse = "")
  original = gsub(" ", "", original)
  original = tolower(original)
  original = gsub("[^A-Za-z]", "!", original)
  original = strsplit(original, "")
  res = as.data.frame(t(as.matrix(table(original))))
  colnames(res)[colnames(res) =='!'] = "OTHER"
  if (length(res) > 1)
  {
    res = res[,c(2:length(res),1)]
  }
  title = paste("Letters In", file)
  barplot(as.table(as.matrix(res)), xlab = 'Letter',ylab = 'Frequency', main = title)
  res
}


handShake = function(n=1, plotMe=FALSE)
{
  library(igraph)
  # Check for valid whole number, and range
  if (class(n) != 'numeric' || n<=0 || n > 40 || floor(n) != n)
  {
    # Stop program with message
    stop("Not a valid number, number of people must be a whole number greater than 0, and less than 41 ")
  }
  
  else
  {
    # Calculate number of handshakes
    number_of_handShakes = n*(n-1)/2
    
    #If user wants plot
    if (plotMe == TRUE)
    {
      # Empty list to hold vertex labels
      people = c()
      
      # Loop through number of people
      for (i in 1:n)
      {
        # Make a label
        node = paste0("Person",i)
        
        # Add label to person list
        people = c(people,node)
      }
      # See if only one person was entered
      if(n == 1)
      {
      # Make graph with one vertex
      graph = make_empty_graph(1)
      
      # Plot the one vertex graph
      plot(graph, vertex.label = "person1", vertex.size = 40,vertex.color = "grey", vertex.frame.color = "black")
      }
      # If more than one person
      else
      {
        # allPeople = combn(people,2)
        # print(allPeople)
        allPeople = makeCombos(people)
        
        # Make graph for all handshakes
        graph = graph( edges = allPeople)
        
        #Plot the graph
        plot(graph,edge.arrow.mode = 3, edge.arrow.size = .25, vertex.size = 50,vertex.color = "grey",
             vertex.frame.color = "black", edge.color = rainbow(number_of_handShakes), layout = layout_in_circle, main = paste("Number of Handsakes Between ", number_of_handShakes, "People") )
      }
     
    }
    # Print the results
    print(sprintf("The number of handshakes between %i people is %i", n, number_of_handShakes))
    
  }
  # Return the results
  return(number_of_handShakes)
}



computeDeterminant = function(matrix){
  # Make sure a 3x3 matrix was passed
  if(class(matrix)[1]!= "matrix" || nrow(matrix) != 3 || ncol(matrix) != 3) 
  {
    # Stop program with error message
    stop("You did not give the function a matrix, or the matrix was not a 3x3,  try again")
  }
  # If a 3x3 matrix was passed
  else
  {
    # Make empty list to hold 2x2 determiant results
    dets = c()
    
    # Loop through each column in 3x3
    for (i in 1:ncol(matrix) )
    {
      # Get the the 2x2, by ignoring the row 1 and the col the loop is on
      twoBytwo = matrix[-1,-i]
      
      # Calculate the 2x2 determinant
      detTwo = (twoBytwo[1,1]*twoBytwo[2,2]) - (twoBytwo[1,2] * twoBytwo[2,1])
      
      # Add the result to det list
      dets = c(dets,detTwo)
    }
    # Get the product of first row values * 2x2 determinant values
    products = matrix[1,] * dets
    
    # Calculate the determinant using product
    determinant = products[1] - products[2] + products[3]
  }
  # Return the determinant of the 3x3
  return(determinant)
}


count_letters = function(file, plotMe = FALSE)
{
  if (file_test("-f", file) == TRUE)
    {

    # Read the file and collapse the lines
    original = paste(readLines(file),collapse = "")
    
    # Remove the spaces from the collapsed letters
    original = gsub(" ", "", original)
    
    # Convert to all lowercase
    original = tolower(original)
    
    # Replace and non-alpha characters to "!"
    original = gsub("[^A-Za-z]", "!", original)
    
    # Split the string into individual letters
    original = strsplit(original, "")
  
    # Put the results in a data frame, by making a matrix from a table of the individual letters
    res = as.data.frame(t(as.matrix(table(original))))
    
    # Replace the "!" column name with "OTHER"
    colnames(res)[colnames(res) =='!'] = "OTHER"
    
    # If there is more than one letter in the file
  if (length(res) > 1)
    {
    # Swap the order of the columns so "OTHER" appears at the end
    res = res[,c(2:length(res),1)]
    }
    # If user wants a plot
  if (plotMe == TRUE)
    {
    # Make a dynamic title for the graph
     title = paste("Letters In", file)
  
     # Make a bar plot for the results
     barplot(as.table(as.matrix(res)), xlab = 'Letter',ylab = 'Frequency', main = title, ylim = c(0,1200))
     }
  # Return the data frame
  return (res)
  }
  else
  {
    stop("The file name or path provided does not exist.")
  }
}


  makeCombos = function(x)
    {
    # Make empty list to hold combinations
  combos = c()
  
  # Loop through all but last element in list
  for(i in 1:(length(x)-1))
  {
   # Offset loop to make combos w/o repeats
    for(e in (i+1):length(x))
      {
      # Add combo to list
      combos = c(combos,c(x[i],x[e]))
    }
    # convert to matrix
    matrix = matrix(combos,nrow = 2)
    
  }
  # Return matrix representation of combos
  return (matrix)
}
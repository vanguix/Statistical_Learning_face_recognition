our_knn_multiple = function(data, tests, target, friends = 3, threshold= 40, metric= 'Euclidean') {
  predictions = list()
  
  for (i in 1:nrow(tests)) {
    test = tests[i, , drop = FALSE]
    aux = rbind(data, test)
    distances = as.matrix(dist(aux,method= metric))
    ndata = nrow(data)
    distances = distances[ndata + 1, 1:ndata]
    
    if (min(distances)>threshold) {
      predicted_class= 'Not in the dataset'
    }
    else{
      ind = sort(distances, index.return = TRUE)
      idx = ind$ix[1:friends]
      neighbors = data[idx,]
      neighbors_class = target[idx]
      tb = table(neighbors_class)
      predicted_class = names(tb)[which.max(tb)]
    }
  
    predictions[[i]] = predicted_class
  }
  
  return(predictions)
}

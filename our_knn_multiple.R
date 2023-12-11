our_knn_multiple = function(data, tests, target, friends = 3) {
  predictions = list()
  
  for (i in 1:nrow(tests)) {
    test = tests[i, , drop = FALSE]
    aux = rbind(data, test)
    distances = as.matrix(dist(aux))
    ndata = nrow(data)
    distances = distances[ndata + 1, 1:ndata]
    ind = sort(distances, index.return = TRUE)
    idx = ind$ix[1:friends]
    neighbors = data[idx,]
    neighbors_class = target[idx]
    tb = table(neighbors_class)
    predicted_class = names(tb)[which.max(tb)]
    ans = list(predicted = predicted_class, indices = idx)
    predictions[[i]] = ans
  }
  
  return(predictions)
}

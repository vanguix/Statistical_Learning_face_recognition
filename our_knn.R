our_knn = function(data, test, target, friends=3){
  aux = rbind(data, test)
  distances = as.matrix(dist(aux))
  ndata = nrow(data)
  distances = distances[ndata+1, 1:ndata]
  ind = sort(distances, index.return=T)
  idx = ind$ix[1:friends]
  neighbors = data[idx,]
  neighbors_class = target[idx]
  tb = table(neighbors_class)
  predicted_class = names(tb)[which.max(tb)]
  ans = list(predicted=predicted_class,
             indices = idx)
  return(ans)
}

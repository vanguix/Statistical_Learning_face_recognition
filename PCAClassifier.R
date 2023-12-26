PCA_Classifier <- function(name){
  # INPUT
  # name - name of the image that is going to be tested, example 'anonym1.jpg'
  # NOTE: the image must be in the same path as the R file
  
  # OUTPUT
  # predicted_class - classification of the image (number ID)
  
  # Here, the optimum hyperparameters are used
  threshold = 3954.6 
  metric = 'euclidean'
  friends = 3
  
  # Include required libraries and Rdata files
  library(OpenImageR)
  load("PCA_data.Rdata")
  # NOTE: PCA_data.Rdata must be in the same folder
  # This will load:
  #    P_pca - the transformation matrix
  #    train_pca - the matrix of the training data once applied PCA
  #    D_pca - the vector that specifies which percentage of variance is retained per each PC
  #    means_pca - the vector of the means
  
  # Read and transform the test image
  image_directory <- name
  data_image = readImage(image_directory)
  
  red <- as.vector(data_image[, , 1])
  green <- as.vector(data_image[, , 2])
  blue <- as.vector(data_image[, , 3])
  test_image <- c(red, green, blue)
  
  # Apply the transformation to the test data
  test_PCA = test_image %*% P_pca
  
  # Compute the distances between the test image and the previous data
  aux = rbind(train_PCA, test_PCA)
  distances = as.matrix(dist(aux),method=metric)
  distance = distances[2, 1:1]
  
  # Predict the class based on the nearest neighbors
  nearest_neighbors <- order(distance)[1:friends]
  neighbor_class <- target[nearest_neighbors]
  tb = table(neighbor_class)
  
  # Stablish if the person is in the dataset or not
  if (min(distance) > threshold) {
    # If the person in the image is not in the training dataset, set to 0
    predicted_class = 0
  } else {
    # If the person in the image is in the training dataset it predicts which is the most similar
    predicted_class = names(tb)[which.max(tb)]
  }
  
  return(predicted_class)
}

PCA_Classifier("anonym1.4.jpg")

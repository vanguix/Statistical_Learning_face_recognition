PCA_Classifier <- function(name){
  # INPUT
  # name - name of the image that is going to be tested, example 'anonym1.jpg'
  # NOTE: the image must be in the same path as the R file
  
  # OUTPUT
  # predicted_class - classification of the image (number ID)
  
  # Here, the optimum hyperparameters are used
  threshold = 2800
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
  
  print(length(test_image))
  print(length(means_pca))
  
  #Centering the data
  #centered_test <- scale(test_image, center = means_pca, scale = F)
  centered_test <- test_image - means_pca
  
  # Apply the transformation to the test data
  test_PCA = centered_test %*% P_pca
  
  predicted_class <- our_knn_single(train_pca, test_PCA, target, friends=3, threshold= 2800, metric='euclidean')
  
  return(predicted_class)
}

PCA_Classifier("anonym1.4.jpg")
PCA_Classifier("9332898.15.jpg")
PCA_Classifier("pspliu.1.jpg")

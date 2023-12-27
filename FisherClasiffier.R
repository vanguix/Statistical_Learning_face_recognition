Fisher_Classifier <- function(name){
  # INPUT
  # name - name of the image that is going to be tested, example 'anonym1.jpg'
  # NOTE: the image must be in the same path as the R file
  
  # OUTPUT
  # predicted_class - classification of the image (number ID)
  
  # Use the optimum hyperparameters
  metric = 'euclidean'
  threshold = 980.6
  friends = 3 
  
  # Include required libraries and Rdata files
  library(OpenImageR)
  load("Fisher_data.Rdata")
  # NOTE: Fisher_data.Rdata must be in the same folder
  # This will load:
  #    P_pca - the transformation matrix
  #    train_pca - the matrix of the training data once applied PCA
  #    D_pca - the vector that specifies which percentage of variance is retained per each PC
  #    means_pca - the vector of the means
  #    target -
  #     W -
  #means -
  #     train_
  #     D_Fisher
  
  # Read and transform the test image
  image_directory <- name
  data_image = readImage(image_directory)
  
  red <- as.vector(data_image[, , 1])
  green <- as.vector(data_image[, , 2])
  blue <- as.vector(data_image[, , 3])
  test_image <- c(red, green, blue)
  
  centered_test <- test_image - means_pca
  
  # Apply the transformation to the test data
  test_PCA <- centered_test %*% P_pca
  
  test_Fisher <- test_PCA %*% W
  
  predicted_class <- our_knn_single(train_fisher, test_Fisher, target, friends=3, threshold= 980.6, metric='euclidean')
  
  return(cat("The predicted class is: ", predicted_class))
}

Fisher_Classifier("anonym1.4.jpg")
Fisher_Classifier("9332898.15.jpg")
Fisher_Classifier("pspliu.1.jpg")

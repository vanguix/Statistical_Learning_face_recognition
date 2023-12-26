# Install and load the required packages and functions
library(OpenImageR)
#install.packages("imager")
#library(imager)
library(gridExtra)
library(grid)
library(class)
library(dplyr)
source("our_knn_multiple.R")
source("accuracy.R")

# Define the required functions
split_train_test <- function(files) {
  # Function to separate between test1, tes2, and train sets
  # INPUT
  # files - a list with the paths of the images
  
  # OUTPUT
  # a dataframe with threee columns:
  #      - the path of each image
  #      - the label/identifier of the image
  #      - the set it belongs (it can be: train, test1 or test2)
  
  #To select people randomly but in the same way in every execution of the code
  set.seed(42)
  
  # Extract label from every image
  images <- c()
  identifiers <- c()
  for (image in files) {
    if (substr(image, nchar(image) - 3 + 1, nchar(image)) == 'jpg') {
      images <- c(images, image)
      label <- gsub(".*?([0-9]+).*", "\\1", image) #1 refers to first
      identifiers <- c(identifiers, as.numeric(label)) #there should be 25 ids
    }
  }
  
  # Set up the dataframe with columns file and target
  df <- data.frame(images, as.factor(identifiers))
  names(df) <- c('file', 'target') 
  
  # Randomly select 4 identifiers to exclude from training
  set.seed(42)
  excluded_identifiers <- sample(unique(df$target), 4)
  
  # Exclude the selected identifiers from the training set
  df_train_validation <- df[!df$target %in% excluded_identifiers,]
  
  # Randomly select 5 images for training and 1 for testing for each remaining identifier
  # Add this images to the dataset 'train' and 'test1'
  df_train_validation <- df_train_validation %>%
    group_by(target) %>%
    mutate(split = sample(c(rep('train', 5), 'test1'), size = n())) %>% #5 IMAGES TO TRAIN, 1 to test
    ungroup()
  
  # Add the excluded identifiers to the 'test2' set
  df_test <- df[df$target %in% excluded_identifiers,]
  df_test$split <- 'test2'
  
  # Combine the datasets
  split_df <- rbind(df_train_validation, df_test)
  return(as.data.frame(split_df))
}

read_images <- function(lista_archivos){
  # INPUT
  # file_list - a list with all the paths of the images
  
  # OUTPUT
  # a list with all the data of the images (each item of the list corresponds to one image)
  
  # Create an empty list to store data of the images
  data_images <- list()
  
  i=1
  # Loop to read every image and store its data in the list
  for (archivo in lista_archivos) {
    data_image = readImage(archivo)
    data_images[[i]] <- data_image
    i=i + 1
  }
  return(data_images)
}

transform_data <- function(image_list){
  # INPUT
  # image_list - a list with the data of every image (this was obtained using read_images)
  
  # OUTPUT
  # pic_matrix - a matrix in which every row contains the data of one image
  #              dimensions must be 108000 columns and as many rows as instances

  # Initialize an empty pic_matrix with the right size (right number of columns)
  pic_matrix <- matrix(nrow = 0, ncol = 108000)  # Must have 108000 columns
  
  # Loop to transform the data of every image into a row and adds it to the pic_matrix
  for (i in seq_along(image_list)) {
    pic <- image_list[[i]]
    new_row <- t(as.vector(pic))
    pic_matrix <- rbind(pic_matrix, new_row)
    
    # Verifying that the necessary matrix has the correct dimensions
    #print(dim(pic_matrix))
    
    }
  return(pic_matrix)
}

pca <- function(data) { 
  # INPUT
  # data - a matrix that contains the data to which we must apply the PCA (a set of observations)
  
  # OUTPUT
  # result - a list that contains the following elements:
  # mean - a vector that contains the mean of every column (this is useful to center the data) 
  # P - the rotation matrix (this is the matrix we must multiply our data to obtain the data reduced with PCA)
  # D - a vector that contains the variance explained with every new eigenvector that is added
  # reduced_data - the data with dimensionality reduction (that is having applied matrix P)

  
  # Calculate the mean of the observations
  mean_vec <- colMeans(data)
  
  # Center the data by subtracting the mean (and we have to decide whether if we scale or not)
  centered_data <- scale(data, center = mean_vec, scale = F) #cambiar centrer = T y ver si sale igual
  
  # Compute the covariance matrix
  cov_matrix <- cov(t(centered_data))
  
  #From now on, we perform everything with the short form of the data matrix (as h<<P).
  # Perform eigenvalue decomposition for the short
  
  eigen_result <- eigen(cov_matrix) #is the short this transposed?

  # Extract eigenvectors (P) and eigenvalues (e_values)
  e_values <- eigen_result$values #as P will be calculated using the shot and the eigenvalues of the long and the short are the same, we direclty calculate them with the short
  P_short <- eigen_result$vectors #this cannot be calculated like this as data is too large (36000*36000) so instead we use the short (150x150) 
  print(dim(P_short)) # should be 150*1????????????????????????? 
  print(dim(data))
  
  
  #sdev <- sqrt(e_values) #standard deviations of the principal components
  
  #Percentage of the variance retaining by the PCs
  #D <- cumsum(sdev^2/sum(sdev^2))
  D<-cumsum(e_values)/sum(e_values)
  
  #Now we calculate the eigenvectors of the long matrix
  #to do this, we need the eigenvectors of the short and data
  
  #When applying the P eigenvectors 
  P <- t(centered_data)%*%P_short #shape(P) should be 36000*1
  
  reduced_data<-centered_data%*% P
  # Return mean, eigenvectors (matrix P), and variance (matrix D) of the set of observations
  result <- list(mean = mean_vec, P = P, D = D, reduced_data= reduced_data)
  return(result)
}

classifier <- function(df_train_data, df_test_data,PCAopt,k=3,thres=40) {
  # INPUT
  # df_train_data - the data (without the label) of the training set (it is a matrix)
  # df_test_data - the data (without the label) of the test set (it is a matrix)
  # PCAopt - binary value 1 if we want to apply PCA, 0 if we do not want to apply PCA
  # k - number of neighbours for the KNN method
  # thres - threshold value to determine whether a new image belongs to the dataset or not
  
  # OUTPUT
  # it returns the output of the knn applied
  
  #1. Read the files from the dataframe
  images_train = read_images(df_train_data$file)
  images_test = read_images(df_test_data$file)
  
  #1. Transform the data
  
  train_matrix = transform_data(images_train)
  test_matrix = transform_data(images_test)
  #print('----------------')
  #print(dim(train_matrix))
  #print(dim(test_matrix))
  
  #2. Apply KNN
  if (PCAopt == TRUE) {
    #2.1. Apply the PCA function only to the training
    pca_values = pca(train_matrix)
    
    P= pca_values$P
    means = pca_values$mean
    train_PCA =  pca_values$reduced_data #train data having used PCA to reduce dimensionality
    D = pca_values$D
    
    #2.2. KNN with PCA
    #First, we select the data that will be used for KNN.
    #The training set is already transformed in the previous PCA function.
    datos_train = data.frame(x = train_PCA[,1], #taking the two first components of train_PCA
                             y = train_PCA[,2])
    
    #The testing set needs to be transformed with the eigenvectors and the mean from
    #the training set after applying PCA.
    centered_data_test <- scale(test_matrix, center = means, scale = F)
    
    print('-------------')
    print(dim(centered_data_test))
    print(dim(P))
    #When applying the P eigenvectors 
    test_PCA <- centered_data_test %*% P
    
    datos_test = data.frame(x = test_PCA[,1], #taking the two first components of test_PCA
                             y = test_PCA[,2])
    
    knn_applied <- our_knn_multiple(datos_train, datos_test, df_train_data$target, friends=k, thres)
  }
  else{
    #2.3. KNN without PCA
    knn_applied <- our_knn_multiple(train_matrix, test_matrix, df_train_data$target, friends=k, thres)
  }
  
  #3. Accuracy from the predictions
  
  #knn_applied$predicted == df_test_data$target
  
  return(knn_applied)
}


# Specify the directory containing your image files
image_directory <- "C:/Training"

# Obtain the file list with all the files in the folder
files_list <- list.files(path = image_directory, pattern = ".*\\.jpg$", full.names = TRUE)

prueba2 = split_train_test(files_list)

df_train_data <- subset(prueba2,split == 'train')
df_test_data <- subset(prueba2,split != 'train')


thresholds = list(20,30,40,50)

accuracies = list()

for (th in thresholds){
  resultado = classifier(df_train_data, df_test_data, 0, k=3,thres=th) # not using PCA
  a= accuracy(resultado,df_test_data$target, df_test_data$split)
  accuracies = append(accuracies, a[1]) # Keep only the accuracy related to belonging (to the threshold parameter)
}

resultado = classifier(df_train_data, df_test_data, 0, k=3,thres=30) # not using PCA
resultado_pca= classifier(df_train_data, df_test_data, 1, k=3,thres=30) # using PCA





#pca_out= pca(resultado) #Our PCA value
#Values to compare our solution
#real_pca = prcomp(resultado) #The real PCA value
#D_real = cumsum(real_pca$sdev^2/sum(real_pca$sdev^2)) #make sure that D calculated in our function has the proper valu
#resultado = classifier(image_list = data_images, parameters)



accuracy <- function(prediction, target, test_type ){
  corrects_belong= 0
  corrects_target=0
  for (i in seq_along(prediction)){
    if ((prediction[i] == 'Not in the dataset' & test_type[i]=='test2') | (prediction[i] != 'Not in the dataset' & test_type[i]=='test1')){
      corrects_belong = corrects_belong + 1 
      
      if(prediction[i]== target[i]){
        corrects_target= corrects_target + 1
      }
      }
  }
  
  accuracy_belong= corrects_belong/ length(prediction)
  accuracy_target = corrects_target/ length(prediction)
  return(list(accuracy_belong, accuracy_target))
}
  
a= accuracy(resultado,df_test_data$target, df_test_data$split)
a[1]
a[2]

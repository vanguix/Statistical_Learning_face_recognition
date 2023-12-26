#library(gplots)
images_train = read_images(df_train_data$file)

#1. Transform the data

train_matrix = transform_data(images_train)
distance_matrix= dist(train_matrix)

dev.new()
# Crear el heatmap
heatmap(as.matrix(distance_matrix),
        main = "Heatmap de Distancias",
        labRow = df_train_data$target,
        labCol = df_train_data$target,
        col= heat.colors(11))

# Obtener los valores únicos en la matriz
legend("topright", legend = seq(min(distance_matrix), max(distance_matrix)-1, length.out = 11), 
       fill = heat.colors(11), title = "Values", cex = 0.8)


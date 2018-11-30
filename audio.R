library(stringr)
library(dplyr)
library(tuneR)
library(seewave)


files <- fs::dir_ls(
  path = "cats_dogs/train/", 
  recursive = TRUE, 
  glob = "*.wav"
)


df <- data_frame(
  fname = files, 
  class = fname %>% str_extract("cats_dogs/train/.*/") %>% 
    str_replace_all("cats_dogs/train", "") %>%
    str_replace_all("/", ""),
  class_id = class %>% as.factor() %>% as.integer() - 1L
)



saveRDS(df, "df.rds")



# Creates a generator from a dataset.

library(tfdatasets)

audio_ops <- tf$contrib$framework$python$ops$audio_ops

data_generator <- function(df, batch_size, shuffle = TRUE, 
                           window_size_ms = 30, window_stride_ms = 10) {
  
  window_size <- as.integer(16000*window_size_ms/1000)
  stride <- as.integer(16000*window_stride_ms/1000)
  fft_size <- as.integer(2^trunc(log(window_size, 2)) + 1)
  n_chunks <- length(seq(window_size/2, 16000 - window_size/2, stride))
  
  ds <- tensor_slices_dataset(df)
  
  if (shuffle) 
    ds <- ds %>% dataset_shuffle(buffer_size = 100)  
  
  ds <- ds %>%
    dataset_map(function(obs) {
      
      # decoding wav files
      audio_binary <- tf$read_file(tf$reshape(obs$fname, shape = list()))
      wav <- audio_ops$decode_wav(audio_binary, desired_channels = 1)
      
      # create the spectrogram
      spectrogram <- audio_ops$audio_spectrogram(
        wav$audio, 
        window_size = window_size, 
        stride = stride,
        magnitude_squared = TRUE
      )
      
      spectrogram <- tf$log(tf$abs(spectrogram) + 0.01)
      spectrogram <- tf$transpose(spectrogram, perm = c(1L, 2L, 0L))
      
      # transform the class_id into a one-hot encoded vector
      response <- tf$one_hot(obs$class_id, 2L)
      
      list(spectrogram, response)
    }) %>%
    dataset_repeat()
  
  ds <- ds %>% 
    dataset_padded_batch(batch_size, list(shape(n_chunks, fft_size, NULL), shape(NULL)))
  
  ds
}

library(keras)
library(dplyr)



df <- readRDS("df.rds") %>% sample_frac(1)
set.seed(4)
id_train <- sample(nrow(df), size = 0.7*nrow(df))

ds_train <- data_generator(df[id_train,], 1L)
ds_test <- data_generator(df[-id_train,], 1, shuffle = FALSE)

set.seed(3)
model <- keras_model_sequential()
model %>%  
  layer_conv_2d(input_shape = c(98, 257, 1), 
                filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 256, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = 'sigmoid')

# Compile model
model %>% compile(
  loss = loss_binary_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

# Train model
model %>% fit_generator(
  generator = ds_train,
  steps_per_epoch = 5,
  epochs = 10, 
  validation_data = ds_test, 
  validation_steps = 5
)

sess <- tf$Session()
batch <- next_batch(ds_train) #error
str(sess$run(batch))





library(TDA)
library(TDAstats)

# read data
setwd('/Users/ibatu/Documents/MyProgramsWindows/TopologicalDataAnalysis/RTDA/')
data <- read.csv(file = 'breast_cancer_dataset.csv', header = TRUE)
data <- data[names(data)!='label']

# configuration parameters
sample_size = 500
n_samples = 5
wasserstein_exp = 1
wasserstein_dim = 0

# draw samples and compute persistent homologies
my_list <- list()
for (i in 1:n_samples) {
  data_sample <- data[sample(nrow(data), sample_size , replace = FALSE), ]
  my_list[[i]] <- calculate_homology(data_sample)
}

# compute wasserstein distances between persistent homologies of samples
distance_matrix <- matrix(nrow = n_samples, ncol = n_samples)
for (row in 1:nrow(distance_matrix)) {
  for (col in row:ncol(distance_matrix)) {
    distance_matrix[row,col] <- wasserstein(my_list[[row]],my_list[[col]], 
                                            p = wasserstein_exp, 
                                            dimension = wasserstein_dim)
  }
}

# print wasserstein distances as a matrix
distance_matrix
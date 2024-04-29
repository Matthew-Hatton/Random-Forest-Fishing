#create gif using images in file
library(gifski)
png_files <- list.files(path = "./fishing/Future Prediction/Figures/probability/",full.names = TRUE)
# Extract the numbers from the file names
file_numbers <- gsub("[^0-9]", "", basename(png_files))

# Convert the extracted numbers to integers
file_numbers <- as.integer(file_numbers)

# Sort the file names based on the extracted numbers
sorted_files <- png_files[order(file_numbers)]
gifski::gifski(sorted_files, gif_file = "./fishing/Future Prediction/Figures/2017probability.gif",delay = 0.2,height = 1080,width = 1920)

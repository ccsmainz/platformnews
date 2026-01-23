library(magick)

grid_similarity <- function(image_path, v_pos = 0.05, cutoff = .1) {
  img  <- image_read(image_path)
  info <- image_info(img)
  if(info$height < 1000){return(FALSE)}
  
  # 1. Calculate Y coordinate from relative position
  # Ensure the 10px patch stays within bounds
  y_coord <- (info$height * v_pos) - 5
  y_coord <- max(0, min(y_coord, info$height - 10))
  
  # 2. Define 5 horizontal centers (10%, 30%, 50%, 70%, 90%)
  x_centers <- seq(info$width * 0.1, info$width * 0.9, length.out = 5) - 5
  
  # 3. Extract 10x10 patches using map
  patches <- map(x_centers, ~image_crop(img, geometry_area(10, 10, .x, y_coord))) %>% 
    do.call(c, .)
  
  # 4. Generate Similarity Matrix
  # expand.grid creates all pairs (1,1; 1,2; etc.)
  n <- length(patches)
  idx <- expand.grid(i = 1:n, j = 1:n)
  
  sim_mat <- map2_dbl(idx$i, idx$j, ~{
    image_compare_dist(patches[.x], patches[.y], metric = "MAE")$distortion
  }) %>% 
    matrix(nrow = n, ncol = n)
  
  # Labels based on percentage for clarity
  labels <- paste0("X", seq(10, 90, 20), "%_Y", round(v_pos * 100), "%")
  rownames(sim_mat) <- colnames(sim_mat) <- labels
  
  # Visual debug
  #print(image_append(patches, stack = FALSE))
  
  mean(sim_mat[lower.tri(sim_mat)]) > cutoff
}



grid_similarity("img/tnrI2XAazx0.mp4.jpg", .95)

a = tibble(img = list.files("img", "*.jpg", full.names = T)) |> 
 mutate(top_vertical = map_lgl(img, grid_similarity, v_pos = .025, .progress = T)) |> 
mutate(bottom_vertical = map_lgl(img, grid_similarity, v_pos = .975, .progress = T))|> 
  mutate(id = str_remove_all(basename(img), ".mp4.jpg")) |> 
  select(-img) |> 
  mutate(model = "image_compare_02") |>
  mutate(is_vertical = top_vertical | bottom_vertical)


a |> summarise_if(is.logical, mean)#

a |> 
  write_tsv("_data/is_vertical.tsv.gz")

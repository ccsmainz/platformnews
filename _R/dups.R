library(tidyverse)
library(textreuse)

find_near_duplicates <- function(df, text_col, id_col, 
                                 threshold = 0.7, 
                                 n_hashes = 200, 
                                 bands = 50) {
  
  # 1. Create the MinHash function specifically
  # Ensure n_hashes is divisible by bands
  if (n_hashes %% bands != 0) {
    stop("n_hashes must be evenly divisible by bands.")
  }
  
  mh <- minhash_generator(n = n_hashes, seed = 42)
  
  # 2. Extract and name the text vector
  # textreuse requires names to identify documents
  text_vector <- df %>% pull({{text_col}})
  names(text_vector) <- df %>% pull({{id_col}})
  
  # 3. Create Corpus 
  # Using the functional approach often avoids the 'binary operator' error
  corpus <- TextReuseCorpus(text = text_vector,
                            tokenizer = tokenize_ngrams, n = 5,
                            minhash_func = mh,
                            keep_tokens = FALSE,
                            progress = TRUE)
  
  # 4. LSH and Comparison
  # We return a clean tibble of matches
  results <- lsh(corpus, bands = bands) %>%
    lsh_candidates() %>%
    lsh_compare(corpus, jaccard_similarity) %>%
    as_tibble() %>%
    filter(score >= threshold) %>%
    arrange(desc(score))
  
  return(results)
}

r_dups = possibly(find_near_duplicates)


d_p = d |> 
  select(id, timestamp, platform)

transcripts = read_tsv("../data/transcripts.tsv.gz", col_types = "cc") |> 
  mutate(id = str_remove_all(id, ".txt"))
transcripts

d_dups =  d  |> 
  #mutate(text = paste(title, text)) |> 
  group_by(outlet) |> 
  nest() |> 
  ungroup() |>
  mutate(dups = map(data, ~ r_dups(.x, "text", "id"))) |> 
  unnest(dups) |> 
  left_join(d_p, by = c(a = "id")) |> 
  left_join(d_p, by = c(b = "id")) |> 
  filter(platform.x != platform.y) 


d_dups |> 
  select(x = a, y = b, platform.x, platform.y) |> 
  write_tsv("_data/duplicates.tsv.gz")

d |> 
  mutate(is_dup = id %in% c(d_dups$a, d_dups$b)) |> 
  count(outlet, is_dup)
  


d_dups |> 
  count(outlet, platform.x, platform.y) |> 
  left_join(count(d, outlet, platform), by = c(platform.x = "platform", outlet = "outlet")) |> 
  mutate(prop_dups = n.x/n.y)
 
         

res
summary(res)

res |> filter(prop_dups < 1) |> summary()

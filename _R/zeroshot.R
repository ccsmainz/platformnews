MEMOISE = T
jgu_creds = function() Sys.getenv("JGU_API_KEY")

source(here::here("_R/setup.R"))

library(ellmer)
library(furrr)
plan(multicore)

library(memoise)

if (MEMOISE==T){
  pcs = memoise(parallel_chat_structured,
                cache = cachem::cache_disk(dir = here::here("_memoise")))
} else {
  pcs = parallel_chat_structured
}


create_types = function(properties){
  do.call(type_object,
    map(properties, ~ paste0("type_", .x$type)) |> map(do.call, args = list(required = T))
  )
}

get_chat = function(model, system_prompt, temperature = 0, max_tokens = 1000){
  p = params(temperature = temperature)
  api_args = list(max_tokens = max_tokens)

  #system_prompt = paste(system_prompt,"/no_think")

  if (str_detect(model, "gemini")) {
    chat_api <- chat_google_gemini(system_prompt = system_prompt,
                                   model = model, params = c(p, api_args)
    )
  } else if(str_detect(model, "gpt")) {
    chat_api <- chat_openai(system_prompt = system_prompt,
                            model = model, params = p, api_args = api_args)
  }
  else if(str_detect(model, ":")) {
    chat_api <- chat_ollama(
      model = model,
      base_url = "http://brain.publizistik.uni-mainz.de:11434",
      system_prompt = system_prompt, params = p,
      api_args = c(api_args, num_gpu = 999)
    )
  } else {
    if (str_detect(model, "GPT")) api_args = c(api_args, reasoning_effort = "low")
    chat_api <- chat_openai_compatible(
      base_url = "https://ki-chat.uni-mainz.de/api",
      #api_key = Sys.getenv("JGU_API_KEY"),
      credentials = jgu_creds,
      model = model,
      system_prompt = system_prompt,
      params = p,
      api_args = api_args)
  }
  chat_api
}



llm_code <- function(prompts, types = type_object(answer = type_string()), model = "Qwen3 235B VL", chunk_size = 50) {
  system_prompt <- " You are a unemotional, precise content analyst, classifying given posts from short video platforms according to the instructions provided. You return JSON only."
  chunks = split(prompts, (seq_along(prompts) - 1) %/% chunk_size)
  map_dfr(chunks, ~ pcs(get_chat(model, system_prompt, temperature = 0), .x, types, on_error = "continue", max_active = 2))
}


auto_code <- function(df, yaml_file, model = "Qwen3 235B VL", image_path = "img") {
  print(yaml_file)
  yml <- yaml::read_yaml(yaml_file, readLines.warn = F)

  task <- yml$task
  types = create_types(yml$schema$properties)
  #print(types)
  #schema_json <- yml$schema |> toJSON(auto_unbox = T)
  #types <- ellmer::type_from_schema(schema_json)

  model_name <- str_to_lower(model) |> str_remove_all("\\s+")
  name <- paste0(basename(yaml_file), "_", model_name)

  df$blank = ""
  columns = c("blank", yml$columns)
  columns = columns[!columns == "image"]

  df <- df |>
    select(id, all_of(columns)) |>
    unite(col = "text", where(is.character) & !starts_with("id"), sep = "\n", na.rm = TRUE, remove = TRUE) |>
    #mutate(text = str_trunc(text, 5000)) |>
    mutate(prompt = interpolate("## Task\n{{task}} \n\n## Post\n{{text}}"))

  if ("image" %in% yml$columns) {
  if(yml$name =="vertical") image_path = paste0(image_path, "/img_top")
  df = df |>
    mutate(img = glue::glue("{image_path}/{id}.mp4.jpg")) |>
    filter(map_lgl(img, file.exists)) |>
    mutate(prompt = map2(prompt, img, ~ list(.x, content_image_file(.y))))
  }
  
  #print(df)
  #print(df$text[1])

  df |>
    mutate(response = llm_code(prompt, types = types, model = model)) |>
    select(id, response) |>
    unnest(response) |> 
    mutate(model = model) |> 
    select(-any_of(c(".error")))
}


code_dataset = function(df, codebook_dir, stage, models){
  multimodal = "^(Gemma|gemini|gpt|Qwen3)"

  todo <- tibble(codebook = list.files(codebook_dir, ".yml", full.names = T)) |>
    mutate(details = map(codebook, yaml::read_yaml, readLines.warn = F),
           stage = map(details, "stage"),
           with_image =  map(details, "columns") |> map_lgl(~ "image" %in% .x)) |>
    unnest(stage) |>
    mutate_if(is.character, str_squish)
  

  if(!is.null(stage)){
    filter_stage = stage
    todo = filter(todo, stage == filter_stage)
  }


 todo = todo |>
   expand_grid(model = models) |>
   filter(! (with_image == T & ! str_detect(model, multimodal))) 
 
 print(todo)
 
 done = map_dfr(models, ~ todo |> 
       filter(model == .x) |> 
       mutate(results = map2(codebook, model, ~ auto_code(df, .x, .y)))
     )
 

 done |> 
   mutate(codebook = basename(codebook) |> str_remove_all(".yml")) |> 
   select(codebook, stage, model, results) |> 
   group_by(codebook, stage) |> 
   summarise(data = list(bind_rows(results))) |> 
   mutate(hash = digest::sha1(data)) |> 
   mutate(fname = glue::glue("_data/{codebook}_{stage}_{hash}.tsv.gz") |> here::here()) |> 
   mutate(w = map2(data, fname, write_tsv))
 
 #results = results |> 
 #  mutate(results = future_pmap(list(df = df, yaml_file = codebook, model = model), auto_code)) |>
 #  select(codebook, model, stage, results) |>
 #  mutate(results = map(results, ~ gather(.x, variable, value, -id))) |>
 #  #unnest(results) |>
 #  mutate(codebook = basename(codebook))

 #results
}

d = read_tsv(here::here("_data/d.tsv.gz")) |> 
  mutate(outlet = paste(brand, country)) |> 
  mutate(outlet_type = ifelse(outlet_type=="broadcast", paste(ownership, "broadcaster"), outlet_type) |> 
           str_to_sentence()) |> 
  mutate(platform = factor(platform) |> relevel("TikTok"))

regions = read_csv(here::here("_data/regions.csv")) |> 
  rename(country = Market, region = Region)

d = d |> 
  left_join(regions, by = "country") |> 
  filter(country != "Hong Kong") |> 
  left_join(read_tsv(here::here("_data/transcripts.tsv.gz"), col_names = c("id", "text")))

reli = read_tsv(here::here("_data/d_reli.tsv.gz")) 
small = read_tsv(here::here("_data/d_small.tsv.gz")) 
big = read_tsv(here::here("_data/d_big.tsv.gz")) 

#code_dataset(reli, codebook_dir = here::here("_codebooks"), stage = "reli", 
             #models = c("gemini-2.5-flash-lite","Qwen3 235B VL"))



#auto_code(reli[1:10,], "_codebooks/vid_features.yml", image_path = "img/img_top", model = "Qwen3 235B VL")

code_dataset(reli, codebook_dir = here::here("_codebooks"), stage = "exagg", models = c("gemini-2.5-flash-lite"))


code_dataset(small, codebook_dir = here::here("_codebooks"), stage = "small", models = c("gemini-2.5-flash-lite"))

#code_dataset(big, codebook_dir = here::here("_codebooks"), stage = "big", models = c("gemini-2.5-flash-lite"))

#big2 = read_tsv("_data/post_topics_big_87e5d10d4e8ae0fdb072c5910c346fed4dcec555.tsv.gz") |> filter(is.na(politics)) |> select(id) |> left_join(big)
#code_dataset(big2, codebook_dir = here::here("_codebooks"), stage = "big", models = c("gemini-2.5-flash-lite"))


#list.files("_data", "post_topics", full.names = T) |> 
#  map_dfr(read_tsv) |> 
#  filter(!is.na(politics)) |> 
#  distinct()

#a = read_tsv("_data/small_post_topics.tsv.gz") 
#
#props = a |> 
#  summarise_if(is.logical, mean, na.rm = T) |> 
#  gather(Variable, prop)
#
#a |> distinct(id, model, .keep_all = T) |> 
#  test_icr(id, model, na.omit = T) |> 
#  left_join(props)
#
##unique(a$model) |> 
##  map_df( ~ filter(a, model != .x) |> 
##            test_icr(id, model, na.omit = T) |> 
##            mutate(without = .x)) |> 
##  ggplot(aes(x = Variable, y = Krippendorffs_Alpha, group = without, color = without))+
##  geom_point(size=2)+
##  coord_flip()
#
##a
#
##a |> 
##  filter(model != "GPT OSS 120B") |> 
##tidycomm::test_icr(unit_var = id, coder_var = model)
#
#
#
#
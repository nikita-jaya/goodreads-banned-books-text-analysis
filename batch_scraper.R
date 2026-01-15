library(readr)
library(tidyverse)
library(rvest)
library(stringdist)
library(quanteda)
library(quanteda.extras)
library(jsonlite)

# https://pen.org/book-bans/pen-america-index-of-school-book-bans-2023-2024/
banned_books_index <- read_csv("PEN America's Index of School Book Bans (July 1, 2023 - June 30, 2024) - Sorted by State & District.csv", skip = 2)


# Function to get Goodreads book title from search
# --- Goodreads URL finder: search by title, match author ---
get_goodreads_url <- function(title, author, title_weight = 0.8, author_weight = 0.2) {
  normalize <- function(x) str_squish(str_to_lower(str_replace_all(x, "[[:punct:]]", "")))
  norm_input_title <- normalize(title)
  norm_input_author <- normalize(author)
  
  # Helper function to fetch candidates from a query
  fetch_candidates <- function(query) {
    search_url <- paste0("https://www.goodreads.com/search?q=", URLencode(query))
    page <- tryCatch(read_html(search_url), error = function(e) return(NULL))
    if (is.null(page)) return(tibble())
    
    candidate_titles <- page %>% html_elements("a.bookTitle span") %>% html_text(trim = TRUE)
    candidate_authors <- page %>% html_elements("a.authorName span") %>% html_text(trim = TRUE)
    candidate_urls <- page %>% html_elements("a.bookTitle") %>% html_attr("href") %>% paste0("https://www.goodreads.com", .)
    
    n <- min(length(candidate_titles), length(candidate_authors), length(candidate_urls))
    if (n == 0) return(tibble())
    
    tibble(
      candidate_title = candidate_titles[1:n],
      candidate_author = candidate_authors[1:n],
      candidate_url = candidate_urls[1:n]
    )
  }
  
  # --- Step 1: Search by title only ---
  candidates <- fetch_candidates(title)
  
  if (nrow(candidates) == 0) return(NA_character_)
  
  candidates <- candidates %>%
    mutate(
      norm_title = normalize(candidate_title),
      norm_author = normalize(candidate_author),
      title_sim = stringsim(norm_title, norm_input_title, method = "jw"),
      author_sim = stringsim(norm_author, norm_input_author, method = "jw"),
      total_score = title_weight * title_sim + author_weight * author_sim
    ) %>%
    arrange(desc(total_score))
  
  return(candidates$candidate_url[1])
}

scrape_goodreads_book <- function(title, author) {
  i <<- 1 + i
  print(paste0("Scraping Book ", i, ":", title))
  url <- get_goodreads_url(title, author)
  if (is.na(url)) {
    return(tibble(
      goodreads_title = NA,
      goodreads_author = NA,
      goodreads_description = NA,
      goodreads_publication_date = NA,
      goodreads_avg_rating = NA,
      goodreads_num_ratings = NA,
      goodreads_num_reviews = NA
    ))
  }
  
  page <- tryCatch(read_html(url), error = function(e) return(NULL))
  if (is.null(page)) {
    return(tibble(
      goodreads_title = NA,
      goodreads_author = NA,
      goodreads_description = NA,
      goodreads_publication_date = NA,
      goodreads_avg_rating = NA,
      goodreads_num_ratings = NA,
      goodreads_num_reviews = NA
    ))
  }
  
  goodreads_title <- page |>
    html_element("h1[data-testid='bookTitle']") |>
    html_text(trim = TRUE)
  
  goodreads_author <- page |>
    html_elements("a.ContributorLink[href*='/author/show/']") %>%
    html_text(trim = TRUE) %>%
    unique() %>%
    paste(collapse = ", ")
  
  goodreads_description <- page |>
    html_element("div[data-testid='description']") |>
    html_text(trim = TRUE)
  

  goodreads_publication_date <- page %>%
    html_element("p[data-testid='publicationInfo']") %>%
    html_text(trim = TRUE) %>%
    str_extract("\\b(?:[A-Za-z]+ \\d{1,2}, \\d{4}|\\d{4})\\b")
  
  # --- Ratings Summary ---
  json_ld <- page |>
    html_element("script[type='application/ld+json']") |>
    html_text()
  
  if (!is.null(json_ld)) {
    json_data <- fromJSON(json_ld)
    
    # Ratings from JSON
    goodreads_avg_rating <- json_data$aggregateRating$ratingValue %>% as.numeric()
    goodreads_num_ratings <- json_data$aggregateRating$ratingCount %>% as.integer()
    goodreads_num_reviews <- json_data$aggregateRating$reviewCount %>% as.integer()
  } else {
    goodreads_avg_rating <- NA
    goodreads_num_ratings <- NA
    goodreads_num_reviews <- NA
  }
  
  
  Sys.sleep(runif(1, min = 2, max = 5))
  
  tibble(
    goodreads_title = goodreads_title,
    goodreads_author = goodreads_author,
    goodreads_url = url,
    goodreads_description = goodreads_description,
    goodreads_publication_date = goodreads_publication_date,
    goodreads_avg_rating = goodreads_avg_rating,
    goodreads_num_ratings = goodreads_num_ratings,
    goodreads_num_reviews = goodreads_num_reviews
  )
}

# --- Main workflow ---
scrape_books_from_df <- function(books_df) {
  books_df |>
    rowwise() |>
    mutate(
      scraped = list(scrape_goodreads_book(title, author))
    ) |>
    unnest(cols = scraped) |>
    ungroup()
}

# Example dataframe of books
banned_abridged <- banned_books_index |>
  select(Title, Author) |>
  unique() |>
  rename(title = Title, author = Author) |>
  mutate(author = str_replace(author, "^(.*),\\s*(.*)$", "\\2 \\1"))


# Run the combined scraper
batch <- function(n){
  i <<- 0
  start <- 1 + (n-1)*100
  end <- min(n*100, nrow(banned_abridged))
  banned_abridged |>
    slice(start:end) |>
    scrape_books_from_df() |>
    write.csv(file = paste0("scraped", n, ".csv"))
}


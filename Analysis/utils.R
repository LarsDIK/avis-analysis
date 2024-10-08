#' Legacy Function to Get Text by Identifier
#'
#'
#' @param corp quanteda corpus object
#' @param ids character text ids
#' @param n numeric number of hits, defaults to NULL.
#' @param identifier character name of id column.
#' @param txt character column identifiert. Defaults to 'texts'.
#'
#' @export
get_text_by_id <- function(corp,
                           ids,
                           n = NULL,
                           identifier = "id",
                           txt = "texts"){
  tf <- docvars(corp, identifier) %in%
    ids

  if(is.null(n)) {
    return(corp$documents[,txt][tf])
  }

  corp$documents[,txt][tf][1:n]


}

#' Get Subsets of a Corpus by Ids
#'
#' @param corp quanteda corpus object
#' @param ids character text ids
#' @param idvar character name of the id column. Defaults to id.
#' @importFrom quanteda corpus_subset docvars
#' @export
get_subcorpus_by_id <- function(corp,
                                ids,
                                idvar = "id"){
  corpus_subset(corp,
                (docvars(corp, idvar) %in% ids)
  )
}

#' Clean up Human Tags (groundtruth)
#'
#' Removes leading numbers from tag and turn comma separated strings
#' into true character vectors.
#'
#' @param x character vector or list of tags.
#' @export
clean_manual_tags <- function(x){
  unique(gsub("(^[0-9]{2})(.+)","\\2",
              unlist(strsplit(x, ","))))
}



#' Convenience Function to Read & Merge Multiple Years
#'
#' @param AVIS_YEARS numeric vectors of selected years.
#' @param just_meta boolean should meta be used w/o reading in the actual text?
#' Defaults to TRUE.
#' @param path character location of the collection files.
#' @export
#' @import data.table
gather_yearly_collections <- function(AVIS_YEARS,
                                      just_meta = TRUE,
                                      path = "~/avis-data/collections"){
  AVIS_YEARS <- sort(as.numeric(AVIS_YEARS))
  AVIS_YEARS <- intersect(AVIS_YEARS,
                          list.files(path, pattern = "csv") |>
                            substr(8, 11) |>
                            as.numeric())
  path_in <- path
  path <- file.path(path, "yearly_")
  # meta information
  meta_dt <- data.table()
  for (i in AVIS_YEARS){
    meta_file <- paste0(path, i, ".json")
    mi <- fromJSON(meta_file)
    d <- data.table::rbindlist(mi)
    d[, id := names(mi)]
    meta_dt <- rbind(meta_dt, d)
  }
  tryCatch({
    meta_dt[, date := as.Date(date)]
  }, error = function(e) {
    message(
      sprintf("Are you sure the path to your data is correct?\nAvisblatt data cannot be found at %s\n\n",
              path_in)
    )
    stop("Execution stopped.")
  }
  )
  setcolorder(meta_dt, neworder = c("id",
                              setdiff(names(meta_dt),"id")))
  # corpus
  dt <- data.table()
  if(!just_meta){
    for (i in AVIS_YEARS){
      data_file <- paste0(path, i, ".csv")
      dt <- rbind(dt, fread(data_file, encoding="UTF-8"))
    }
    crps <- corpus(dt, docid_field = "id",
                 text_field = "text")
    }

  if(just_meta){
    collect <- Collection$new(NULL, meta_dt)
  } else {
    collect <- Collection$new(crps, meta_dt)
  }
  collect
}


#' @importFrom utils head
purge_spacing <- function(txtlist){
  splits <- strsplit(txtlist, "\\s")
  more_than_1 <- lapply(splits, grepl, pattern = "\\S{2,}")
  single_digit <- lapply(splits, grepl, pattern = "^\\d{1}$")
  not_single_letter <- mapply("|", single_digit, more_than_1, SIMPLIFY = FALSE)
  not_single_letter_shifted <- mapply(c, FALSE, not_single_letter, SIMPLIFY = FALSE)
  not_single_letter_shifted <- lapply(not_single_letter_shifted, head, -1, SIMPLIFY = FALSE)
  tf <- mapply("|", not_single_letter, not_single_letter_shifted, SIMPLIFY = FALSE)
  # cumsum is an elegant way to keep track of T/F swaps
  tfcs <- lapply(tf, cumsum)
  out <- mapply(split, splits, tfcs)
  # reconstruct
  out <- lapply(out, sapply, paste, collapse = "")
  unlist(lapply(out, paste, collapse = " "))
}



advert_distance <- function(corpus_a, corpus_b, consider_length_diff = FALSE){
  if (!is.corpus(corpus_a)|!is.corpus(corpus_b)){
    stop("This function requires (exactly) two quanteda corpora.")
  }

  # STEP 1
  # Use quanteda's textstat_dist to measure distance
  # -> lower values, smaller distance / greater similarity
  dfm_a <- tokens(corpus_a, remove_punct = TRUE, remove_numbers = TRUE) |>
    dfm() |>
    dfm_weight("prop")
  dfm_b <- tokens(corpus_b, remove_punct = TRUE, remove_numbers = TRUE) |>
    dfm() |>
    dfm_weight("prop")
  dist <- as.matrix(textstat_dist(dfm_a, dfm_b))

  # measure is not independent of ad length, correcting for length.
  # Result will be: dist <= 1 indicates reprint, > 1 otherwise
  lengths_a <- as.vector(ntoken(as.character(corpus_a),
                                remove_punct = TRUE, remove_numbers = TRUE))
  lengths_b <- as.vector(ntoken(as.character(corpus_b),
                                remove_punct = TRUE, remove_numbers = TRUE))
  m_a <- matrix(lengths_a, nrow(dist), ncol(dist))
  m_b <- matrix(lengths_b, nrow(dist), ncol(dist), byrow = TRUE)
  x <- m_a + m_b
  dist <- dist * sqrt(x + 0.5 - 2*log(x + 0.5))
  # this approx is fine below 16,
  # a bit too high for values around 25,
  # and increasingly too low for higher N.
  # Correct for that:
  dist <- dist * 25/(24+abs(sqrt(pmax(16, x))-5))

  # STEP 2 (optional):
  # if text lengths of two ads are dissimilar,
  # dist might show similarity where there isn't
  # (true especially for very long ads).
  # Rule out such cases in reprint detection
  # by adding 100 to similarity measure
  # (pushing it recognizably above the regular values)
  # if length difference is too big.
  # One token plus 5% of combined token number is okay,
  # any larger difference rules out reprint
  if (consider_length_diff){
    m_length_diff_above_threshold <- (abs(m_a-m_b)-1)/(m_a+m_b) > 0.2
    # multiplying logical matrix with numeric value turns FALSE into 0, TRUE into 1:
    dist <- dist + 100 * m_length_diff_above_threshold
  }
  dist
}


#' List Avialable Years Based on Meta Information
#'
#' Checks which yearly collections are available based on
#' meta information file names.
#'
#' @param collection_root character file path to collection.
#' @export
available_years <- function(collection_root){
 list.files(collection_root, pattern=".json") |>
    substr(8, 11) |>
    as.numeric()
}



#' @importFrom utils getFromNamespace
tf_integrity <- function(){
  ns <- ls(envir = asNamespace("avisblatt"))
  tfs <- ns[grepl("tagfilter_",ns)]
  l <- lapply(tfs, function(x){
    getFromNamespace(x, ns = "avisblatt")()
  })
  names(l) <- tfs
  summary <- "Checked all tagfilters, no problem with regular expressions found."
  for(i in 1:length(l)){
    tf <- l[[i]][2]$tagfilters
    for(t in c(tf$neg, tf$pos)){
      err <- try(gsub(t, "", "", perl = T), silent = T)
      if (err!=""){
        message("\nIn ", names(l[i]), ", there is an invalid regular expression:")
        print(t)
        summary <- "IMPORTANT: Fix any problem with tagfilter regex before creating collections / re-doing tagging!"
      }
    }
  }
  message(summary)
}

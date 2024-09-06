#--------------------------------
# 1 Functions to list available tags, headers & no_adverts
#    (= marriage, death, election notices etc.)
#--------------------------------

show_tags = function(ids, coll, manual = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1)
  {if(ids=="all"){ids <- coll$meta$id}}
  if(manual){
    unique(unlist(coll$meta$tags_manual[coll$meta$id %in% ids]))
  } else {
    unique(unlist(coll$meta$tags[coll$meta$id %in% ids]))
  }
}

show_headers = function(coll){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  unique(unlist(coll$meta$tags_section))
}

get_headers = function(coll, text = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(text){
    if(is.null(coll$corpus)){
      stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to get header texts")
    } else{
      as.character(corpus_subset(coll$corpus, isheader == TRUE))}
  } else{
    coll$meta[(isheader), id]
  }
}

get_noadverts = function(coll, text = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    if(text){
      as.character(corpus_subset(coll$corpus, noadvert == TRUE))
      } else{
      names(corpus_subset(coll$corpus, noadvert == TRUE))
      }
  }
}


#--------------------------------
# 2 Functions to filter a set of records according to different criteria
#--------------------------------
#   Always IDs in, same or less IDs out.
#   If you want to start with all records in collection, use "all" in ids argument


select_by_date = function(ids = NULL, coll,
                          min = "1729-01-01", max = "1844-12-31"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  min_is_date <- tryCatch(!is.na(as.Date(min, format = "%Y-%m-%d")),
                          error = function(err) {FALSE})
  max_is_date <- tryCatch(!is.na(as.Date(max, format = "%Y-%m-%d")),
                          error = function(err) {FALSE})
  if (min_is_date & max_is_date){
    min <- as.Date(min)
    max <- as.Date(max)
    ids <- intersect(ids, coll$meta[date >= min, id])
    intersect(ids, coll$meta[date <= max, id])
  } else{
    message("If giving a min or max date, it must be in format YYYY-MM-DD.")
  }
}


select_by_season = function(ids = NULL, coll, date_MM_DD = NULL,
                          days_before = 0, days_after = 0){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(!(is.numeric(days_before) & is.numeric(days_after))
     |days_before<0|days_after<0)
    {
    stop("days_before and days_after need to be 0 [default] or positive integers")
  }
  #Converting Xmas/fair to dates
  if(date_MM_DD=="Christmas"){
    date_MM_DD <- "12-25"}
  else if(date_MM_DD=="Fair"){
    date_MM_DD <- "10-27"
    days_after <- days_after+13
    }
  if(tryCatch(!is.na(as.Date(date_MM_DD, format = "%m-%d")))){
    min <- as.Date(paste(seq.int(1729, 1844, 1), rep(paste("-", date_MM_DD, sep=""), 116), sep="")) - days_before
    max <- as.Date(paste(seq.int(1729, 1844, 1), rep(paste("-", date_MM_DD, sep=""), 116), sep="")) + days_after
  } else if (date_MM_DD=="Easter"){
    # Get easter sunday dates for 1729-1844 (according to Grotefend)
    easter_sundays <- fread("../avis-analysis/data/easter_sunday.csv", encoding="UTF-8")
    min <- as.Date(unlist(easter_sundays)) - days_before
    max <- as.Date(unlist(easter_sundays)) + days_after
  } else {
    stop("Date must be either ''Easter'' (= Easter Sunday), ''Christmas'' (Xmas day, 25th), 'Fair' (duration of Basel autumn fait), or an in-year-date in format MM-DD.")
  }
  dt <- coll$meta
  # Limit dt to records in ids
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]}
  # Check for each record in dt, if its date (x)
  # is in between ANY of the 116 min-max intervals.
  # Hence between() will give 116 boolean values here.
  # Sum counts all TRUE occurences,
  # so if date x is in any of the 116 intervals,
  # sum(between()) will deliver 1, otherwise 0.
  dt[unlist(lapply(dt$date,  function(x) (sum(between(x, min, max)) == 1))), id]
}


select_by_length = function(ids = NULL, coll, min = 0, max = 1000000, unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(unit == "tokens"){
    ids <- intersect(ids, coll$meta[ntokens >= min, id])
    intersect(ids, coll$meta[ntokens <= max, id])
  } else if(unit == "char"){
    ids <- intersect(ids, coll$meta[nchar >= min, id])
    intersect(ids, coll$meta[nchar <= max, id])
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}


select_by_tags = function(ids = NULL, coll, tagslist = NULL, headerlist = NULL, manualtagslist = NULL){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  # Limit dt to records in ids
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]}
  tags <- dt$id
  header <- dt$id
  manual <- dt$id
  if(!is.null(tagslist)){
    for (i in 1:length(tagslist)){
      tags <- intersect(tags, dt[grepl(tagslist[i], dt$tags), id])
    }
  }
  if(!is.null(headerlist)){
    for (i in 1:length(headerlist)){
      header <- intersect(header, dt[grepl(headerlist[i], dt$tags_section), id])
    }
  }
  if(!is.null(manualtagslist)){
    for (i in 1:length(manualtagslist)){
      manual <- intersect(manual, dt[grepl(manualtagslist[i], dt$tags_manual), id])
    }
  }
  intersect(tags, intersect(manual, header))
}


select_by_text = function(ids = null, coll, searchlist = ""){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    if(length(searchlist) > 0){
      for (i in 1:length(searchlist)){
        ids <- intersect(ids, names(coll$corpus[grepl(searchlist[i], as.character(coll$corpus), perl = TRUE)]))
      }
    }
    ids
  }
}


select_by_reprint_status = function(ids = NULL, coll, status = c("reprinted_orig","unreprinted_orig", "postings", "reprints", "other", "ads")){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  match.arg(status)
  switch(status,
         reprinted_orig   = intersect(ids, coll$meta[grepl("orig_r",  coll$meta$reprint_status), id]),
         unreprinted_orig = intersect(ids, coll$meta[grepl("orig_u",  coll$meta$reprint_status), id]),
         postings         = intersect(ids, coll$meta[grepl("orig",    coll$meta$reprint_status), id]),
         reprints         = intersect(ids, coll$meta[grepl("reprint", coll$meta$reprint_status), id]),
         other            = intersect(ids, coll$meta[is.na(coll$meta$reprint_status), id]),
         ads              = intersect(ids, coll$meta[!is.na(coll$meta$reprint_status), id]))
}


select_by_language = function(ids = NULL, coll, status = c("de", "fr", "unknown")){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  match.arg(status)
  switch(status,
         de      = intersect(ids, coll$meta[grepl("de",  coll$meta$language), id]),
         fr      = intersect(ids, coll$meta[grepl("fr",  coll$meta$language), id]),
         unknown = intersect(ids, coll$meta[is.na(coll$meta$language), id]))
}



#--------------------------------
# 3 Count and show records
#--------------------------------


count_records_by_date = function(ids = NULL, coll, level = NULL){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  if(!is.null(ids)){
    dt <- dt[id %in% ids,]
  }
  if(is.null(level)){
    dt[, .(N = .N), by = date][order(date)]
  } else if(level == "year"){
    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")))][order(year)]
  } else if(level == "quarter"){
    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                              quarter = ceiling(as.numeric(format(date, "%m"))/3))][order(year, quarter)]
  } else if(level == "month"){
    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                              month = as.numeric(format(date, "%m")))][order(year, month)]
  } else if(level == "week"){
    dt[, .(N = .N), by = list(year = as.numeric(format(date, "%Y")),
                              week = as.numeric(format(date, "%V")))][order(year, week)]
  } else{
    message("Only supports 'year', 'quarter', 'month' and 'week' based aggregation.")
  }
}

count_records_by_length = function(ids = NULL, coll, boundaries = c(0, 10, 20, 40, 80, 160, 1000), unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(boundaries)<2|!is.numeric(boundaries)){stop("The boundaries argument needs to be a list of at leats two non-negative numbers - default is c(0, 100000).")}
  if(!all(diff(boundaries)>0)){stop("The boundaries need to ascending numbers.")}
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  if(!is.null(ids)){dt <- dt[id %in% ids,]}
  if(unit == "tokens"){
    dt[, .N, keyby = .(interval=cut(dt$ntoken, boundaries, dig.lab = 4))]
  } else if(unit == "char"){
    dt[, .N, keyby = .(interval=cut(dt$nchar, boundaries, dig.lab = 4))]
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}

average_length_by_date = function(ids = NULL, coll, level = "year", unit = "tokens"){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  dt <- coll$meta
  if(!is.null(ids)){dt <- dt[id %in% ids,]}
  if(unit == "tokens"){
    if(level == "year"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")))][order(year)]
    } else if(level == "quarter"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                quarter = ceiling(as.numeric(format(date, "%m"))/3))][order(year, quarter)]
    } else if(level == "month"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                month = as.numeric(format(date, "%m")))][order(year, month)]
    } else if(level == "week"){
      dt[, list(average_length = round(mean(ntokens), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                week = as.numeric(format(date, "%V")))][order(year, week)]
    } else{
      message("Only supports 'year', 'quarter', 'month' and 'week' based aggregation.")
    }
  } else if(unit == "char"){
    if(level == "year"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")))][order(year)]
    } else if(level == "quarter"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                     quarter = ceiling(as.numeric(format(date, "%m"))/3))][order(year, quarter)]
    } else if(level == "month"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                     month = as.numeric(format(date, "%m")))][order(year, month)]
    } else if(level == "week"){
      dt[, list(average_length = round(mean(nchar), 1)), by = list(year = as.numeric(format(date, "%Y")),
                                                                     week = as.numeric(format(date, "%V")))][order(year, week)]
    } else{
      message("Only supports 'year', 'quarter', 'month' and 'week' based aggregation.")
    }
  } else{
    message("Unit must be 'tokens' (default) or 'char' for characters")
  }
}

show_records = function(ids = NULL, coll, show_date = TRUE, show_text = TRUE, show_id = TRUE, show_tags = FALSE, show_header = FALSE, show_edit = FALSE){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    if (show_date){p_date <- paste0("[", coll$corpus[ids]$date, "] ")}
    else {p_date <-""}
    if (show_text){p_text <- as.vector(as.character(coll$corpus)[ids])}
    else {p_text <-""}
    if (show_id){p_id <- paste0(" (", names(coll$corpus[ids]), ")")}
    else {p_id <-""}
    if (show_tags){p_tags <- paste0("\n Tags: ", coll$meta$tags[coll$meta$id %in% ids])}
    else {p_tags <-""}
    if (show_header){p_header <- paste0("\n Header: ", coll$meta$tags_section[coll$meta$id %in% ids])} 
    else {p_header <-""}
    if (show_edit){p_edit <- paste0("\nbrowseURL('https://avisblatt.freizo.org/iiif/anno/", names(coll$corpus[ids]), "/edit')")} 
    else {p_edit <-""}
    output <- paste0(p_date, p_text, p_id, p_tags, p_header, p_edit, "\n\n")
    cat(output)
  }
}


show_wordcloud = function(ids = NULL, coll, remove = "", max_words = 200){
  stopifnot(inherits(coll, "Collection"))
  stopifnot(inherits(coll, "R6"))
  if(length(ids)==1){if(ids=="all"){ids <- coll$meta$id}}
  if(is.null(coll$corpus)){
    stop("Collection has been read with meta info only. Use just_meta = FALSE in read_collections/gather_collections to be able to search in texts")
  } else{
    corp <- corpus_subset(c_all$corpus, names(c_all$corpus) %in% ids)
    removal <- c(avis_stop(), remove)
    corp <- corp %>%
      tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
      tokens_remove(removal, min_nchar = 3)
    textplot_wordcloud(dfm(corp), max_words = max_words)
  }
}

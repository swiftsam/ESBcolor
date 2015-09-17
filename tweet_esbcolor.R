library(twitteR)
library(rvest)
library(data.table)
library(stringr)
library(lubridate)
library(yaml)

kCacheFilename <- "esb_sched.RData"
kCreds         <- yaml.load_file("esb.yml")[['esbcolor']]

GetColorSchedule <- function(cache.expire.hrs = 12,
                             append.cache = TRUE) {

  # return cached version if it exists and is fresh enough
  if(file.exists(kCacheFilename)){
    modified.time <- file.info(kCacheFilename)$mtime
    cache.expired <- modified.time < Sys.time() - cache.expire.hrs * 60 * 60

    if(!cache.expired) {
      load(kCacheFilename)
      return(sched)
    }
  }

  # load existing data if we're appending to it
  if(append.cache & file.exists(kCacheFilename)){
    load(kCacheFilename)
  }

  # scrape calendar page
  page  <- html("http://www.esbnyc.com/explore/tower-lights/calendar")

  # identify nodes with dates, urls, and descriptions
  raw.dates <- html_nodes(page, ".views-row .date-display-single")
  raw.urls  <- html_nodes(page, ".views-row a")
  raw.desc  <- html_nodes(page, ".views-row .lighting-desc")

  # compile into data.table
  sched <- data.table(date = html_text(raw.dates),
                      desc = html_text(raw.desc),
                      url  = html_attr(raw.urls, "href"))

  # format, transform, create variables
  sched[, event_date := as.Date(mdy(date))]
  sched[, date := NULL]
  sched[, desc := str_trim(desc)]
  sched[, scraped_datetime := Sys.time()]
  sched[, most_recent_scrape := scraped_datetime == pmax(scraped_datetime),
        by= event_date]

  # try to make a hash tag
  HashTagDesc <- function(desc){

    inds <- c(str_locate(desc, "in honor of")[2],
              str_locate(str_to_lower(desc), "anniversary of")[2],
              str_locate(desc, "in celebration of")[2],
              str_locate(desc, "to raise awareness for")[2],
              str_locate(desc, "in memory of")[2])

    if(length(na.omit(inds))) {
      ind.start <- max(na.omit(inds)) + 1
    } else {
      return(NULL)
    }

    desc <- str_replace_all(desc, "'", "’")

    if(!is.na(str_locate(desc, "’s")[2]) &
       str_locate(desc, "’s")[2] > ind.start) {
      ind.end <- str_locate(desc, "’s")[2]-2
    } else {
      ind.end <- str_length(desc)-1
    }

    event <- str_sub(desc, ind.start, ind.end)
    event <- str_replace_all(event, "the ", "")
    event <- str_replace_all(event, "The ", "")
    event <- str_replace_all(event, "its ", "")
    event <- str_replace_all(event, "[[:punct:]]", "")
    event <- str_trim(event)
    event <- str_split(event, "and")
    event <- c(unlist(sapply(event, str_replace_all, " ", "")))

    hash.tag <- paste(str_c("#",event), collapse=" ")
  }

  sched[, hashtag := HashTagDesc(desc), by=event_date]
  sched[is.na(hashtag), hashtag := ""]

  setkey(sched, event_date)
  # save to cachefile
  save(sched,file = kCacheFilename)
  return(sched)
}

ComposeTweet <- function(tweet.date = Sys.Date()){
  sched <- GetColorSchedule(cache.expire.hrs = 0)
  day <- sched[event_date == tweet.date & most_recent_scrape == TRUE]

  if(day[, .N] > 0){

    # start tweeet with the first 140 char of description
    tweet <- day[, str_sub(desc, 1,140)]

    # add a hashtag if we have room
    if(str_length(paste(tweet, day[, hashtag])) <= 140) {
      tweet <- paste(tweet, day[, hashtag])
    }

    # # add the url (which will be shortened to 22 chars) if we have room
    # if(str_length(tweet) + 22 <= 140){
    #   tweet <- paste(tweet, day[, url])
    # }
    return(tweet)
  } else {
    return(NULL)
  }
}

PostTweet <-function(tweet.date = Sys.Date(), test=FALSE){
  tweet.text <- ComposeTweet(tweet.date)

  if(is.null(tweet.text)){
    return(FALSE)
  } else {
    options(httr_oauth_cache=T)
    setup_twitter_oauth(consumer_key = kCreds$consumer_key,
                        consumer_secret = kCreds$consumer_secret,
                        access_token = kCreds$access_token,
                        access_secret = kCreds$access_secret)

    if(!test){
      tweet(tweet.text)
    } else {
      message(tweet.text)
    }
  }
}

PostTweet(Sys.Date())

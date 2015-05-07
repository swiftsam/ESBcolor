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

  # save to cachefile
  save(sched,file = kCacheFilename)
  return(sched)
}

ComposeTweet <- function(tweet.date = Sys.Date()){
  sched <- GetColorSchedule(cache.expire.hrs = 0)
  sched <- sched[event_date == tweet.date & most_recent_scrape == TRUE]

  if(sched[, .N] > 0){
    tweet <- sched[, paste(desc, url)]
  } else {
    return(NULL)
  }
}

PostTweet <-function(tweet.date = Sys.Date()){
  tweet.text <- ComposeTweet(tweet.date)

  if(is.null(tweet.text)){
    return(FALSE)
  } else {
    setup_twitter_oauth(consumer_key = kCreds$consumer_key,
                        consumer_secret = kCreds$consumer_secret,
                        access_token = kCreds$access_token,
                        access_secret = kCreds$access_secret)

    tweet(tweet.text)
  }
}

PostTweet(Sys.Date())

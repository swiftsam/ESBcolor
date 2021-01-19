library(yaml)
library(rvest)
library(magrittr)
library(twitteR)
library(stringr)
library(purrr)

page <- read_html("http://www.esbnyc.com/explore/tower-lights/calendar")

color_date <- page %>%
  html_node("div.today div.info h2") %>%
  html_text()

colors <- page %>%
  html_node("div.today div.info h3") %>%
  html_text()

reason <- page %>%
  html_node("div.today div.info div.field_description p") %>%
  html_text()

log_file_name <- file.path("tweets", paste0(format(Sys.Date(), "%Y%m%d"),".txt"))

if(!file.exists(log_file_name)){
  if(str_starts(color_date, "Today,")) {
    if(colors != "SIGNATURE WHITE COLOR") {

      tweet_safely <- safely(function() {

        tweet_color  <- colors %>% str_to_lower %>% str_remove(" color") %>% str_to_sentence()
        tweet_reason <- reason

        tweet_text   <- paste0(tweet_color, "\n\n", tweet_reason)

        creds <- read_yaml("ESBcolor.yaml")$esbcolor
        options(httr_oauth_cache=T)
        setup_twitter_oauth(consumer_key    = creds$consumer_key,
                            consumer_secret = creds$consumer_secret,
                            access_token    = creds$access_token,
                            access_secret   = creds$access_secret)
        tweet(tweet_text)

      })

      response <- tweet_safely()

      if(is.null(response$error)) {

        file_con <- file(log_file_name)
        writeLines(response$result$text, file_con)
        close(file_con)

      } else {
        message(response$error)
      }
    } else {
      message("just white lights")
    }
  } else {
    message("didn't find today's info")
  }
} else {
  message("already tweeted for today")
}


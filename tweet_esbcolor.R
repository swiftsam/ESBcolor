library(yaml)
library(rvest)
library(magrittr)
library(twitteR)
library(stringr)

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

log_file_name <- paste0(format(Sys.Date(), "%Y%m%d"),".txt")

if(!file.exists(log_file_name)){
  if(str_starts(color_date, "Today,")) {
    if(colors != "SIGNATURE WHITE COLOR") {

      tweet_color  <- colors %>% str_to_lower %>% str_remove(" color") %>% str_to_sentence()
      tweet_reason <- reason

      tweet_text   <- paste0(post_color, "\n\n", post_reason)

      options(httr_oauth_cache=T)
      creds <- read_yaml("ESBcolor.yaml")$esbcolor

      setup_twitter_oauth(consumer_key    = creds$consumer_key,
                          consumer_secret = creds$consumer_secret,
                          access_token    = creds$access_token,
                          access_secret   = creds$access_secret)
      status <- tweet(tweet_text)
    }
  }
}


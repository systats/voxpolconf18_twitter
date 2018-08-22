Vox-Pol Conf Twitter Analysis
================
Simon
2018-08-21

<!-- After all the interesting extremism research at #voxpolconf18 it's time to talk about the E X T R E M E L Y nice @VOX_Pol Twitter community. Here is some #dataviz about the conference tweets, enjoy :)  -->
<!-- Code: https://github.com/systats/voxpolconf18_twitter  -->
<!-- #rstats #ggraph @FabioFavusMaxim  -->
This is a short notebook scraping tweets related to the Vox-Pol Conference 2018 in Amsterdam. As this was again a very inspiring Vox-Pol event I thought it was time to further explore the twitter community.

Packages
--------

Load the necessary packages

``` r
# install pacman once if not avaible on your machine
# install.packages("pacman")
#devtools::install_github("sy")
pacman::p_load(tidyverse, purrr, tidyr, rtweet, stringr, ggraph, igraph, tidygraph, forcats)
```

Get Data
--------

Call Twitter API. If you want to scrape data yourself you have to register a free account where you get your personal access point to Twitter. Check out [`rtweet`](https://github.com/mkearney/rtweet/) on github and follow their instructions to the twitter authentication.

``` r
twitter_token <- readRDS("twitter_token.rds")

rt <- search_tweets(
  "#VOXPolConf18 OR #VOXPolConf2018", n = 2000, include_rts = T, retryonratelimit = T
)
save(rt, file = "rt.Rdata")
```

Lets first look at the data structure and column names. Twitter returns a huge amount of data.

``` r
rt %>% glimpse # the same as str, returns a df overview
```

    ## function (n, df, ncp)

The top ten retweeted tweets.

``` r
load("rt.Rdata")
rt %>% 
  select(screen_name, text, retweet_count) %>% 
  filter(!str_detect(text, "^RT")) %>% 
  mutate(text = str_replace_all(text, "\\\n", " ")) %>% 
  arrange(desc(retweet_count)) %>% 
  top_n(n = 10) %>% 
  knitr::kable(., format = "markdown")
```

<table style="width:100%;">
<colgroup>
<col width="5%" />
<col width="90%" />
<col width="4%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">screen_name</th>
<th align="left">text</th>
<th align="right">retweet_count</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">MubarazAhmed</td>
<td align="left">ISIS jihadis remain keen on returning to mainstream social media platforms for recruitment purposes, don’t just want to be talking to each other on Telegram, says <span class="citation">@AmarAmarasingam</span>. #voxpolconf18 <a href="https://t.co/JwQLqakmF6" class="uri">https://t.co/JwQLqakmF6</a></td>
<td align="right">28</td>
</tr>
<tr class="even">
<td align="left">intelwire</td>
<td align="left">It's happening!!! #voxpolconf18 <a href="https://t.co/XV7nWCG6vN" class="uri">https://t.co/XV7nWCG6vN</a></td>
<td align="right">21</td>
</tr>
<tr class="odd">
<td align="left">MubarazAhmed</td>
<td align="left">Fascinating findings presented by ⁦<span class="citation">@AmarAmarasingam</span>⁩ on languages used in Telegram communications by jihadi groups. Arabic remains integral to ISIS on Telegram, but there is also a surprisingly high level of activity in Persian. Cc: ⁦<span class="citation">@KasraAarabi</span>⁩ #voxpolconf18 <a href="https://t.co/7YVFUr2WBD" class="uri">https://t.co/7YVFUr2WBD</a></td>
<td align="right">19</td>
</tr>
<tr class="even">
<td align="left">FabioFavusMaxim</td>
<td align="left">Very excited to have presented our research on the Alt-Right with <span class="citation">@systatz</span> at #voxpolconf2018. Received some great suggestions by <span class="citation">@miriam_fs</span>. to improve our analysis, which is definitely something we'll implement. You can check out our slides here: <a href="https://t.co/uGV7es8VhF" class="uri">https://t.co/uGV7es8VhF</a> <a href="https://t.co/pz3113fPZN" class="uri">https://t.co/pz3113fPZN</a></td>
<td align="right">19</td>
</tr>
<tr class="odd">
<td align="left">VOX_Pol</td>
<td align="left">We look forward to seeing many of you in Amsterdam next week for #voxpolconf18. Most up-to-date version of the Conference Programme is at <a href="https://t.co/SLzRsN2y6E" class="uri">https://t.co/SLzRsN2y6E</a> and also below. <a href="https://t.co/aw5IdEWcRD" class="uri">https://t.co/aw5IdEWcRD</a></td>
<td align="right">17</td>
</tr>
<tr class="even">
<td align="left">ErinSaltman</td>
<td align="left">Present &amp; future trends within violent extremism &amp; terrorism; new tech, new tactics, old problems, old groups. Pleasure &amp; privilege to share panel discussion with <span class="citation">@intelwire</span> <span class="citation">@techvsterrorism</span> <span class="citation">@p_vanostaeyen</span> moderated by <span class="citation">@VOX_Pol</span> <span class="citation">@galwaygrrl</span>. Big Qs at #voxpolconf18 !! <a href="https://t.co/fBnlrEe4c2" class="uri">https://t.co/fBnlrEe4c2</a></td>
<td align="right">17</td>
</tr>
<tr class="odd">
<td align="left">lizzypearson</td>
<td align="left">Really looking forward to <span class="citation">@VOX_Pol</span> Amsterdam conference where I'm talking UK Islamist offline reflections on online. Plus! seeing presentations by <span class="citation">@Swansea_Law</span> colleagues <span class="citation">@CTProject_JW</span> on online Jihadism in the US and <span class="citation">@CTP_ALW</span> on Britain First imagery in the UK #voxpolconf18 <a href="https://t.co/vGDsJgZM4I" class="uri">https://t.co/vGDsJgZM4I</a></td>
<td align="right">15</td>
</tr>
<tr class="even">
<td align="left">AmarAmarasingam</td>
<td align="left">Day 2: <span class="citation">@pieternanninga</span> talks about the dramatic drop in ISIS video releases from 2015 to 2018. #VOXpolconf18 <a href="https://t.co/ipJYXzXIUI" class="uri">https://t.co/ipJYXzXIUI</a></td>
<td align="right">14</td>
</tr>
<tr class="odd">
<td align="left">MoignKhawaja</td>
<td align="left">.<span class="citation">@AmarAmarasingam</span> giving a very interesting presentation on how jihadists are using <span class="citation">@telegram</span> as a platform for various purposes including propaganda dissemination here at .⁦<span class="citation">@VOX_Pol</span>⁩ #VOXPolConf2018 day 1 session 2 chaired by ⁦<span class="citation">@galwaygrrl</span>⁩ <a href="https://t.co/NOLTPDUn1G" class="uri">https://t.co/NOLTPDUn1G</a></td>
<td align="right">13</td>
</tr>
<tr class="even">
<td align="left">Drjohnhorgan</td>
<td align="left">Follow #VoxPolConf18 this week to learn about new research on terrorism, extremism and everything in between</td>
<td align="right">13</td>
</tr>
<tr class="odd">
<td align="left">MiloComerford</td>
<td align="left">Important corrective on online extremism from <span class="citation">@MubarazAhmed</span>’s research at #VOXPolConf18 - large proportion of traffic to extremist websites comes from searches, not social media. <span class="citation">@VOX_Pol</span> <a href="https://t.co/JVGIXouaa4" class="uri">https://t.co/JVGIXouaa4</a></td>
<td align="right">13</td>
</tr>
</tbody>
</table>

Timeline
--------

What was the best time to tweet?

``` r
rt %>%
  ## parse date format
  mutate(
    cdate = created_at %>% 
      str_extract("\\d{4}-\\d{2}-\\d{2}") %>% 
      lubridate::ymd(),
    hour = lubridate::hour(created_at)
  ) %>%
  ## select relevant time period
  filter(cdate >= as.Date("2018-08-19")) %>% 
  ## count tweet per and and hour
  group_by(cdate, hour) %>%
  tally %>%
  ungroup %>%
  ggplot(aes(hour, n)) +
  geom_line() +
  ## split the visualization 
  facet_wrap(~cdate, ncol = 2) +
  theme_minimal() +
  ggtitle("Number of Tweets by Day and Hour")
```

![](Readme_files/figure-markdown_github/unnamed-chunk-5-1.png)

Retweet Network
---------------

``` r
rt_graph <- rt %>% 
  ## select relevant variables
  dplyr::select(screen_name, mentions_screen_name) %>% 
  ## unnest list of mentions_screen_name
  unnest %>% 
  ## count the number of coocurences
  group_by(screen_name, mentions_screen_name) %>% 
  tally(sort = T) %>%
  ungroup %>% 
  ## drop missing values
  drop_na %>% 
  ## iflter those coocurences that appear at least 2 times
  filter(n > 1) %>% 
  ## transforming the dataframe to a graph object
  as_tbl_graph() %>% 
  ## calculating node centrality
  mutate(popularity = centrality_degree(mode = 'in'))

rt_graph %>% 
  ## create graph layout
  ggraph(layout = "kk") + 
  ## define edge aestetics
  geom_edge_fan(aes(alpha = n, edge_width = n, color = n)) + 
  ## scale down link saturation
  scale_edge_alpha(range = c(.5, .9)) +
  ## define note size param
  scale_edge_color_gradient(low = "gray50", high = "#1874CD") +
  geom_node_point(aes(size = popularity), color = "gray30") +
  ## define node labels
  geom_node_text(aes(label = name), repel = T, fontface = "bold") +
  ## equal width and height
  coord_fixed() +
  ## plain theme
  theme_void() +
  ## title
  ggtitle("#VOXPolConf18 Tweets and Retweets")
```

![](Readme_files/figure-markdown_github/unnamed-chunk-6-1.png)

Most Frequent Hashtags
----------------------

``` r
rt_hashtags <- rt %>% 
  select(hashtags) %>% 
  ## unnest list of hastags
  unnest %>% 
    na.omit %>% 
  ## clean hashtags
  mutate(hashtags = stringr::str_to_lower(hashtags) %>% 
           str_replace_all("2018", "18") %>% 
           ## add #symbol to vector
           paste0("#", .)) %>% 
  ## count each hashtag and sort
  count(hashtags, sort = T) %>% 
  filter(n > 2)

rt_hashtags %>% 
  filter(hashtags != "#voxpolconf18") %>%
  mutate(hashtags = forcats::fct_reorder(hashtags, n)) %>% 
  ggplot(aes(hashtags, n)) +
  geom_bar(stat = "identity", alpha = .7) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Most Frequent Hastags related to #voxpolconf18")
```

![](Readme_files/figure-markdown_github/unnamed-chunk-7-1.png)

Most Frequent Bigram Network
----------------------------

``` r
gg_bigram <- rt %>%
  select(text) %>% 
  ## remove text noise
  mutate(text = stringr::str_remove_all(text, "w |amp ")) %>% 
  ## remove retweets
  filter(!stringr::str_detect(text, "^RT")) %>% 
  ## remove urls
  mutate(text = stringr::str_remove_all(text, "https?[:]//[[:graph:]]+")) %>% 
  mutate(id = 1:n()) %>% 
  ## split text into words
  tidytext::unnest_tokens(word, text, token = "words") %>% 
  ## remove stop words
  anti_join(tidytext::stop_words) %>% 
  ## paste words to text by id
  group_by(id) %>% 
  summarise(text = paste(word, collapse = " ")) %>% 
  ungroup %>% 
  ## again split text into bigrams (word occurences or collocations)
  tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  ## count bigrams
  count(word1, word2, sort = T) %>% 
  ## select first 90
  slice(1:100) %>% 
  drop_na() %>%
  ## create tidy graph object
  as_tbl_graph() %>% 
  ## calculate node centrality
  mutate(Popularity = centrality_degree(mode = 'in'))
```

``` r
gg_bigram %>% 
  ggraph() +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(aes(size = Popularity)) + 
  geom_node_text(aes(label = name),  repel = TRUE) +
  theme_void() +
  scale_edge_alpha("", range = c(0.3, .6)) +
  ggtitle("Top Bigram Network form Tweets using hashtag #VOXPolConf18")
```

![](Readme_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
sessionInfo()
```

    ## R version 3.5.0 (2018-04-23)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.5
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] de_DE.UTF-8/de_DE.UTF-8/de_DE.UTF-8/C/de_DE.UTF-8/de_DE.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] bindrcpp_0.2.2    tidygraph_1.1.0   igraph_1.2.2     
    ##  [4] ggraph_1.0.1.9999 rtweet_0.6.20     forcats_0.3.0    
    ##  [7] stringr_1.3.1     dplyr_0.7.6       purrr_0.2.5      
    ## [10] readr_1.1.1       tidyr_0.8.1       tibble_1.4.2     
    ## [13] ggplot2_3.0.0     tidyverse_1.2.1  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] ggrepel_0.8.0     Rcpp_0.12.18      lubridate_1.7.4  
    ##  [4] lattice_0.20-35   deldir_0.1-15     assertthat_0.2.0 
    ##  [7] rprojroot_1.3-2   digest_0.6.15     psych_1.8.4      
    ## [10] ggforce_0.1.1     R6_2.2.2          cellranger_1.1.0 
    ## [13] plyr_1.8.4        backports_1.1.2   evaluate_0.10.1  
    ## [16] highr_0.6         httr_1.3.1        pillar_1.2.3     
    ## [19] rlang_0.2.1       lazyeval_0.2.1    readxl_1.1.0     
    ## [22] rstudioapi_0.7    Matrix_1.2-14     rmarkdown_1.9    
    ## [25] labeling_0.3      tidytext_0.1.8    foreign_0.8-70   
    ## [28] polyclip_1.9-1    munsell_0.5.0     broom_0.4.5      
    ## [31] janeaustenr_0.1.5 compiler_3.5.0    modelr_0.1.2     
    ## [34] pkgconfig_2.0.1   mnormt_1.5-5      htmltools_0.3.6  
    ## [37] openssl_1.0.1     tidyselect_0.2.4  gridExtra_2.3    
    ## [40] viridisLite_0.3.0 crayon_1.3.4      withr_2.1.2      
    ## [43] SnowballC_0.5.1   MASS_7.3-49       grid_3.5.0       
    ## [46] nlme_3.1-137      jsonlite_1.5      gtable_0.2.0     
    ## [49] pacman_0.4.6      magrittr_1.5      concaveman_1.0.0 
    ## [52] tokenizers_0.2.1  scales_1.0.0      cli_1.0.0        
    ## [55] stringi_1.2.4     farver_1.0        reshape2_1.4.3   
    ## [58] viridis_0.5.1     xml2_1.2.0        tools_3.5.0      
    ## [61] glue_1.3.0        tweenr_0.1.5.9999 hms_0.4.2        
    ## [64] parallel_3.5.0    yaml_2.1.19       colorspace_1.3-2 
    ## [67] rvest_0.3.2       knitr_1.20        bindr_0.1.1      
    ## [70] haven_1.1.2

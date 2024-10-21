Lab 7
================
Adrian Jauregui
2024-10-20

``` r
library(readr)
library(tidyverse)
library(stringr)
library(tidytext)
library(lubridate)
library(stopwords)
library(wordcloud)
```

``` r
df <- read_csv("Health_and_Personal_Care.csv")

df$text <- str_replace_all(df$text, pattern = '\\"', replacement = '')

df %>% head()
```

    ## # A tibble: 6 × 8
    ##   rating title    text  product_id parent_id user_id timestamp verified_purchase
    ##    <dbl> <chr>    <chr> <chr>      <chr>     <chr>       <dbl> <lgl>            
    ## 1      4 12 mg i… This… B07TDSJZMR B07TDSJZ… AFKZEN…   1.58e12 TRUE             
    ## 2      5 Save th… Love… B08637FWWF B08637FW… AEVWAM…   1.60e12 TRUE             
    ## 3      5 Fantast… I ha… B07KJVGNN5 B07KJVGN… AHSPLD…   1.56e12 TRUE             
    ## 4      4 It hold… It's… B007HY7GC2 B092RP73… AEZGPL…   1.66e12 TRUE             
    ## 5      1 Not for… Didn… B08KYJLF5T B08KYJLF… AEQAYV…   1.64e12 TRUE             
    ## 6      5 Every h… I ha… B09GBMG83Z B09GBMG8… AFSKPY…   1.65e12 FALSE

``` r
meta <- read_csv("Health_and_Personal_Care_metadata.csv")
meta %>% head()
```

    ## # A tibble: 6 × 8
    ##   main_category title average_rating rating_number price store details parent_id
    ##   <chr>         <chr>          <dbl>         <dbl> <dbl> <chr> <chr>   <chr>    
    ## 1 Health & Per… Sili…            3.9             7  NA   Rzoe… 15 x 3… B07V346G…
    ## 2 Health & Per… iPho…            3.8             2  NA   ZHXIN ZHXIN,… B075W927…
    ## 3 Health & Per… Zig …            3.9             7  NA   <NA>  4.1 x … B01FB26V…
    ## 4 Health & Per… Stin…            4.1             6  21.4 Stin… Sting-… B01IAI29…
    ## 5 Health & Per… Heat…            3.3             8  NA   BiBO… 6.1 x … B08CMN38…
    ## 6 Health & Per… Ball…            4.6            19  NA   Tikt… Bachel… B07YJ5JB…

``` r
words <- c(
  "love"
  ,"recommend"
  ,"enjoy"
)

positive_words <- paste(words, collapse = '|')

positive <- df %>%
  filter(str_detect(string = df$text,pattern = positive_words)) %>% 
  distinct(product_id)%>%
  count(name = "products_with_positive_review") 

positive
```

    ## # A tibble: 1 × 1
    ##   products_with_positive_review
    ##                           <int>
    ## 1                         23180

``` r
filtrados <- df %>%
  filter(str_detect(string = text, pattern = positive_words)) %>%
  distinct(product_id,parent_id)

tiendas <- filtrados %>%
  inner_join(meta, by = "parent_id") %>%
  filter(!is.na(store)) %>%
  group_by(store) %>%
  summarise(total_products = n_distinct(product_id)) %>%
  arrange(desc(total_products)) %>%
  slice_max(total_products, n = 5)%>%
  select(store)

tiendas
```

    ## # A tibble: 5 × 1
    ##   store      
    ##   <chr>      
    ## 1 HAARBB     
    ## 2 Eyekepper  
    ## 3 US Organic 
    ## 4 Andaz Press
    ## 5 Generic

``` r
positive2 <- df %>%
  filter(str_detect(string = df$text,pattern = positive_words)) %>% 
  distinct(product_id,text)%>%
  select(text)

stop_words <- c(stopwords(language = "en"), stopwords(language = "es"))

words <- str_split(positive2$text[1:100], boundary("word")) %>% unlist()

no_stopwords <- words %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_words)) %>% 
  group_by(value) %>% 
  summarise(freq = n())

wordcloud(no_stopwords$value, no_stopwords$freq)
```

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B08NT3QPMM could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): design could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): production could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): I could not be fit
    ## on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): number could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): difference could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): PACKAGE could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): inflammation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): accuracy could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): After could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): minutes could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): individually could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): connection could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Urion could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): expensive could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pores could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Pressure could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): SET could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): adjustment could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): minute could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): simple could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): connect could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): head could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): PANACARE could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): difficult could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): minus could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): arms could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): scent could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): heartbeat could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sources could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): seconds could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 543 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): perfect could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): box could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): close could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): supposedly could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): get could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): reading could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): voice could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): never could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): longer could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): people could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): single could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): program could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): favorite could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): indicator could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): They could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): supplements could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): manual could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): regular could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stores could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quirky could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): known could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): next could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): plastic could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 16.535 could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Setting could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Checkme could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): rechargeable could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): long could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): purchase could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Difini could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): trying could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cloth could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09GVR6X6W could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): natural could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): BP could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): flavor could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bag could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): soap could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hour could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): replace could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): provided could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Therefore could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hours could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Each could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09WDK7P8Y could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): portability could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bought could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quality could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pulse could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): readout could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): digital could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): listed could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): money could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Love could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): storage could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): wonderful could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): AQESO could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): instruction could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): right could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B08BC1DXHF could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): keep could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): br could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): error could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): weeks could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): working could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): notes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): thing could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): brush could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): reminder could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): medium could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): notation could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): needed could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): made could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Series could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): soft could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): function could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): takes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): area could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): current could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): larger could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): looking could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B01AFQK85K could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): instead could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): way could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): change could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ModelBP2A could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Baymed could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): machine could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): KIUZOU could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): options could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): automated could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): without could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): wrist could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): muscle could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): helps could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): typical could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Electronic could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): smoothie could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Basic could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): might could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): high could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): matter could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): side could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ONE could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pressed could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stomach could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mention could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Guidance could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): style could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): HealthSmart could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): coffee could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): second could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09NN8VS8G could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): range could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): There could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cheap could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pamphlet could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): displayed could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Femometer could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Chocolate could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ones could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): total could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): least could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 198 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): keeps could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): guidance could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): move could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): two could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): video could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Spanish could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): points could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hair could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): take could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): VIDEOID could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): health could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): say could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Store could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): back could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Up could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): averages could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): rings could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): battery could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09F3HTWYM could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): type could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 2022 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hope could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): haze could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): background could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): extremely could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): reasonably could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): results could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): well could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): four could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): try could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sturdy could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): adhere could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Bluetooth could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): monitors could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bath could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): information could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quite could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stimulation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): gift could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): powder could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): carrying could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09DPPF2ZD could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): crashing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): That could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): old could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): per could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): comprehensive
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): repeatedly could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): forget could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): shorter could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): various could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B08QH16BVY could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): included could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): arthritis could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 6V could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): heavy could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): better could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): There’s could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stars could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cuff could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): female could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): micro could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): priced could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): body could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): PM2.5 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): winter could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): moderated could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): case could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bags could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): neither could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Overall could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): caused could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): average could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): socks could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Date could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): absolutely could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): else could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): recommended could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): taking could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): accent could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): junk could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): feel could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): certain could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): feature could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mark could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B07G3D6BVG could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mode could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quickly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lights could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): shower could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 120 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): large could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): yellow could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Reminder could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): upside could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): wood could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Chinese could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): iOS could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Press could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): default could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Cuff could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): recall could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): The could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): fact could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): essential could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): I've could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): go could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): accented could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): noise could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): taken could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hold could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 240 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): times could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): designation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): dosage could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): diastolic could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): teach could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): spot could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): orange could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bottom could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ideal could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): simply could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): just could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): unless could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): remembered could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): compared could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): already could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 4 could not be fit
    ## on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): muscular could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B085VN5DQB could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): reader could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): recommend could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): effective could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bombs could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): EXCELLENT could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lower could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 24 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 2 could not be fit
    ## on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): color could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): USB could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): powered could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): FEATURES could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Voice could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): couple could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): getting could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Arm could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): features could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): comfortable could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ANJOCARE could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09JYND7QX could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): goes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Nevertheless could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): away could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pressures could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pairing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): based could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sure could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): packaged could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): really could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): even could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): A could not be fit
    ## on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Model could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): tested could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cherry could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Large could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): drops could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): viHealth could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Only could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): days could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): controls could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): look could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Note could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Highly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): uncomfortable
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): turn could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): reads could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): summary could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): selecting could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09WGYJRH8 could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): along could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): unknown could not
    ## be fit on page. It will not be plotted.

![](LABORATORIO-7-AJ_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
tiendas <- c("HAARBB","Eyekepper","US Organic","Andaz Press","Generic")

parents <- meta %>%
  filter(store %in% tiendas) %>%
  select(parent_id)

tabla <- df %>%
  inner_join(parents, by = "parent_id") %>%
  select(text)

words2 <- str_split(tabla$text[1:100], boundary("word")) %>% unlist()

no_stopwords2 <- words2 %>% 
  as_tibble() %>% 
  filter(!(value %in% stop_words)) %>% 
  group_by(value) %>% 
  summarise(freq = n())

wordcloud(no_stopwords2$value, no_stopwords2$freq)
```

![](LABORATORIO-7-AJ_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
general <- str_split(df$text, boundary("word")) %>%
  unlist()

no_stopwords3 <- general %>%
  as_tibble() %>%
  filter(!(value %in% stop_words)) %>%
  group_by(value) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) 

general_25 <- no_stopwords3 %>%
  slice_max(freq, n = 25)

general_25
```

    ## # A tibble: 25 × 2
    ##    value     freq
    ##    <chr>    <int>
    ##  1 I       590669
    ##  2 br      136733
    ##  3 product 100610
    ##  4 The      94876
    ##  5 It       81423
    ##  6 use      80636
    ##  7 like     76352
    ##  8 great    71485
    ##  9 This     69662
    ## 10 one      64936
    ## # ℹ 15 more rows

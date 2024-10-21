Lab 7
================
Adrián Jáuregui
2024-10-21

``` r
library(readr)
library(tidyverse)
library(stringr)
library(tidytext)
library(lubridate)
library(stopwords)
library(wordcloud)

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

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): female could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): supposedly could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): compartments could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): front could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): unverified could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): powered could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): nightlight could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09JYND7QX could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): change could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): two could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): made could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): great could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): portability could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): assist could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): almost could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): consider could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): absolutely could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stores could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): rechargeable could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): takes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): support could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): probably could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): plastic could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): It's could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): exercise could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Urion could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): variety could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): outstanding could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): INCLUDED could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): always could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Etekcity could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): scent could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): version could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): repeatedly could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): controls could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): high could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): product could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): default could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09DPPF2ZD could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 240 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Also could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): speech could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pressing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): systolic could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): everything could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): turned could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): taken could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): least could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): basic could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ONE could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ever could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): That could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): probiotics could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ERR4 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): included could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): anyone could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): now could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): read could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B07G3D6BVG could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): goods could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): You could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): machine could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): daily could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): wooden could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09KXD6SNP could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): includes could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): writing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): After could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): However could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): suitable could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): background could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): User could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): adjustment could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hours could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): many could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): highly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): online could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Cable could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): second could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): URBEST could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): family could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): around could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): slightly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): head could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bar could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): competitive could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ensure could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sugar could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): skincare could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): immune could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): favorite could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): multiple could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): wrapped could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): AVERAGED could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stick could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): button could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): nose could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 1by could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): kept could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): BP could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): notation could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): products could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hair could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): display could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): excellent could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): I've could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): AVERAGE could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): numbers could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): enough could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): heavily could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): dry could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): instruction could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): treatment could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): else could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Position could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Timer could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mask could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Accuracy could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): designation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pricey could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09WGYJRH8 could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): capsule could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): people could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Chinese could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Most could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): say could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): means could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): months could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): working could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): problem could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): tested could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): caused could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): depending could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B08KXWKDDM could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bottles could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): issues could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): acceptable could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): videos could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lovely could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): half could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): glove could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Setting could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): function could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): brand could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): often could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): smaller could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mostly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): amount could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): handle could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): purchased could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): app could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ankle could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): rings could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): minute could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): come could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): personally could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): features could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): previously could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): smells could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Nevertheless could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): individually could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): looking could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ear could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): since could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): smell could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ordered could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): diastolic could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): obese could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): monitor could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): XUMAO could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): important could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): NEGATIVES could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): tea could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): like could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): secure could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): unless could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): works could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): digital could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): feet could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): close could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): easily could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Ziqing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): based could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Aleshon could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): error could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): settings could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): shape could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): arch could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): recommend could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): size could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): slots could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): enjoy could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): large could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): regular could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): color could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): accented could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): deleting could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): red could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): inflammation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): charging could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): spot could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): heartbeat could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): throw could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): averages could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cheap could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): provided could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): may could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): performance could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hard could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): crashing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Sejoy could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): operate could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): store could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): short could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Omron could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): adhere could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): comprehensive
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): switch could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quite could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): C.P could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): better could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): What could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): volume could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Correct could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): coffee could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): needed could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lights could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): One could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): fits could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): fact could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): matter could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Famidoc could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): speaking could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): turns could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B08HK71G16 could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cycle could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Talking could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): brushes could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): much could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): vials could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Asobilor could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): They could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Reminder could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): expect could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): another could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cup could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): decrease could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B085VN5DQB could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lids could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): tight could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): averaged could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): pressure could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): This could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): day could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): attractive could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): C could not be fit
    ## on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): members could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): LEFT could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): style could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): single could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B0995WJ42M could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): care could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bombs could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): nature could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): record could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): paper could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): holding could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): provides could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Operation could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): minus could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Batteries could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): set could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Electronic could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): eyes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): supplement could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): attached could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 90 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): arm could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): quickly could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): room could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): clear could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 16.535 could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09WDK7P8Y could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): neither could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): box could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): affixed could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): small could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Therefore could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): compared could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): displaying could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): protein could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 180 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): accurate could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): a.k.a could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Switching could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): mark could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): dissolves could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): date could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B09GVR6X6W could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): monitors could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ribbon could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): DEAD could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): put could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): difference could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): variable could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ModelBP2A could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): stars could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): hand could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): powder could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): thing could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): separately could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Once could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Android could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Unfortunately
    ## could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): less could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): however could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): press could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): modes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): cleaning could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): moderated could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): effective could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): turn could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): B08GCPJ8C1 could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): vitamins could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): playing could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 120 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): plus could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): decent could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): device could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): blocks could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): number could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): center could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ON could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): lightweight could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): looks could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Baymed could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): sound could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): design could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): 4.5 could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): star could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Difini could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): goes could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): palm could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): item could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ASIN could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): ply could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): successful could
    ## not be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): trying could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): unique could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): muscle could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): couple could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): English could not
    ## be fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Large could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): big could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): blend could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): away could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): Note could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): bomb could not be
    ## fit on page. It will not be plotted.

    ## Warning in wordcloud(no_stopwords$value, no_stopwords$freq): side could not be
    ## fit on page. It will not be plotted.

![](Lab7_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

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

![](Lab7_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

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

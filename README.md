
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bundeslaendeR

<!-- badges: start -->
[![R-CMD-check](https://github.com/RStelzle/bundeslaendeR/workflows/R-CMD-check/badge.svg)](https://github.com/RStelzle/bundeslaendeR/actions)
<!-- badges: end -->

The goal of bundeslaendeR is to …

Dies ist ein Test.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RStelzle/bundeslaendeR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyverse)

library(bundeslaendeR)


ltw_election_results %>% 
  select(state_name_en, election_date, partyname_short, party_vshare) %>% 
  mutate(partyname_short = case_when(
    partyname_short %in% c("SPD", "FDP", "Grüne", "Linke", "AfD") ~ partyname_short,
    partyname_short %in% c("CDU", "CSU") ~ "CDU/CSU", 
    TRUE ~ "Others"
         )) %>% 
  mutate(partyname_short = as_factor(partyname_short) %>% 
                            fct_relevel("CDU/CSU", "SPD", "FDP", "Grüne", "Linke", "AfD", "Others")) %>%
  mutate(state_name_en = str_replace(state_name_en, "former state ", "Former state\n")) %>% 
  group_by(state_name_en, election_date, partyname_short) %>% 
  summarise(party_vshare = sum(party_vshare)) %>% 
  ungroup() %>% 
  ggplot(aes(x = election_date, y = party_vshare, col = partyname_short)) +
    geom_point() +
    geom_line() +
    facet_wrap(~state_name_en) +
    scale_color_manual(values = c("black", "#E3000F", "#ffed00", "#51bc4a", "#b47ab5", "#00adef", "grey"),
                       guide = guide_legend(nrow = 1)) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"),
          legend.position = "bottom") +
    labs(x = NULL,
         y = "Party Voteshare",
         col = NULL)
#> `summarise()` has grouped output by 'state_name_en', 'election_date'. You can override using the `.groups` argument.
#> geom_path: Each group consists of only one observation. Do you need to adjust
#> the group aesthetic?
#> geom_path: Each group consists of only one observation. Do you need to adjust
#> the group aesthetic?
```

<img src="man/figures/README-example-1.png" width="100%" />

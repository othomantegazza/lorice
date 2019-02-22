library(tidyverse)
library(httr)
# library(readxl)
library(rvest)

load("data/nomi_comuni.Rdata")

reqs <- 
  cds2 %>% 
  map_chr(~paste0("https://www.enterisi.it/servizi/seriestoriche/superfici_fase01.aspx?",
                  "fn=582&Campo_16523=57&Campo_16594=0&Campo_16571=0&Campo_16547=",
                  .,
                  "&Campo_16617=0&Campo_16640=0&AggiornaDB=Cerca&esporta=true"))

# read_excel("https://www.enterisi.it/servizi/seriestoriche/superfici_fase01.aspx?fn=582&Campo_16523=57&Campo_16594=0&Campo_16571=0&Campo_16547=595&Campo_16617=0&Campo_16640=0&AggiornaDB=Cerca&esporta=true")

names(reqs) %>% 
  map(~httr::GET(url = reqs[.], 
                 write_disk(paste0("data/produzione-enterisi/",
                                   .,
                                   ".xlsx"))))

r <- 
  httr::GET(url = "https://www.enterisi.it/servizi/seriestoriche/superfici_fase01.aspx?fn=582&Campo_16523=57&Campo_16594=0&Campo_16571=0&Campo_16547=595&Campo_16617=0&Campo_16640=0&AggiornaDB=Cerca&esporta=true",
            write_disk("test.xlsx"))

content(r) %>% read_excel()


# collapse dataset --------------------------------------------------------

# The files are named xlsx, but they actually store html tables
read_ricefile <- function(path) 
{
  read_html(path) %>%
    html_table(header = TRUE) %>% 
    .[[1]] %>% 
    as_tibble() %>% 
    mutate(acri = `Ha totali` %>% str_replace(",", ".") %>% as.numeric()) %>% 
    select(-`Ha totali`)
}

read_ricefile("data/produzione-enterisi/ABBIATEGRASSO.xlsx")

xsl_files <- 
  list.files(path = "data/produzione-enterisi") %>% 
  {set_names(.,
             nm = str_replace(., ".xlsx", ""))} %>% 
  map(~paste0("data/produzione-enterisi/", .)) %>% 
  map(read_ricefile)

rice_prod <- 
  names(xsl_files) %>% 
  map(~mutate(xsl_files[[.]], COMUNE = .)) %>% 
  reduce(bind_rows)

save(rice_prod, file = "data/enterisi-riceprod.Rdata")  

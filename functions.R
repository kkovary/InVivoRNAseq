
formatHTML <- function(origFile, newFile){
  
  # This function reformats an HTLM file produced by Rmarkdown
  # so that it's compatible with a Shiny app that uses navbarMenu.
  
  require(magrittr)
  require(xml2)
  require(rvest)
  
  xml2::read_html(origFile) %>% 
    rvest::html_node('body') %>% 
    xml2::write_html(newFile)
  
}


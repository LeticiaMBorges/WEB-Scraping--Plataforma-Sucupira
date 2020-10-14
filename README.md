# Web- Scraping Plataforma Sucupira
Construção da base de dados, via WEB Scraping, com as informações das fichas de avaliação de cada programa, relativa à avaliação da Quadrienal 2017. 

      # Pacotes

library(RSelenium)
library(xml2)
library(rvest)
library(stringr)
library(dplyr)


      # Função que calcula o número de programas disponíveis para a área de avaliação fornecida, bem como o número de páginas em que estes estão dipostos :
calculate.pages <- function() {
  
      # Conta o número de programas :
count.programs <- rd$getPageSource() %>% 
  unlist() %>%
  read_html() %>%
  html_node(xpath = '//*[@id="form:j_idt87:div_paginacao"]') %>%
  html_text(trim = TRUE) %>%
  str_match_all("[0-9]+") %>%
  unlist() %>%
  .[3] %>%
  as.numeric()
  
      # Conta o número de páginas em que estes programas estão dispostos :
count.pages <- 1 + count.programs %/% 50
  
      # Retorna as informações :
list("N_Programas" = count.programs, "N_paginas" = count.pages) %>% return()
  
}

      # Função responsável por extrair os links contidas nos seletores xpath :
extract.links <- function(xpath.vector) {
  
links.vector <- rep(0, times = length(xpath.vector))
  
      # Interação pelo vetor de xpaths com o objetivo de extrair os links :
for (i in 1:length(xpath.vector)) {
    
  links.vector[i] <-  rd$getPageSource() %>% 
  unlist() %>%
  read_html() %>%
  html_nodes(xpath = xpath.vector[i]) %>%
  as.character() %>%
  str_extract('(?<=href=\\")(.*?)(?=" )')
    
  links.vector[i] <- str_c("https://sucupira.capes.gov.br", links.vector[i])
    
  }
  
      # Retorna o vetor resposta :
return(links.vector)
}

      # Função que navega para a página do i-ésimo programa e extraí as informações de interesse :
navigate.to_pages <- function(page.count) {
  
      # Interage com todas páginas :
for (j in 1:page.count) {
    
  aux.page_count <- calculate.pages()
    
      # Extração dos seletores xpath das desta página :
  xpaths.selectors <- paste0('//*[@id="form:j_idt63"]/span[2]/div/div/table/tbody/tr[',1:50,']/td[5]/a')
    
      # Extrair os links contidos nestes seletores :
  xpaths.links <- extract.links(xpaths.selectors)
    
      # Variável auxiliar :
  temp.program_table <- list()
    
      # Interage com todos os links desta página :
  for (i in 1:length(xpaths.links)) {
      
      # Navega efetivamente para a página :
  rd$navigate(xpaths.links[i])
  Sys.sleep(4)
  paste0("Programa ",i," da página ",page.count) %>% print()
      
      # Extrai o código de identificação do programa :
  temp.program_id <- rd$getPageSource() %>% 
  unlist() %>%
  read_html() %>%
  html_node(xpath = '//*[@id="form"]/div/div/div/div/fieldset/div[2]/div[2]') %>%
  as.character()
      
      # Extrai a tabela com a nota final do programa :
  temp.program_table[[i]] <- rd$getPageSource() %>% 
  unlist() %>%
  read_html() %>%
  html_table('#form\\:j_idt149\\:content > table:nth-child(1)', header = T, trim = T, dec = ".")
      
  temp.program_table[[i]] <- temp.program_table[[i]][14] %>% as.data.frame()
      
      # Guardando as informações que nos interessam :
  Sucupira.Data[[j]][i] <- list("Pagina" = page.count, "ID" = temp.program_id, "Nota_Final" = temp.program_table)
      
}
    
      # Uma vez scrapeados todos os links da j-ésima página, forçamos o selenium retornar à home page do SUCUPIRA :
rd$navigate("https://sucupira.capes.gov.br/sucupira/public/consultas/avaliacao/consultaFichaAvaliacao.jsf")
Sys.sleep(4)
    
      # Interage com o campo Período de Avaliação :
we <- rd$findElement(using = "css", "#form\\:periodoAvaliacao")
we$clickElement()
we$sendKeysToElement(list(key="down_arrow",key="return")) 
    Sys.sleep(4)
    
      # Interage com o campo Área de Avaliação :
we <- rd$findElement(using = "css", "#form\\:autocompleteAreaAv\\:input")
we$clickElement()
we$sendKeysToElement(list("MATEMATICA / PROBABILIDADE E ESTATISTICA"))
Sys.sleep(4)
    
      # Clicka mais uma vez no anterior para fazer sumir a caixinha expandida em Área de Avaliação :
we <- rd$findElement(using = "css", "#form\\:autocompleteAreaAv\\:listbox")
we$clickElement()
    
      # Interage com o campo "Modalidade" :
we <- rd$findElement(using = "css", "#form\\:idModalidade")
we$clickElement()
we$sendKeysToElement(list(key="down_arrow", key="return"))
    
      # Interage com o botão consultar :
we <- rd$findElement(using = "xpath", '//*[@id="form:consultar"]')
we$clickElement()
Sys.sleep(2)
    
      # Entra no botão da próxima página 'j-1' vezes :
sapply(c(1:j), function(k) {
we <- rd$findElement(using = "xpath", '//*[@id="form:j_idt87:botaoProxPagina"]')
we$clickElement()
Sys.sleep(2)
})
    
} 
  
}


# Programa Principal :

      # Incializa o Selenium :
rs <<- rsDriver(browser = "firefox", port = 4444L)
rd <<- rs$client

      # Navega para a página principal do SUCUPIRA :
rd$navigate("https://sucupira.capes.gov.br/sucupira/public/consultas/avaliacao/consultaFichaAvaliacao.jsf")
Sys.sleep(4)

      # Interage com o campo Período de Avaliação :
we <- rd$findElement(using = "css", "#form\\:periodoAvaliacao")
we$clickElement()
we$sendKeysToElement(list(key="down_arrow",key="return")) 
Sys.sleep(4)

      # Interage com o campo Área de Avaliação :
we <- rd$findElement(using = "css", "#form\\:autocompleteAreaAv\\:input")
we$clickElement()
we$sendKeysToElement(list("MATEMATICA / PROBABILIDADE E ESTATISTICA")) #Exemplo
Sys.sleep(4)

      # Clicka mais uma vez no anterior para fazer sumir a caixinha expandida em Área de Avaliação :
we <- rd$findElement(using = "css", "#form\\:autocompleteAreaAv\\:listbox")
we$clickElement()

      # Interage com o campo "Modalidade" :
we <- rd$findElement(using = "css", "#form\\:idModalidade")
we$clickElement()
we$sendKeysToElement(list(key="down_arrow", key="return"))

      # Interage com o botão consultar :
we <- rd$findElement(using = "xpath", '//*[@id="form:consultar"]')
we$clickElement()
Sys.sleep(2)

      # Lista que guarda todas as nossas informações de interesse :
Sucupira.Data <<- list()

n.pages <- calculate.pages()

navigate.to_pages(page.count = n.pages$N_paginas)

#' Lê jurisprudência baixada por cjf_baixar_cjsg
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar diretório se não forneceu arquivos
#'
#' @return Tibble
#' @export
#'
cjf_ler_cjsg <- function(arquivos = NULL, diretorio = "."){


   if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern = "html$",full.names = TRUE)

  }


  pb <- progress::progress_bar$new(total = length(arquivos))


  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    x <- .x %>%
        xml2::read_html()

    titulo <- x %>%
          xml2::xml_find_all("//td[@class= 'titulo_doc']") %>%
          xml2::xml_text(trim = TRUE)


    doc <- stringr::str_extract(titulo,".+?(?=\\s\\d)")
    proc <- stringr::str_extract(titulo,"\\d.+")

    tipo <- x %>%
           xml2::xml_find_all("//span[contains(text(),'Tipo')]/../../following-sibling::tr/td") %>%
           xml2::xml_text(trim = TRUE)

    n <- length(tipo)

    numero <- x %>%
      xml2::xml_find_all("//span[contains(text(),'N\u00famero')]/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    outro_numero <- stringr::str_extract(numero,"(?<=\\s).+")
    numero <- stringr::str_extract(numero,"\\S+")


    classe <- x %>%
      xml2::xml_find_all("//span[contains(text(),'Classe')]/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)


    relator <- x %>%
      xml2::xml_find_all("//span[contains(text(),'Relator(a)')]/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    relator_convocado <- x %>%
      xml2::xml_find_all("//tbody") %>%
      xml2::xml_child(".//tr//span[text()='Relator convocado']/../../following-sibling::tr/td") %>%
      purrr::map(~xml2::xml_text(.x,trim= TRUE)) %>%
      unlist()


    origem <- x %>%
      xml2::xml_find_all("//span[contains(text(),'Origem')]/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    oj <- x %>%
      xml2::xml_find_all("//span[contains(text(),'\u00d3rg\u00e3o julgador')]/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    orgao_julgador <- rep(NA_character_, n)

    orgao_julgador[which(tipo=='Ac\u00f3rd\u00e3o')] <- oj

    data_julgamento <- x %>%
      xml2::xml_find_all("//span[text()='Data']/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE) %>%
      as.Date(format='%d/%m/%Y')

    data_publicacao <- x %>%
      xml2::xml_find_all("//span[text()='Data da publica\u00e7\u00e3o']/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)  %>%
      as.Date(format='%d/%m/%Y')

    fonte_publicacao <- x %>%
      xml2::xml_find_all("//span[text()='Fonte da publica\u00e7\u00e3o']/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    e <- x %>%
      xml2::xml_find_all("//span[text()='Ementa']/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    ementa <- rep(NA_character_,n)

    ementa[which(tipo == "Ac\u00f3rd\u00e3o")]<- e


    decisao <- x %>%
      xml2::xml_find_all("//span[text()='Decis\u00e3o']/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    t <- x %>%
      xml2::xml_find_all("//span[text()='Texto']/../../following-sibling::tr/td") %>%
      xml2::xml_text(trim = TRUE)

    texto_decisao <- rep(NA_character_,n)

    texto_decisao[which(tipo=="Ac\u00f3rd\u00e3o")] <- t

    tibble::tibble(doc,
                   proc,
                   tipo,
                   numero,
                   outro_numero,
                   classe,
                   relator,
                   relator_convocado,
                   origem,
                   orgao_julgador,
                   data_julgamento,
                   data_publicacao,
                   fonte_publicacao,
                   ementa,
                   decisao,
                   texto_decisao)

},NULL))


}


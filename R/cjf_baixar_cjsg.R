#' Baixar jurisprudência da justiça federal
#'
#' @param livre Busca livre
#' @param aspas Colocar entre aspas?
#' @param numero Número
#' @param classe Classe
#' @param relator Relator
#' @param revisor Revisor
#' @param relator_convocado Relator convocado
#' @param relator_acordao  Relacor do acórdão
#' @param ementa_decisao  Busca na ementa ou decisão
#' @param referencia_legislativa Referência legislativa
#' @param data_inicial Data inicial no formato "dd/mm/aaaa"
#' @param data_final  Data final no formato "dd/mm/aaaa"
#' @param tipo Datas por "julgamento" ou "publicacao". Default: julgamento
#' @param tribunal informar tribunal, e.g, "trf1" ou "stf"
#' @param diretorio Diretório onde armazenar arquivos
#'
#' @return html
#' @export
#'
cjf_baixar_cjsg <- function(
                            livre = "",
                            aspas = FALSE,
                            numero = "",
                            classe = "",
                            relator = "",
                            revisor = "",
                            relator_convocado = "",
                            relator_acordao = "",
                            ementa_decisao = "",
                            referencia_legislativa = "",
                            data_inicial = "",
                            data_final = "",
                            tipo = c("julgamento","publicacao"),
                            tribunal = NULL,
                            diretorio = "."
                            ){


  if (aspas == TRUE){

    livre <- deparse(livre)
  }

  tipo <- tipo[[1]]

  if (tipo =="julgamento"){

    tipo <- "DTDP"

  } else {

    tipo <- "DTPP"
  }

  tribunal <- toupper(tribunal)

  #data_inicial <- stringr::str_remove(data_inicial,"\\d{20}(?=\\d{2}$)")
  #data_final <- stringr::str_remove(data_final,"\\d{20}(?=\\d{2}$)")


  url1 <- "https://www2.cjf.jus.br/jurisprudencia/unificada/"


  ViewState <- httr::RETRY("GET", url1, httr::timeout(30)) %>%
    httr::content() %>%
    xml2::xml_find_first("//form/input[@type='hidden'][2]") %>%
    xml2::xml_attr("value")


  url2 <- "https://www2.cjf.jus.br/jurisprudencia/unificada/index.xhtml"



  body1 <-
    list(
      javax.faces.partial.ajax = "true",
      javax.faces.source = "formulario:ckbAvancada",
      javax.faces.partial.execute = "formulario:ckbAvancada",
      javax.faces.partial.render = "formulario:pesquisaAvancada",
      javax.faces.behavior.event = "change",
      javax.faces.partial.event = "change",
      formulario = "formulario",
      `formulario:textoLivre` = "",
      `formulario:ckbAvancada_input` = "on",
      `formulario:j_idt42` = "TNU",
      javax.faces.ViewState = ViewState
    )

  resposta <- httr::POST(url1, body = body1, encode  = "form")


  inicial <- "0"

  body2 <-
    list(
      javax.faces.partial.ajax = "true",
      javax.faces.source = "formulario:actPesquisar",
      javax.faces.partial.execute = "@all",
      javax.faces.partial.render = "formulario:resultado",
      `formulario:actPesquisar` = "formulario:actPesquisar",
      formulario = "formulario",
      `formulario:textoLivre` = livre,
      `formulario:ckbAvancada_input` = "on",
      `formulario:j_idt19` = numero,
      `formulario:combo_classes` = classe,
      `formulario:j_idt22` = relator,
      `formulario:j_idt24` = revisor,
      `formulario:j_idt26` = relator_convocado,
      `formulario:j_idt28` = relator_acordao,
      `formulario:j_idt30` = ementa_decisao,
      `formulario:j_idt32` = referencia_legislativa,
      `formulario:j_idt34_input` = data_inicial,
      `formulario:j_idt36_input` = data_final,
      `formulario:combo_tipo_data_focus` = "",
      `formulario:combo_tipo_data_input` = tipo,
      `formulario:j_idt42` = tribunal,
      `javax.faces.ViewState` = ViewState
    )



  body3 <-
    list(
      javax.faces.partial.ajax = "true",
      javax.faces.source = "formulario:tabelaDocumentos",
      javax.faces.partial.execute = "formulario:tabelaDocumentos",
      javax.faces.partial.render = "formulario:tabelaDocumentos",
      `formulario:tabelaDocumentos` = "formulario:tabelaDocumentos",
      `formulario:tabelaDocumentos_pagination` = "true",
      `formulario:tabelaDocumentos_first` = inicial,
      `formulario:tabelaDocumentos_rows` = "50",
      formulario = "formulario",
      `formulario:textoLivre` = livre,
      `formulario:ckbAvancada_input` = "on",
      `formulario:j_idt19` = "",
      `formulario:combo_classes` = classe,
      `formulario:j_idt22` = relator,
      `formulario:j_idt24` = revisor,
      `formulario:j_idt26` = relator_convocado,
      `formulario:j_idt28` = relator_acordao,
      `formulario:j_idt30` = ementa_decisao,
      `formulario:j_idt32` = referencia_legislativa,
      `formulario:j_idt34_input` = data_inicial,
      `formulario:j_idt36_input` = data_final,
      `formulario:combo_tipo_data_focus` = "",
      `formulario:combo_tipo_data_input` = tipo,
      `formulario:j_idt42` = tribunal,
      `formulario:j_idt64_scrollState` = "0,0",
      `formulario:tabelaDocumentos_rppDD` = "50",
      `formulario:tabelaDocumentos:30:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:31:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:32:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:33:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:34:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:35:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:36:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:37:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:38:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:39:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:40:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:41:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:42:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:43:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:44:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:45:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:46:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:47:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:48:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:49:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:50:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:51:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:52:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:53:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:54:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:55:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:56:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:57:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:58:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos:59:j_idt246_activeIndex` = "0",
      `formulario:tabelaDocumentos_rppDD` = "50",
      javax.faces.ViewState = ViewState
    )

  n <- NA_real_

  while (is.na(n)) {
    n <- httr::RETRY("POST", url2, body = body2, encode = "form", httr::timeout(30)) %>%
      httr::content("text") %>%
      xml2::read_html() %>%
      xml2::xml_find_first("//span[@class='ui-paginator-current']") %>%
      xml2::xml_text(trim = T) %>%
      stringr::str_extract("\\d+(?=,)") %>%
      as.numeric()
  }


  inicial <- seq(0, n, 50)


   pb <- progress::progress_bar$new(total = length(inicial))

  purrr::walk(inicial, ~ {

    pb$tick()

    body2$`formulario:tabelaDocumentos_first` <- as.character(.x)

    arquivo <- paste0("_cfj_cjsg_pagina_", .x, ".html")

    httr::RETRY("POST", url2,
                body = body3, encode = "form", httr::timeout(30),
                httr::write_disk(file.path(diretorio, Sys.time() %>%
                                             stringr::str_replace_all("\\D+", "_") %>%
                                             stringr::str_replace("$", arquivo)))
    )
  })

}


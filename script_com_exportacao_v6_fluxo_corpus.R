# ============================================================================
# SCRIPT ÚNICO DE BIBLIOMETRIA  (v6.0 - fluxo guiado do corpus + parâmetros simplificados + referências harmonizadas)
# Esse script une um processo em duas etapas em um só fluxo guiado por janelas, compreendendo:
#
# Compilação das bases (Scopus .csv + WoS .txt), harmonização das referências citadas, deduplicação com preservação de dados e resumo PRISMA 
#  Exportações padrão do protocolo bibliométrico, em 2 fases, com pausa para a lista de sinônimos  
#
# COMO USAR (não é preciso saber R nem editar nada):
#
#   1. Abra no RStudio e execute o arquivo inteiro com Source (ou Run All / Cmd+Shift+Enter)
#   
#   2. ORIGEM DO CORPUS: o script pergunta explicitamente se você:
#      (1) já possui um arquivo final.rds; ou
#      (2) precisa compilar os arquivos brutos Scopus + WoS.
#      Se escolher (1), uma janela pedirá o arquivo .rds.
#      Se escolher (2), uma janela pedirá a PASTA com os arquivos brutos
#      (Scopus .csv e WoS .txt). O script compila, harmoniza referências,
#      remove duplicados, mostra o resumo PRISMA e salva final.rds/final.csv
#      nessa mesma pasta.
#
#   3. JANELA DE SINÔNIMOS: selecione o sinonimos.txt, se já tiver.
#      Na 1ª rodada, clique em CANCELAR: o script roda tudo que não depende
#      de palavras-chave, gera "keywords_frequencias.csv" e o prompt para
#      pedir a lista de sinônimos à IA de sua preferência.
#
#   4. Salve a resposta da IA como "sinonimos.txt", rode o script de novo
#      (agora escolhendo a opção 1, selecionando o final.rds e depois o .txt):
#      ele completa as análises de palavras-chave (WordCloud, Trend Topics,
#      Co-occurrence, Thematic Map, Thematic Evolution etc.).
#
# Todos os resultados vão para a pasta "exports_DATA", criada ao lado do
# corpus. Cada bloco é isolado, ou seja, se um falhar os demais
# continuam e o erro fica registrado em _log_erros.txt.
# ============================================================================

# ---------------------- INSTALAÇÃO AUTOMÁTICA DE PACOTES --------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))
pacotes <- c("bibliometrix", "writexl", "ggplot2", "htmlwidgets",
             "dplyr", "tidyr", "ineq", "rstudioapi")
faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(faltando) > 0) {
  message("Instalando pacotes que faltam: ", paste(faltando, collapse = ", "))
  install.packages(faltando, quiet = TRUE)
}
library(bibliometrix)
library(writexl)
library(ggplot2)
library(dplyr)
library(ineq)

# ===================== CONFIGURAÇÃO INTERATIVA INICIAL =====================
# No RStudio, os parâmetros são solicitados por caixas de diálogo NATIVAS do
# próprio RStudio (rstudioapi), sem Tcl/Tk/X11. Fora do RStudio, o console é
# usado como fallback. Pressionar OK sem alterar mantém o valor padrão.

em_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) &&
    isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))
}

perguntar_texto <- function(titulo, mensagem, padrao = "") {
  if (em_rstudio()) {
    resp <- tryCatch(
      rstudioapi::showPrompt(
        title = titulo,
        message = mensagem,
        default = as.character(padrao)
      ),
      error = function(e) NULL
    )
    # Cancelar não derruba o script: mantém o padrão.
    if (is.null(resp) || length(resp) == 0L || is.na(resp)) return(as.character(padrao))
    resp <- trimws(as.character(resp))
    if (!nzchar(resp)) return(as.character(padrao))
    return(resp)
  }

  resp <- trimws(readline(sprintf("%s [padrão: %s]: ", mensagem, padrao)))
  if (!nzchar(resp)) as.character(padrao) else resp
}

ler_numero <- function(pergunta, padrao, minimo = -Inf, maximo = Inf, inteiro = FALSE) {
  repeat {
    resp <- perguntar_texto("Parâmetros da análise", pergunta, padrao)
    valor <- suppressWarnings(as.numeric(gsub(",", ".", resp, fixed = TRUE)))

    valido <- !is.na(valor) && valor >= minimo && valor <= maximo &&
      (!inteiro || valor == round(valor))

    if (valido) return(if (inteiro) as.integer(valor) else valor)

    msg <- sprintf(
      "Valor inválido para '%s'. Informe um valor entre %s e %s.",
      pergunta, minimo, maximo
    )
    if (em_rstudio()) {
      try(rstudioapi::showDialog("Valor inválido", msg), silent = TRUE)
    } else {
      cat("  ", msg, "\n", sep = "")
    }
  }
}

ler_cortes <- function(pergunta, padrao) {
  repeat {
    resp <- perguntar_texto(
      "Parâmetros da análise",
      paste0(pergunta, "\nUse anos crescentes separados por vírgula."),
      paste(padrao, collapse = ",")
    )
    partes <- trimws(unlist(strsplit(resp, "[,; ]+")))
    valores <- suppressWarnings(as.integer(partes[nzchar(partes)]))

    if (length(valores) > 0L && !anyNA(valores) && all(diff(valores) > 0)) {
      return(valores)
    }

    msg <- "Formato inválido. Exemplo: 2005,2010,2015,2020"
    if (em_rstudio()) {
      try(rstudioapi::showDialog("Valor inválido", msg), silent = TRUE)
    } else {
      cat("  ", msg, "\n", sep = "")
    }
  }
}

mostrar_parametros_padrao <- function() {
  texto <- paste(
    "Antes das análises, você poderá confirmar ou alterar apenas os parâmetros principais.",
    "",
    "PADRÕES:",
    "Período: 2001–2025",
    "Cortes da Thematic Evolution: 2005, 2010, 2015, 2020",
    "Itens nos rankings: 20",
    "Itens por campo no Three-Field Plot: 10",
    sep = "\n"
  )

  if (em_rstudio()) {
    try(rstudioapi::showDialog("Configuração da análise", texto), silent = TRUE)
  } else {
    cat("\n", texto, "\n\n", sep = "")
  }
}

configurar_parametros <- function() {
  mostrar_parametros_padrao()

  # --------------------------------------------------------------------------
  # PARÂMETROS SOLICITADOS AO USUÁRIO
  # --------------------------------------------------------------------------
  ano_inicio <- ler_numero("Ano inicial das análises", 2001, 1, 9999, inteiro = TRUE)
  ano_fim <- ler_numero("Ano final das análises", 2025, 1, 9999, inteiro = TRUE)
  while (ano_fim < ano_inicio) {
    if (em_rstudio()) {
      try(rstudioapi::showDialog(
        "Período inválido",
        "O ano final não pode ser anterior ao ano inicial."
      ), silent = TRUE)
    } else {
      cat("  O ano final não pode ser anterior ao ano inicial.\n")
    }
    ano_fim <- ler_numero("Ano final das análises", 2025, ano_inicio, 9999, inteiro = TRUE)
  }

  cortes <- ler_cortes("Cortes da Thematic Evolution", c(2005, 2010, 2015, 2020))
  cortes <- cortes[cortes > ano_inicio & cortes < ano_fim]
  if (length(cortes) == 0L) {
    cortes <- unique(as.integer(round(seq(ano_inicio, ano_fim, length.out = 5))))
    cortes <- cortes[cortes > ano_inicio & cortes < ano_fim]
  }

  top_rank <- ler_numero("Número de itens nos rankings", 20, 1, 10000, inteiro = TRUE)
  threefield_n <- ler_numero("Itens por campo no Three-Field Plot", 10, 1, 1000, inteiro = TRUE)

  # --------------------------------------------------------------------------
  # PARÂMETROS TÉCNICOS FIXOS
  # Mantidos no script para garantir consistência e reprodutibilidade, mas não
  # são solicitados ao usuário na abertura.
  # --------------------------------------------------------------------------
  threshold <- 0.90
  nos_rede <- 50L
  freq_kw <- 5L
  autores_tempo <- 10L
  nos_paises <- 30L
  hist_n <- 30L
  kw_growth_top <- 10L
  trend_n_items <- 5L
  thematic_n <- 250L
  thematic_labels <- 3L

  data.frame(
    parametro = c(
      "ano_inicio_analise", "ano_fim_analise", "ref_match_threshold",
      "top_k", "n_net", "min_freq_kw", "periodos_thematic_evolution",
      "author_prod_k", "n_net_paises", "hist_n", "keyword_growth_top",
      "trend_n_items", "thematic_n", "thematic_labels", "threefield_n"
    ),
    valor = c(
      ano_inicio, ano_fim, threshold, top_rank, nos_rede, freq_kw,
      paste(cortes, collapse = ";"), autores_tempo, nos_paises, hist_n,
      kw_growth_top, trend_n_items, thematic_n, thematic_labels, threefield_n
    ),
    stringsAsFactors = FALSE
  )
}

PARAMETROS <- configurar_parametros()

valor_param <- function(nome, tipo = c("numeric", "integer", "character")) {
  tipo <- match.arg(tipo)
  v <- PARAMETROS$valor[PARAMETROS$parametro == nome][1]
  switch(tipo, numeric = as.numeric(v), integer = as.integer(v), character = as.character(v))
}

ano_inicio_analise <- valor_param("ano_inicio_analise", "integer")
ano_fim_analise <- valor_param("ano_fim_analise", "integer")
ref_match_threshold <- valor_param("ref_match_threshold", "numeric")
top_k <- valor_param("top_k", "integer")
n_net <- valor_param("n_net", "integer")
min_freq_kw <- valor_param("min_freq_kw", "integer")
periodos <- as.integer(strsplit(valor_param("periodos_thematic_evolution", "character"), ";", fixed = TRUE)[[1]])
author_prod_k <- valor_param("author_prod_k", "integer")
n_net_paises <- valor_param("n_net_paises", "integer")
hist_n <- valor_param("hist_n", "integer")
keyword_growth_top <- valor_param("keyword_growth_top", "integer")
trend_n_items <- valor_param("trend_n_items", "integer")
thematic_n <- valor_param("thematic_n", "integer")
thematic_labels <- valor_param("thematic_labels", "integer")
threefield_n <- valor_param("threefield_n", "integer")

resumo_parametros <- paste(
  sprintf("Período: %s–%s", ano_inicio_analise, ano_fim_analise),
  sprintf("Cortes da Thematic Evolution: %s", paste(periodos, collapse = ", ")),
  sprintf("Itens nos rankings: %s", top_k),
  sprintf("Itens por campo no Three-Field Plot: %s", threefield_n),
  sep = "\n"
)

if (em_rstudio()) {
  try(rstudioapi::showDialog(
    "Parâmetros definidos",
    paste0(resumo_parametros, "\n\nA análise será iniciada agora.")
  ), silent = TRUE)
} else {
  cat("\n============================================================\n")
  cat("PARÂMETROS DEFINIDOS PARA ESTA EXECUÇÃO\n")
  cat("============================================================\n")
  cat(resumo_parametros, "\n")
  cat("============================================================\n")
}
# ============================================================================
# ============================================================================

# ============================================================================
# FUNÇÕES AUXILIARES GERAIS - não mexer
# ============================================================================

# Escolher ARQUIVO sem Tcl/Tk: janela nativa do RStudio quando disponível;
# caso contrário, entrada pelo console.
# IMPORTANTE: execute o ARQUIVO INTEIRO (Source/Run All), não apenas a linha atual.
escolher_arquivo <- function(titulo, obrigatorio = TRUE) {
  message("\n>>> ", titulo)
  caminho <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    caminho <- tryCatch(rstudioapi::selectFile(caption = titulo), error = function(e) NULL)
  } else {
    caminho <- readline(paste0(titulo, " (digite o caminho, ou Enter para pular): "))
  }
  if (is.null(caminho) || length(caminho) == 0L || is.na(caminho) || !nzchar(trimws(caminho))) {
    if (obrigatorio) stop("Nenhum arquivo selecionado. Rode o script novamente.", call. = FALSE)
    return(NULL)
  }
  path.expand(trimws(caminho))
}

# Escolher PASTA sem Tcl/Tk.
escolher_pasta <- function(titulo = "Selecione a pasta") {
  message("\n>>> ", titulo)
  pasta <- NULL
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    pasta <- tryCatch(rstudioapi::selectDirectory(caption = titulo), error = function(e) NULL)
  } else {
    pasta <- readline(paste0(titulo, " (digite o caminho): "))
  }
  if (is.null(pasta) || length(pasta) == 0L || is.na(pasta) || !nzchar(trimws(pasta))) {
    stop("Nenhuma pasta selecionada. Operação cancelada.", call. = FALSE)
  }
  path.expand(trimws(pasta))
}

# Perguntar explicitamente se o usuário já possui o corpus final ou se precisa
# compilar as bases brutas. Sem Tcl/Tk: usa janela nativa do RStudio.
perguntar_origem_corpus <- function() {
  mensagem <- paste(
    "Como deseja obter o corpus para esta análise?",
    "",
    "1 = Já possuo o arquivo final.rds",
    "2 = Preciso compilar os arquivos brutos Scopus + WoS",
    "",
    "Digite 1 ou 2.",
    sep = "\n"
  )

  repeat {
    if (em_rstudio()) {
      resp <- tryCatch(
        rstudioapi::showPrompt(
          title = "Origem do corpus",
          message = mensagem,
          default = "1"
        ),
        error = function(e) NULL
      )
      if (is.null(resp) || length(resp) == 0L || is.na(resp)) {
        stop("Seleção da origem do corpus cancelada.", call. = FALSE)
      }
    } else {
      cat("\n", mensagem, "\n", sep = "")
      resp <- readline("Opção [1/2]: ")
    }

    resp <- trimws(as.character(resp))
    if (resp %in% c("1", "2")) return(resp)

    aviso <- "Opção inválida. Digite 1 para usar um final.rds ou 2 para compilar Scopus + WoS."
    if (em_rstudio()) {
      try(rstudioapi::showDialog("Opção inválida", aviso), silent = TRUE)
    } else {
      cat(aviso, "\n")
    }
  }
}

titulo_bloco <- function(t) {
  cat("\n============================================================\n",
      t, "\n============================================================\n", sep = "")
}

# Preparar data frame para CSV (resolve colunas do tipo list)
to_csv_safe <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  x[] <- lapply(x, function(coluna) {
    if (is.list(coluna)) {
      sapply(coluna, function(v) {
        if (is.null(v) || length(v) == 0) return(NA_character_)
        paste(as.character(v), collapse = "; ")
      })
    } else coluna
  })
  x
}

# Salvar CSV em UTF-8 COM BOM (acentos corretos no Excel, inclusive Mac)
salvar_csv_excel <- function(df, caminho) {
  con <- file(caminho, open = "wb")
  writeBin(charToRaw("\xEF\xBB\xBF"), con)
  close(con)
  suppressWarnings(write.table(df, file = caminho, append = TRUE, sep = ",",
                               row.names = FALSE, col.names = TRUE,
                               qmethod = "double", fileEncoding = "UTF-8"))
}

# ============================================================================
# FUNÇÕES DA COMPILAÇÃO (dedup sem perda de dados) - não mexer
# ============================================================================

contar_referencias <- function(cr) {
  cr <- as.character(cr)
  cr[is.na(cr) | trimws(cr) %in% c("", "NA")] <- ""
  vapply(strsplit(cr, ";", fixed = TRUE), function(x) {
    if (length(x) == 1L && !nzchar(trimws(x))) return(0L)
    sum(nzchar(trimws(x)))
  }, integer(1))
}

normalizar_doi <- function(x) {
  x <- tolower(trimws(as.character(x))); x[is.na(x)] <- ""
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi[[:space:]]*:[[:space:]]*", "", x)
  gsub("[[:space:]]", "", x)
}

normalizar_titulo <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^[:alnum:]]+", " ", tolower(x))
  trimws(gsub("[[:space:]]+", " ", x))
}

celula_vazia <- function(v) {
  is.na(v) | !nzchar(trimws(as.character(v)))
}

unir_lista_sem_duplicar <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x)) & trimws(x) != "NA"]
  if (length(x) == 0L) return(NA_character_)
  itens <- unlist(strsplit(x, ";", fixed = TRUE), use.names = FALSE)
  itens <- trimws(itens)
  itens <- itens[nzchar(itens) & itens != "NA"]
  if (length(itens) == 0L) return(NA_character_)
  paste(unique(itens), collapse = "; ")
}

obter_reference_matcher <- function() {
  ns <- asNamespace("bibliometrix")
  if (exists("applyReferenceMatching", envir = ns, inherits = FALSE)) {
    return(get("applyReferenceMatching", envir = ns, inherits = FALSE))
  }
  if (exists("applyCitationMatching", envir = ns, inherits = FALSE)) {
    warning("Sua versão do bibliometrix usa applyCitationMatching(). Considere atualizar o pacote; o script seguirá com a função compatível disponível.")
    return(get("applyCitationMatching", envir = ns, inherits = FALSE))
  }
  stop(
    "Sua versão do bibliometrix não possui a rotina de Reference Matching necessária. Atualize o pacote bibliometrix e rode o script novamente.",
    call. = FALSE
  )
}

harmonizar_referencias <- function(M, threshold = ref_match_threshold) {
  if (!all(c("SR", "CR") %in% names(M))) {
    stop("A base precisa conter os campos SR e CR para harmonizar as referências.", call. = FALSE)
  }

  # O matching usa SR para devolver as referências ao documento correto.
  # Como SR pode coincidir entre bases, criamos um identificador temporário único.
  M$.SR_original <- as.character(M$SR)
  M$.ref_id <- paste0("REFDOC_", seq_len(nrow(M)))
  mapa_origem <- data.frame(
    SR = M$.ref_id,
    SR_original = M$.SR_original,
    DB_origem = if ("DB" %in% names(M)) as.character(M$DB) else NA_character_,
    titulo_documento = if ("TI" %in% names(M)) as.character(M$TI) else NA_character_,
    ano_documento = if ("PY" %in% names(M)) as.character(M$PY) else NA_character_,
    DOI_documento = if ("DI" %in% names(M)) as.character(M$DI) else NA_character_,
    stringsAsFactors = FALSE
  )
  M$SR <- M$.ref_id

  # Preserva a referência exatamente como veio das bases para auditoria.
  if (!"CR_raw" %in% names(M)) M$CR_raw <- M$CR
  M$CR_raw <- as.character(M$CR_raw)
  M$CR <- as.character(M$CR)

  matcher <- obter_reference_matcher()
  matching <- matcher(
    M,
    threshold = threshold,
    method = "jw",
    min_chars = 20,
    max_block_size = 100,
    use_iso4 = TRUE,
    use_doi = TRUE,
    use_exact = TRUE,
    fuzzy = TRUE,
    use_postproc = TRUE,
    title_guard = FALSE
  )

  norm <- matching$CR_normalized
  if (is.null(norm) || !all(c("SR", "CR") %in% names(norm))) {
    stop("O Reference Matching não retornou CR_normalized no formato esperado.", call. = FALSE)
  }

  M <- M %>%
    dplyr::select(-CR) %>%
    dplyr::left_join(norm %>% dplyr::select(SR, CR, n_references), by = "SR")

  # Não repõe CR bruto quando nenhuma referência válida foi normalizada.
  # Isso evita misturar novamente formatos incompatíveis de WoS e Scopus.
  M$CR[is.na(M$CR) | trimws(M$CR) == ""] <- NA_character_
  M$SR <- M$.SR_original
  M$.SR_original <- NULL
  M$.ref_id <- NULL

  n_brutas <- sum(contar_referencias(M$CR_raw), na.rm = TRUE)
  n_normalizadas <- sum(ifelse(is.na(M$n_references), 0L, M$n_references), na.rm = TRUE)
  n_unicas <- if (!is.null(matching$summary)) nrow(matching$summary) else NA_integer_
  n_clusters_variantes <- if (!is.null(matching$summary) && "n_variants" %in% names(matching$summary)) {
    sum(matching$summary$n_variants > 1, na.rm = TRUE)
  } else NA_integer_

  diagnostico_ref <- data.frame(
    referencias_brutas = n_brutas,
    referencias_normalizadas_por_documento = n_normalizadas,
    trabalhos_citados_unicos = n_unicas,
    clusters_com_variantes = n_clusters_variantes,
    threshold = threshold,
    stringsAsFactors = FALSE
  )

  if (!is.null(matching$full_data) && "SR" %in% names(matching$full_data)) {
    matching$full_data <- matching$full_data %>%
      dplyr::left_join(mapa_origem, by = "SR")
  }

  list(M = M, matching = matching, diagnostico = diagnostico_ref)
}

salvar_auditoria_reference_matching <- function(h, pasta) {
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)

  if (!is.null(h$matching$summary)) {
    salvar_csv_excel(
      to_csv_safe(h$matching$summary),
      file.path(pasta, "reference_matching_summary.csv")
    )
  }

  if (!is.null(h$matching$full_data)) {
    variantes <- h$matching$full_data
    salvar_csv_excel(
      to_csv_safe(variantes),
      file.path(pasta, "reference_matching_variants.csv")
    )
  }

  salvar_csv_excel(
    to_csv_safe(h$diagnostico),
    file.path(pasta, "reference_matching_diagnostics.csv")
  )
}

consolidar_grupo <- function(grupo) {
  base <- grupo[1, , drop = FALSE]
  recuperados <- 0L

  if (nrow(grupo) > 1L) {
    # CR já está harmonizado. Para documentos duplicados, a regra correta é
    # UNIÃO das referências canônicas das duas bases, e não escolher a lista maior.
    if ("CR" %in% names(grupo)) {
      base$CR <- unir_lista_sem_duplicar(grupo$CR)
    }

    # CR_raw preserva todas as variantes originais para auditoria/rastreabilidade.
    if ("CR_raw" %in% names(grupo)) {
      base$CR_raw <- unir_lista_sem_duplicar(grupo$CR_raw)
    }

    if ("DB_ORIGINS" %in% names(grupo)) {
      origens <- unique(trimws(as.character(grupo$DB_ORIGINS)))
      origens <- origens[!is.na(origens) & nzchar(origens)]
      base$DB_ORIGINS <- paste(origens, collapse = "; ")
    }

    colunas <- setdiff(
      names(grupo),
      c(".ordem", ".n_ref", ".n_campos", "CR", "CR_raw", "DB_ORIGINS", "n_references")
    )

    for (col in colunas) {
      if (is.list(grupo[[col]])) next
      if (celula_vazia(base[[col]][1])) {
        candidatos <- grupo[[col]][-1]
        candidatos <- candidatos[!celula_vazia(candidatos)]
        if (length(candidatos) > 0L) {
          base[[col]] <- candidatos[1]
          recuperados <- recuperados + 1L
        }
      }
    }

    if ("n_references" %in% names(base)) {
      base$n_references <- contar_referencias(base$CR)
    }
  }

  attr(base, "recuperados") <- recuperados
  base
}

manter_mais_referencias <- function(df, chave) {
  valida <- !is.na(chave) & nzchar(chave)
  com <- df[valida, , drop = FALSE]; sem <- df[!valida, , drop = FALSE]
  recuperados_total <- 0L
  if (nrow(com) > 0L) {
    chaves <- chave[valida]
    ordem <- order(-com$.n_ref, -com$.n_campos, com$.ordem)
    com <- com[ordem, , drop = FALSE]; chaves <- chaves[ordem]
    eh_dup <- chaves %in% chaves[duplicated(chaves)]
    unicos <- com[!eh_dup, , drop = FALSE]
    if (any(eh_dup)) {
      grupos <- split(com[eh_dup, , drop = FALSE], chaves[eh_dup])
      consolidados <- lapply(grupos, function(g) {
        r <- consolidar_grupo(g)
        recuperados_total <<- recuperados_total + attr(r, "recuperados")
        r
      })
      com <- dplyr::bind_rows(c(list(unicos), consolidados))
    } else {
      com <- unicos
    }
  }
  resultado <- dplyr::bind_rows(com, sem)
  attr(resultado, "campos_recuperados") <- recuperados_total
  resultado
}

unir_bases_priorizando_referencias <- function(WOS, SCOPUS, pasta_auditoria = NULL) {
  n_wos <- if (is.null(WOS)) 0L else nrow(WOS)
  n_scopus <- if (is.null(SCOPUS)) 0L else nrow(SCOPUS)

  if (n_wos > 0L && n_scopus > 0L) {
    # mergeDbSources harmoniza a estrutura das bases. Em coleções multi-base,
    # bibliometrix move CR para CR_raw e esvazia CR deliberadamente.
    todos <- mergeDbSources(WOS, SCOPUS, remove.duplicated = FALSE, verbose = FALSE)
    todos$CR <- todos$CR_raw
  } else if (n_wos > 0L) {
    todos <- WOS
    if (!"CR_raw" %in% names(todos)) todos$CR_raw <- todos$CR
  } else if (n_scopus > 0L) {
    todos <- SCOPUS
    if (!"CR_raw" %in% names(todos)) todos$CR_raw <- todos$CR
  } else {
    stop("Nenhuma base válida foi fornecida para compilação.", call. = FALSE)
  }

  for (col in c("DB", "SR", "CR", "DI", "TI", "PY")) {
    if (!col %in% names(todos)) todos[[col]] <- NA_character_
  }
  todos$DB_ORIGINS <- as.character(todos$DB)
  n_bruto <- nrow(todos)

  # Harmonização GLOBAL antes da deduplicação dos documentos.
  h <- harmonizar_referencias(todos, threshold = ref_match_threshold)
  todos <- h$M

  if (!is.null(pasta_auditoria)) {
    salvar_auditoria_reference_matching(h, pasta_auditoria)
  }

  todos$.ordem <- seq_len(nrow(todos))
  todos$.n_ref <- contar_referencias(todos$CR)
  campos <- intersect(c("AU", "TI", "SO", "PY", "DI", "AB", "DE", "ID", "C1", "RP"), names(todos))
  todos$.n_campos <- if (length(campos) > 0L) {
    rowSums(vapply(todos[campos], function(x) {
      !is.na(x) & nzchar(trimws(as.character(x)))
    }, logical(nrow(todos))))
  } else rep(0L, nrow(todos))

  todos <- manter_mais_referencias(todos, normalizar_doi(todos$DI))
  rec_doi <- attr(todos, "campos_recuperados")
  n_apos_doi <- nrow(todos)

  titulo <- normalizar_titulo(todos$TI); ano <- trimws(as.character(todos$PY))
  chave <- ifelse(nzchar(titulo) & !is.na(ano) & nzchar(ano), paste(titulo, ano, sep = "||"), "")
  todos <- manter_mais_referencias(todos, chave)
  rec_titulo <- attr(todos, "campos_recuperados")
  n_final <- nrow(todos)

  todos <- todos[order(todos$.ordem), , drop = FALSE]
  todos$.n_ref <- contar_referencias(todos$CR)

  diagnostico <- data.frame(
    registros_wos = n_wos,
    registros_scopus = n_scopus,
    registros_identificados = n_bruto,
    duplicados_por_doi = n_bruto - n_apos_doi,
    duplicados_adicionais_titulo_ano = n_apos_doi - n_final,
    duplicados_removidos = n_bruto - n_final,
    campos_recuperados_de_duplicados = rec_doi + rec_titulo,
    documentos = n_final,
    documentos_com_CR = sum(todos$.n_ref > 0L),
    total_referencias_harmonizadas = sum(todos$.n_ref),
    trabalhos_citados_unicos = h$diagnostico$trabalhos_citados_unicos,
    clusters_com_variantes = h$diagnostico$clusters_com_variantes,
    reference_matching_threshold = h$diagnostico$threshold
  )

  todos$.ordem <- todos$.n_ref <- todos$.n_campos <- NULL
  class(todos) <- unique(c("bibliometrixDB", class(todos)))
  attr(todos, "diagnostico_CR") <- diagnostico
  attr(todos, "referencias_harmonizadas") <- TRUE
  attr(todos, "reference_matching_threshold") <- ref_match_threshold
  todos
}

imprimir_resumo_prisma <- function(base_final) {
  d <- attr(base_final, "diagnostico_CR")
  if (is.null(d)) return(invisible(NULL))
  sem_cr <- d$documentos - d$documentos_com_CR
  cobertura <- if (d$documentos > 0) 100 * d$documentos_com_CR / d$documentos else 0
  cat("\n============================================================\n",
      "RESUMO PARA O FLUXOGRAMA PRISMA E HARMONIZACAO DE REFERENCIAS\n",
      "============================================================\n",
      sprintf("Registros identificados na Web of Science: %d\n", d$registros_wos),
      sprintf("Registros identificados na Scopus:        %d\n", d$registros_scopus),
      sprintf("Total de registros identificados:         %d\n", d$registros_identificados),
      "------------------------------------------------------------\n",
      sprintf("Duplicados removidos por DOI:              %d\n", d$duplicados_por_doi),
      sprintf("Duplicados adicionais por titulo/ano:      %d\n", d$duplicados_adicionais_titulo_ano),
      sprintf("Total de duplicados removidos:             %d\n", d$duplicados_removidos),
      sprintf("Campos recuperados dos duplicados:         %d\n", d$campos_recuperados_de_duplicados),
      "------------------------------------------------------------\n",
      sprintf("Corpus final para triagem/analise:         %d\n", d$documentos),
      sprintf("Documentos com referencias harmonizadas:   %d\n", d$documentos_com_CR),
      sprintf("Documentos sem referencias utilizaveis:    %d\n", sem_cr),
      sprintf("Cobertura de referencias citadas:          %.1f%%\n", cobertura),
      sprintf("Referencias harmonizadas no corpus final:  %d\n", d$total_referencias_harmonizadas),
      sprintf("Trabalhos citados unicos identificados:    %d\n", d$trabalhos_citados_unicos),
      sprintf("Clusters com variantes de referencia:      %d\n", d$clusters_com_variantes),
      sprintf("Threshold do Reference Matching:           %.2f\n", d$reference_matching_threshold),
      "------------------------------------------------------------\n",
      "CR      = referencias canonicas harmonizadas usadas nas analises\n",
      "CR_raw  = referencias originais preservadas para auditoria\n",
      "DB_ORIGINS = base(s) em que o documento foi recuperado\n",
      "============================================================\n\n", sep = "")
  invisible(d)
}

remover_undefined <- function(celula) {
  if (is.na(celula) || celula == "") return(celula)
  itens <- unlist(strsplit(celula, ";", fixed = TRUE))
  itens <- trimws(itens)
  itens <- itens[itens != "" & !grepl("undefined", itens, ignore.case = TRUE)]
  if (length(itens) == 0) return("")
  paste(itens, collapse = "; ")
}

# ============================================================================
# ETAPA A - OBTER O CORPUS (final.rds pronto OU compilar dos brutos)
# ============================================================================

origem_corpus <- perguntar_origem_corpus()

if (origem_corpus == "1") {
  # ---- Corpus pronto ----
  arquivo_rds <- escolher_arquivo(
    "Selecione o arquivo final.rds",
    obrigatorio = TRUE
  )

  if (!grepl("\\.rds$", arquivo_rds, ignore.case = TRUE)) {
    stop("O arquivo selecionado não possui extensão .rds. Selecione o arquivo final.rds.", call. = FALSE)
  }

  M <- readRDS(arquivo_rds)
  if (!isTRUE(attr(M, "referencias_harmonizadas"))) {
    stop(
      paste0(
        "Este final.rds foi criado por uma versão anterior do script e não contém a harmonização segura de CR. ",
        "Rode novamente o script, escolha a opção 2 (compilar Scopus + WoS) e selecione a pasta com os arquivos brutos. ",
        "Isso é necessário para recuperar referências exclusivas de registros duplicados antes da deduplicação."
      ),
      call. = FALSE
    )
  }
  if (!inherits(M, "bibliometrixDB")) class(M) <- c("bibliometrixDB", "data.frame")
  pasta_base <- dirname(arquivo_rds)
} else {
  # ---- Compilar a partir dos arquivos brutos ----
  titulo_bloco("ETAPA A - COMPILAÇÃO DAS BASES (Scopus + WoS)")
  pasta_brutos <- escolher_pasta(
    "Selecione a pasta que contém os arquivos brutos da Scopus (.csv) e da WoS (.txt)")

  arquivos_scopus <- list.files(pasta_brutos, pattern = "\\.csv$",
                                full.names = TRUE, ignore.case = TRUE)
  arquivos_wos <- list.files(pasta_brutos, pattern = "\\.txt$",
                             full.names = TRUE, ignore.case = TRUE)

  cat("\nArquivos encontrados na pasta:\n")
  cat(sprintf("  Scopus (.csv): %d\n", length(arquivos_scopus)))
  if (length(arquivos_scopus) > 0) cat(paste0("    - ", basename(arquivos_scopus), "\n"), sep = "")
  cat(sprintf("  WoS (.txt):    %d\n", length(arquivos_wos)))
  if (length(arquivos_wos) > 0) cat(paste0("    - ", basename(arquivos_wos), "\n"), sep = "")

  if (length(arquivos_scopus) == 0 && length(arquivos_wos) == 0) {
    stop("Nenhum arquivo .csv (Scopus) ou .txt (WoS) encontrado na pasta selecionada.")
  }

  SCOPUS <- NULL
  if (length(arquivos_scopus) > 0) {
    SCOPUS <- convert2df(file = arquivos_scopus, dbsource = "scopus", format = "csv")
  }
  WOS <- NULL
  if (length(arquivos_wos) > 0) {
    WOS <- convert2df(file = arquivos_wos, dbsource = "wos", format = "plaintext")
  }

  FINAL <- unir_bases_priorizando_referencias(WOS, SCOPUS, pasta_auditoria = pasta_brutos)

  # Tratamentos da base. CR já chega harmonizado; CR_raw permanece apenas para auditoria.
  for (col in c("DE", "ID")) {
    if (!col %in% names(FINAL)) FINAL[[col]] <- ""
    FINAL[[col]] <- trimws(as.character(FINAL[[col]]))
    FINAL[[col]][is.na(FINAL[[col]])] <- ""
  }
  if (!"CR" %in% names(FINAL)) FINAL$CR <- NA_character_
  FINAL$CR <- gsub("\n", ";", as.character(FINAL$CR))
  FINAL$CR <- vapply(FINAL$CR, remover_undefined, character(1), USE.NAMES = FALSE)
  FINAL$CR[trimws(FINAL$CR) == ""] <- NA_character_
  if ("CR_raw" %in% colnames(FINAL)) {
    FINAL$CR_raw <- vapply(FINAL$CR_raw, remover_undefined, character(1), USE.NAMES = FALSE)
  }

  # Salvar final.rds e final.csv na própria pasta dos brutos
  saveRDS(FINAL, file.path(pasta_brutos, "final.rds"))
  salvar_csv_excel(to_csv_safe(FINAL), file.path(pasta_brutos, "final.csv"))
  cat(sprintf("\nArquivos salvos:\n  %s\n  %s\n",
              file.path(pasta_brutos, "final.rds"),
              file.path(pasta_brutos, "final.csv")))

  imprimir_resumo_prisma(FINAL)

  M <- FINAL
  if (!inherits(M, "bibliometrixDB")) class(M) <- c("bibliometrixDB", "data.frame")
  pasta_base <- pasta_brutos
}

# ============================================================================
# ETAPA B - ARQUIVO DE SINÔNIMOS (opcional na 1ª rodada)
# ============================================================================

synonyms_file <- escolher_arquivo(
  "Selecione o arquivo de SINÔNIMOS (.txt gerado pela IA). Se ainda NÃO tiver, clique em CANCELAR",
  obrigatorio = FALSE)
if (is.null(synonyms_file)) {
  synonyms_file <- ""
  message("Sem arquivo de sinônimos: as análises de palavras-chave ficarão para a próxima rodada.")
}

# Pasta de saída e log
output_dir <- file.path(pasta_base, paste0("exports_", Sys.Date()))
dir.create(output_dir, showWarnings = FALSE)
log_file <- file.path(output_dir, "_log_erros.txt")
if (file.exists(log_file)) file.remove(log_file)  # limpa log de rodadas anteriores

# Registro dos parâmetros usados nesta execução para reprodutibilidade.
PARAMETROS$executado_em <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
salvar_csv_excel(PARAMETROS, file.path(output_dir, "00_parametros_execucao.csv"))

run <- function(nome, expr) {
  message(">> ", nome)
  tryCatch(expr, error = function(e) {
    msg <- paste(Sys.time(), "|", nome, "|", conditionMessage(e))
    cat(msg, "\n", file = log_file, append = TRUE)
    message("   ERRO registrado no log: ", conditionMessage(e))
  })
}

save_png <- function(p, nome, w = 12, h = 8) {
  ggsave(file.path(output_dir, paste0(nome, ".png")), plot = p,
         width = w, height = h, dpi = 300, bg = "white")
}

with_png_device <- function(caminho, expr, width = 3000, height = 3000, res = 300) {
  png(caminho, width = width, height = height, res = res)
  on.exit({
    if (dev.cur() > 1L) dev.off()
  }, add = TRUE)
  force(expr)
}

salvar_csv <- function(df, nome) {
  caminho <- file.path(output_dir, nome)
  salvar_csv_excel(df, caminho)
  cat("  [salvo]", caminho, "\n")
}

# Guarda o resumo PRISMA também em arquivo (quando houver compilação)
if (!is.null(attr(M, "diagnostico_CR"))) {
  writeLines(capture.output(imprimir_resumo_prisma(M)),
             file.path(output_dir, "00_Resumo_PRISMA.txt"))
}

# Copia os arquivos de auditoria do Reference Matching para a pasta de resultados.
auditoria_refs <- c("reference_matching_summary.csv",
                    "reference_matching_variants.csv",
                    "reference_matching_diagnostics.csv")
for (arq in auditoria_refs) {
  origem <- file.path(pasta_base, arq)
  if (file.exists(origem)) file.copy(origem, file.path(output_dir, arq), overwrite = TRUE)
}

message("Corpus carregado: ", nrow(M), " documentos, ",
        min(M$PY, na.rm = TRUE), "-", max(M$PY, na.rm = TRUE))

# Saneamento: CR vazio ("") vira NA — string vazia quebra o histNetwork /
# localCitations ("arguments imply differing number of rows: 0, 1")
if ("CR" %in% names(M)) M$CR[!is.na(M$CR) & trimws(M$CR) == ""] <- NA

# ============================================================================
# ETAPA C / FASE 1 - EXPORTAÇÕES PADRÃO INDEPENDENTES DE PALAVRAS-CHAVE
# ============================================================================

titulo_bloco("ETAPA C - EXPORTAÇÕES PADRÃO DO PROTOCOLO (FASE 1)")

res <- biblioAnalysis(M)
S   <- summary(res, k = top_k, verbose = FALSE)

# 1. Main Information --------------------------------------------------------
run("1_main_information", {
  write_xlsx(list(MainInformation = S$MainInformationDF),
             file.path(output_dir, "01_Main_Information.xlsx"))
})

# 1b. Overview — painel de cartões (equivalente ao "Plot" do Main Information)
run("1b_overview_dashboard", {
  mi <- S$MainInformationDF
  pega <- function(padrao) {
    i <- grep(padrao, mi$Description, ignore.case = TRUE)[1]
    if (is.na(i)) return("-")
    as.character(mi$Results[i])
  }
  cards <- data.frame(
    titulo = c("Timespan", "Sources", "Documents", "Annual Growth Rate %",
               "Authors", "Authors of single-authored docs",
               "International Co-Authorship %", "Co-Authors per Doc",
               "Author's Keywords (DE)", "References",
               "Document Average Age", "Average Citations per Doc"),
    valor = c(pega("^Timespan"), pega("^Sources"), pega("^Documents$"),
              pega("Annual Growth"), pega("^Authors$"),
              pega("single-authored docs"), pega("International"),
              pega("Co-Authors per"), pega("Keywords \\(DE\\)"),
              pega("^References"), pega("Average Age"),
              pega("Average citations per doc")),
    stringsAsFactors = FALSE)
  cards$col <- rep(1:4, 3)
  cards$lin <- rep(3:1, each = 4)
  p <- ggplot(cards) +
    geom_rect(aes(xmin = col - .47, xmax = col + .47,
                  ymin = lin - .42, ymax = lin + .42),
              fill = "#2E86C1") +
    geom_text(aes(col - .40, lin + .26, label = titulo),
              hjust = 0, color = "white", size = 3.4) +
    geom_text(aes(col, lin - .06, label = valor),
              color = "white", size = 7, fontface = "bold") +
    theme_void() +
    ggtitle("Main Information — Overview") +
    theme(plot.title = element_text(hjust = .5, face = "bold", size = 16))
  save_png(p, "01b_Overview", w = 13, h = 7)
})

# 2. Annual Scientific Production (tabela + plot) ----------------------------
run("2_annual_production", {
  ap <- M %>% count(PY, name = "Articles") %>% arrange(PY)
  write_xlsx(list(AnnualProduction = ap),
             file.path(output_dir, "02_Annual_Production.xlsx"))
  p <- ggplot(ap, aes(PY, Articles)) + geom_line(linewidth = .9) +
    geom_point() + theme_minimal() +
    labs(x = "Ano", y = "Documentos", title = "Annual Scientific Production")
  save_png(p, "02_AnnualProduction")
})

# 3. Average Citations Per Year (tabela + plot) ------------------------------
run("3_avg_citations_per_year", {
  tc <- M %>% group_by(PY) %>%
    summarise(N = n(), MeanTCperArt = round(mean(TC, na.rm = TRUE), 2)) %>%
    mutate(CitableYears = max(M$PY, na.rm = TRUE) + 1 - PY,
           MeanTCperYear = round(MeanTCperArt / CitableYears, 2))
  write_xlsx(list(AvgCitationsPerYear = tc),
             file.path(output_dir, "03_Average_Citations_per_Year.xlsx"))
  p <- ggplot(tc, aes(PY, MeanTCperYear)) + geom_line(linewidth = .9) +
    geom_point() + theme_minimal() +
    labs(x = "Ano", y = "Citações médias/ano", title = "Average Citations per Year")
  save_png(p, "03_AvgCitationsPerYear")
})

# 5. Most Relevant Authors ---------------------------------------------------
run("5_most_relevant_authors", {
  write_xlsx(list(Authors = as.data.frame(S$MostProdAuthors)),
             file.path(output_dir, "05_Most_Relevant_Authors.xlsx"))
})

# 6. Authors' Production over Time -------------------------------------------
run("6_author_prod_over_time", {
  apt <- authorProdOverTime(M, k = author_prod_k, graph = FALSE)
  save_png(apt$graph, "06_AuthorsProdOverTime")
  write_xlsx(list(ProductionPerYear = apt$dfAU, Documents = apt$dfPapersAU),
             file.path(output_dir, "06_Author_Prod_over_Time.xlsx"))
})

# 7. Lotka's Law (+ Bradford, complementar) ----------------------------------
run("7_lotka", {
  lk <- lotka(M)                       # bibliometrix novo: recebe o corpus M
  if (!is.list(lk)) lk <- lotka(res)   # bibliometrix antigo: recebe biblioAnalysis
  if ("g_shiny" %in% names(lk)) save_png(lk$g_shiny, "07_LotkaLaw")
  write_xlsx(list(AuthorProd = lk$AuthorProd,
                  Summary = data.frame(Beta = lk$Beta, C = lk$C,
                                       R2 = lk$R2, p.value = lk$p.value)),
             file.path(output_dir, "07_Lotka_Law.xlsx"))
})
run("7b_bradford", {
  br <- bradford(M)
  save_png(br$graph, "07b_BradfordLaw")
  write_xlsx(list(Bradford = br$table),
             file.path(output_dir, "07b_Bradford_Law.xlsx"))
})

# 8. Most Relevant Affiliations + Sources + Countries ------------------------
run("8_affiliations_sources_countries", {
  write_xlsx(list(Affiliations = data.frame(head(res$Affiliations, top_k)),
                  Sources = as.data.frame(S$MostRelSources),
                  Countries = as.data.frame(S$MostProdCountries),
                  TCperCountry = as.data.frame(S$TCperCountries)),
             file.path(output_dir, "08_Affiliations_Sources_Countries.xlsx"))
})

# 9. Most Global (e Local) Cited Documents -----------------------------------
run("9_most_cited_documents", {
  doc <- M %>%
    mutate(TCperYear = round(TC / (max(PY, na.rm = TRUE) + 1 - PY), 2)) %>%
    group_by(PY) %>% mutate(NTC = round(TC / mean(TC, na.rm = TRUE), 3)) %>%
    ungroup() %>% arrange(desc(TC)) %>%
    select(SR, TI, SO, PY, DI, TC, TCperYear, NTC) %>% head(50)
  write_xlsx(list(MostGlobalCited = doc),
             file.path(output_dir, "09_Most_Global_Cited_Documents.xlsx"))
})
run("9b_local_citations", {
  lc <- localCitations(M, sep = ";")
  write_xlsx(list(Papers = head(lc$Papers, 50),
                  Authors = head(as.data.frame(lc$Authors), top_k)),
             file.path(output_dir, "09b_Most_Local_Cited.xlsx"))
})

# 16. Collaboration Network (autores e países) -------------------------------
run("16_collaboration_authors", {
  nm <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
  net <- NULL
  with_png_device(file.path(output_dir, "16_Collaboration_Authors.png"), {
    net <- networkPlot(nm, n = n_net, Title = "Author Collaboration", type = "auto",
                       size.cex = TRUE, labelsize = 1, remove.multiple = TRUE,
                       cluster = "louvain")
  })
  write_xlsx(list(Clusters = net$cluster_res),
             file.path(output_dir, "16_Collaboration_Authors.xlsx"))
})
run("16b_collaboration_countries", {
  M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
  nm <- biblioNetwork(M2, analysis = "collaboration", network = "countries", sep = ";")
  net <- NULL
  with_png_device(file.path(output_dir, "16b_Collaboration_Countries.png"), {
    net <- networkPlot(nm, n = n_net_paises, Title = "Country Collaboration", type = "circle",
                       size.cex = TRUE, labelsize = 1, cluster = "louvain")
  })
  write_xlsx(list(Clusters = net$cluster_res),
             file.path(output_dir, "16b_Collaboration_Countries.xlsx"))
})

# Extras padrão: Co-citation e Historiograph ---------------------------------
run("cocitation_network", {
  nm <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
  net <- NULL
  with_png_device(file.path(output_dir, "CoCitation_Network.png"), {
    net <- networkPlot(nm, n = n_net, Title = "Co-citation Network",
                       type = "fruchterman", size.cex = TRUE, labelsize = 1,
                       remove.multiple = TRUE, cluster = "louvain")
  })
  write_xlsx(list(Clusters = net$cluster_res),
             file.path(output_dir, "CoCitation_Network.xlsx"))
})
run("historiograph", {
  hn <- histNetwork(M, min.citations = 5, sep = ";")
  hp <- histPlot(hn, n = hist_n, size = 4, labelsize = 3)
  save_png(hp$g, "Historiograph", w = 14, h = 9)
  write_xlsx(list(HistData = hn$histData),
             file.path(output_dir, "Historiograph.xlsx"))
})

# ---------------- Base para a lista de sinônimos ----------------------------
run("keywords_frequencias_csv", {
  tab <- tableTag(M, "DE")
  kw <- data.frame(keyword = names(tab), freq = as.integer(tab))
  write.csv(kw, file.path(output_dir, "keywords_frequencias.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
})

prompt_ia <- paste(
  "Anexo o arquivo keywords_frequencias.csv (colunas: keyword, freq) de uma",
  "análise bibliométrica. Gere uma lista de sinônimos no formato do",
  "bibliometrix: arquivo .txt, uma linha por grupo, termos separados por",
  "ponto e vírgula, com o termo preferido em primeiro lugar.",
  "Ex.: CORPORATE SOCIAL RESPONSIBILITY;CSR;CORPORATE SOCIAL-RESPONSIBILITY",
  "Agrupe APENAS variações do mesmo conceito (singular/plural, sigla vs.",
  "forma extensa, grafia com/sem hífen). Não agrupe conceitos apenas",
  "relacionados e não invente termos fora do CSV. Gere o arquivo com a lista",
  "em formato .txt.")
writeLines(prompt_ia, file.path(output_dir, "PROMPT_para_gerar_sinonimos.txt"))

# ============================================================================
# ETAPA C (continuação) - ANÁLISES AVANÇADAS (independentes de keywords)
# ============================================================================

indicadores <- list()

# BLOCO A1 - LEI DE LOTKA (inverse power law + K-S) ---------------------------
run("A1_lotka_avancado", {
  titulo_bloco("AVANÇADO 1 - LEI DE LOTKA (inverse power law + K-S)")
  L <- tryCatch(lotka(M), error = function(e) lotka(res))
  if (!is.list(L)) L <- lotka(res)

  cat(sprintf("Beta estimado (expoente da lei de potência): %.3f\n", L$Beta))
  cat(sprintf("Constante C: %.3f\n", L$C))
  cat(sprintf("R2 do ajuste: %.3f\n", L$R2))
  cat(sprintf("p-valor do teste K-S (H0: dados seguem a Lotka teórica, beta = 2): %.4f\n", L$p.value))
  cat(if (L$p.value > 0.05) {
    "=> p > 0,05: NÃO se rejeita H0 - a distribuição é compatível com a lei de Lotka.\n"
  } else {
    "=> p <= 0,05: rejeita-se H0 - a distribuição observada difere da Lotka teórica (beta = 2).\n"
  })

  prod_autores_tab <- L$AuthorProd
  names(prod_autores_tab) <- c("n_artigos", "n_autores", "prop_autores")
  salvar_csv(prod_autores_tab, "blocoA1_lotka_produtividade.csv")

  Observed <- prod_autores_tab$prop_autores
  Theoretical <- 10^(log10(L$C) - 2 * log10(prod_autores_tab$n_artigos))
  with_png_device(file.path(output_dir, "blocoA1_lotka.png"), {
    plot(prod_autores_tab$n_artigos, Theoretical, type = "l", lty = 2, lwd = 2,
         col = "grey40", log = "xy",
         xlab = "Número de artigos", ylab = "Proporção de autores",
         main = "Lei de Lotka: observado vs. teórico (beta = 2)")
    lines(prod_autores_tab$n_artigos, Observed, lwd = 2, col = "firebrick")
    legend("topright", c("Teórico (beta = 2)", "Observado"),
           lty = c(2, 1), lwd = 2, col = c("grey40", "firebrick"), bty = "n")
  }, width = 900, height = 650, res = 110)

  indicadores$lotka_beta <- round(L$Beta, 3)
  indicadores$lotka_R2 <- round(L$R2, 3)
  indicadores$lotka_ks_p <- round(L$p.value, 4)
})

# BLOCO A2 - RPYS (Reference Publication Year Spectroscopy) -------------------
run("A2_rpys", {
  titulo_bloco("AVANÇADO 2 - RPYS / SPECTROSCOPY")
  RP <- rpys(M, sep = ";", graph = FALSE)

  rpys_tab <- RP$rpysTable
  salvar_csv(rpys_tab, "blocoA2_rpys_tabela.csv")

  col_desvio <- grep("diff", names(rpys_tab), ignore.case = TRUE, value = TRUE)[1]
  picos <- head(rpys_tab[order(-rpys_tab[[col_desvio]]), ], 10)
  cat("Top 10 anos-pico (maiores desvios da mediana móvel de 5 anos):\n")
  print(picos, row.names = FALSE)
  salvar_csv(picos, "blocoA2_rpys_anos_pico.csv")

  with_png_device(file.path(output_dir, "blocoA2_rpys.png"), {
    print(RP$spectroscopy)
  }, width = 1000, height = 650, res = 110)

  # Referências seminais dos anos-pico
  CRlist <- unlist(strsplit(as.character(M$CR), ";", fixed = TRUE))
  CRlist <- trimws(CRlist)
  CRlist <- CRlist[nzchar(CRlist) & CRlist != "NA"]
  anos_ref <- suppressWarnings(as.integer(sub(".*?((17|18|19|20)[0-9]{2}).*", "\\1", CRlist)))
  anos_pico <- head(picos$Year, 5)
  seminais <- do.call(rbind, lapply(anos_pico, function(a) {
    refs <- CRlist[!is.na(anos_ref) & anos_ref == a]
    if (length(refs) == 0) return(NULL)
    tab <- sort(table(refs), decreasing = TRUE)
    head(data.frame(ano_pico = a, referencia = names(tab),
                    citacoes = as.integer(tab), row.names = NULL), 10)
  }))
  cat("\nReferências seminais dos 5 principais anos-pico (top 10 de cada):\n")
  if (is.null(seminais) || nrow(seminais) == 0L) {
    message("Nenhuma referência seminal pôde ser extraída para os anos-pico selecionados.")
  } else {
    print(head(seminais, 20), row.names = FALSE)
    salvar_csv(seminais, "blocoA2_referencias_seminais.csv")
  }
})

# ============================================================================
# ETAPA D / FASE 2 - ANÁLISES DE PALAVRAS-CHAVE (exigem sinônimos)
# ============================================================================

if (synonyms_file == "" || !file.exists(synonyms_file)) {
  message("\n============================================================")
  message("RODADA SEM SINÔNIMOS CONCLUÍDA. Saídas em: ", normalizePath(output_dir))
  message("------------------------------------------------------------")
  message("PRÓXIMO PASSO (antes das análises de palavras-chave):")
  message("1. Envie 'keywords_frequencias.csv' para a IA de sua preferência")
  message("   com o prompt salvo em 'PROMPT_para_gerar_sinonimos.txt':\n")
  message(prompt_ia)
  message("\n2. Salve a resposta como 'sinonimos.txt' na pasta do projeto.")
  message("3. Revise o arquivo (a IA pode agrupar demais ou de menos).")
  message("4. Rode este script de novo: escolha a opção 1 e selecione o final.rds")
  message("   e o sinonimos.txt na 2ª janela.")
  message("============================================================")
} else {

titulo_bloco("ETAPA D - ANÁLISES DE PALAVRAS-CHAVE (FASE 2)")

aplicar_sinonimos <- function(M, arquivo) {
  linhas <- trimws(readLines(arquivo, warn = FALSE, encoding = "UTF-8"))
  linhas <- linhas[linhas != ""]
  mapa <- list()
  for (ln in linhas) {
    termos <- toupper(trimws(strsplit(ln, ";")[[1]]))
    termos <- termos[termos != ""]
    if (length(termos) < 2) next
    for (t in termos[-1]) mapa[[t]] <- termos[1]
  }
  M$DE <- vapply(M$DE, function(s) {
    if (is.na(s)) return(NA_character_)
    v <- toupper(trimws(strsplit(s, ";")[[1]]))
    v <- vapply(v, function(x) if (x %in% names(mapa)) mapa[[x]] else x,
                character(1), USE.NAMES = FALSE)
    paste(unique(v[v != ""]), collapse = ";")
  }, character(1), USE.NAMES = FALSE)
  message("Sinônimos aplicados: ", length(mapa), " variações unificadas.")
  M
}
M <- aplicar_sinonimos(M, synonyms_file)

# 10. WordCloud (tabela de frequência) ----------------------------------------
run("10_word_frequency", {
  tab <- tableTag(M, "DE")
  kw <- data.frame(keyword = names(tab), freq = as.integer(tab)) %>% head(100)
  write_xlsx(list(WordFrequency = kw),
             file.path(output_dir, "10_Word_Frequency.xlsx"))
  p <- ggplot(head(kw, 25), aes(reorder(keyword, freq), freq)) +
    geom_col() + coord_flip() + theme_minimal() +
    labs(x = NULL, y = "Ocorrências", title = "Most Frequent Author's Keywords")
  save_png(p, "10_MostFrequentWords")
})

# 11. Words' Frequency over Time ----------------------------------------------
run("11_word_dynamics", {
  kg <- KeywordGrowth(M, Tag = "DE", sep = ";", top = keyword_growth_top, cdf = TRUE)
  write_xlsx(list(WordDynamics = kg),
             file.path(output_dir, "11_Words_Frequency_over_Time.xlsx"))
  kg_long <- tidyr::pivot_longer(kg, -Year, names_to = "Termo", values_to = "Freq")
  p <- ggplot(kg_long, aes(Year, Freq, color = Termo)) +
    geom_line(linewidth = .8) + theme_minimal() +
    labs(y = "Ocorrências acumuladas")
  save_png(p, "11_WordsFrequencyOverTime")
})

# 12. Trend Topics -------------------------------------------------------------
run("12_trend_topics", {
  tt <- fieldByYear(M, field = "DE", timespan = c(ano_inicio_analise, ano_fim_analise),
                    min.freq = min_freq_kw, n.items = trend_n_items, graph = FALSE)
  save_png(tt$graph, "12_TrendTopics")
  write_xlsx(list(TrendTopics = tt$df_graph),
             file.path(output_dir, "12_TrendTopics.xlsx"))
})

# 12b. Trend Topics POR QUINQUÊNIO (análise avançada, bloco 3) -----------------
# Período do projeto definido nos parâmetros no início do script
run("12b_trend_topics_quinquenios", {
  cortes <- seq(ano_inicio_analise, ano_fim_analise, by = 5)
  quinquenios <- lapply(cortes, function(inicio) c(inicio, min(inicio + 4, ano_fim_analise)))

  tabela_quinquenios <- do.call(rbind, lapply(quinquenios, function(q) {
    sub <- M[!is.na(M$PY) & M$PY >= q[1] & M$PY <= q[2], ]
    if (nrow(sub) == 0) return(NULL)
    tab <- tableTag(sub, "DE")
    tab <- tab[!names(tab) %in% c("", "NA")]
    head(data.frame(quinquenio = paste(q[1], q[2], sep = "-"),
                    documentos_no_bloco = nrow(sub),
                    termo = names(tab), frequencia = as.integer(tab),
                    row.names = NULL), 15)
  }))
  print(tabela_quinquenios, row.names = FALSE)
  salvar_csv(tabela_quinquenios, "12b_trend_topics_quinquenios.csv")
})

# 13. Co-occurrence Network -----------------------------------------------------
run("13_cooccurrence_network", {
  nm <- biblioNetwork(M, analysis = "co-occurrences",
                      network = "author_keywords", sep = ";")
  net <- NULL
  with_png_device(file.path(output_dir, "13_CoOccurrence_Network.png"), {
    net <- networkPlot(nm, normalize = "association", n = n_net,
                       Title = "Keyword Co-occurrence", type = "fruchterman",
                       size.cex = TRUE, labelsize = 1, remove.multiple = TRUE,
                       cluster = "louvain")
  })
  write_xlsx(list(Clusters = net$cluster_res),
             file.path(output_dir, "13_CoOccurrence_Network.xlsx"))
})

# 13b. Estrutura conceitual (análise fatorial MCA) -----------------------------
run("13b_factorial_analysis", {
  cs <- conceptualStructure(M, field = "DE", method = "MCA",
                            minDegree = min_freq_kw, clust = "auto",
                            k.max = 8, stemming = FALSE, graph = FALSE)
  save_png(cs$graph_terms, "13b_FactorialMap")
  # o dendrograma não é um ggplot: salvar via png() + plot()
  with_png_device(file.path(output_dir, "13b_Dendrogram.png"), {
    plot(cs$graph_dendogram)
  }, width = 3000, height = 2000, res = 300)
  wc <- data.frame(word = rownames(cs$km.res$data.clust), cs$km.res$data.clust)
  write_xlsx(list(WordsByCluster = wc),
             file.path(output_dir, "13b_Factorial_Analysis.xlsx"))
})

# 14. Thematic Map --------------------------------------------------------------
run("14_thematic_map", {
  tm <- thematicMap(M, field = "DE", n = thematic_n, minfreq = min_freq_kw,
                    stemming = FALSE, size = 0.5, n.labels = thematic_labels, repel = TRUE)
  save_png(tm$map, "14_ThematicMap")
  write_xlsx(list(Clusters = tm$clusters, Words = tm$words),
             file.path(output_dir, "14_ThematicMap_Clusters.xlsx"))
  write_xlsx(list(Docs = tm$documentToClusters),
             file.path(output_dir, "14_ThematicMap_Documents.xlsx"))
})

# 15. Thematic Evolution (slices definidos em 'periodos') ------------------------
run("15_thematic_evolution", {
  te <- thematicEvolution(M, field = "DE", years = periodos,
                          n = thematic_n, minFreq = min_freq_kw)
  write_xlsx(list(Evolution = te$Data),
             file.path(output_dir, "15_Thematic_Evolution.xlsx"))
  for (i in seq_along(te$TM)) {
    write_xlsx(list(Terms = te$TM[[i]]$words, Clusters = te$TM[[i]]$clusters),
               file.path(output_dir,
                         paste0("15_ThematicEvolution_Slice_", i, ".xlsx")))
    save_png(te$TM[[i]]$map, paste0("15_ThematicEvolution_Map_Slice_", i))
  }
  pe <- plotThematicEvolution(te$Nodes, te$Edges)
  htmlwidgets::saveWidget(pe, file.path(output_dir, "15_ThematicEvolution_Sankey.html"))
})

# 4. Three-Field Plot (CR x AU x DE) ---------------------------------------------
run("4_three_field_plot", {
  tfp <- threeFieldsPlot(M, fields = c("CR", "AU", "DE"), n = rep(threefield_n, 3))
  htmlwidgets::saveWidget(tfp, file.path(output_dir, "04_ThreeFieldPlot.html"))
})

message("\nFASE 2 (palavras-chave) CONCLUÍDA.")
}


# Abre a pasta de resultados no Finder/Explorer (comando direto do sistema,
# sem Tcl/Tk — evita a dependência de X11/TclTk no Mac)
try({
  caminho <- normalizePath(output_dir)
  sis <- Sys.info()[["sysname"]]
  if (sis == "Darwin") {
    system2("open", shQuote(caminho))
  } else if (sis == "Windows") {
    shell.exec(caminho)
  } else {
    system2("xdg-open", shQuote(caminho))
  }
}, silent = TRUE)

# O script chegou ao fim e os arquivos estão na pasta. Boa sorte na análise.
# Esse código foi desenvolvido pela Prof. Dra. Larissa Raposo - lararaposo@gmail.com para fins acadêmicos e de uso pessoal.
# Fique à vontade para evoluí-lo!

# Agora siga com o Biblioshiny para as análises que dependem de extração manual:
# Author Profile, Diachronic Networks, ajustes visuais das redes etc.

#ATENÇÃO: Caso não tenha carregado a lista de sinônimos, acesse os arquivos PROMPT_para_gerar_sinonimos.txt e keywords_frequencias.csv, acesse a sua IA de preferência (ChatGPT ou Claude) e rode o script novamente. Dessa vez, escolha a opção 1 quando perguntado sobre a origem do corpus, selecione o arquivo final.rds e carregue o arquivo de sinônimos.txt.

biblioshiny()

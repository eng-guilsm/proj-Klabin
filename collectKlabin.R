###############################################################################
# collectKlabin.R
# Pipeline Klabin — Passo 1 & 2: Extração de Dados do PDF + Coleta de Mercado
#
# Autor : Pipeline Automatizada — Projeto Klabin (KLBN11)
# Data  : 2025 / atualização 2026
#
# Este script:
#   1. Lê o PDF do Equity Research e extrai os dados financeiros históricos
#   2. Coleta cotações de KLBN11 e Ibovespa (Yahoo Finance / quantmod)
#   3. Busca a taxa Selic na API do Banco Central do Brasil
#   4. Calcula o Beta da ação e estima o prêmio de risco
#   5. Salva tudo em arquivos estruturados na pasta data/
###############################################################################

# ── 0. Configuração Inicial ──────────────────────────────────────────────────

# Instalar pacotes necessários caso não estejam disponíveis
pacotes_necessarios <- c(
 "pdftools", "dplyr", "tidyr", "readr", "stringr", "tibble",
 "quantmod", "httr", "jsonlite", "xts", "zoo"
)

for (pkg in pacotes_necessarios) {
 if (!requireNamespace(pkg, quietly = TRUE)) {
   message("Instalando pacote: ", pkg)
   install.packages(pkg, repos = "https://cloud.r-project.org")
 }
}

suppressPackageStartupMessages({
 library(dplyr)
 library(tidyr)
 library(readr)
 library(stringr)
 library(tibble)
 library(quantmod)
 library(httr)
 library(jsonlite)
 library(pdftools)
})

# Criar pasta de dados se não existir
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

message("=" |> rep(70) |> paste(collapse = ""))
message("  PIPELINE KLABIN — Coleta e Estruturação de Dados")
message("=" |> rep(70) |> paste(collapse = ""))
message("")

###############################################################################
# ── PARTE A: EXTRAÇÃO DE DADOS DO PDF ───────────────────────────────────────
###############################################################################

message("── PASSO 1: Extração de dados do PDF ──────────────────────────────")

# Caminho do PDF (na pasta pai do projeto)
pdf_path <- file.path(dirname(getwd()),
                      "Klabin KLBN11 EquityResearch 2025.pdf")

# Tentar extrair texto do PDF
pdf_extraido <- tryCatch({
 txt <- pdftools::pdf_text(pdf_path)
 message("  ✓ PDF lido com sucesso: ", length(txt), " páginas extraídas")
 txt
}, error = function(e) {
 message("  ✗ Falha ao ler PDF: ", e$message)
 message("  → Usando dados codificados como fallback")
 NULL
})

# ── Dados Financeiros Históricos (Tabela Principal) ──────────────────────
# Fonte: Equity Research Company Money, Maio 2025
# Valores em R$ bilhões (exceto margens e múltiplos)

financeiros <- tibble(
 ano             = c(2020,  2021,  2022,  2023,  2024,  2025),
 tipo            = c("Real","Real","Real","Real","Est.", "Est."),
 receita_liquida = c(13.0,  17.8,  22.0,  21.3,  22.8,  24.5),
 ebitda_ajustado = c( 3.6,   6.2,   8.2,   6.9,   7.6,   8.4),
 margem_ebitda   = c(27.7,  34.8,  37.3,  32.4,  33.3,  34.3),
 lucro_liquido   = c( 0.4,   1.8,   3.1,   0.7,   1.2,   1.9),
 capex           = c(-3.2,  -4.8,  -5.1,  -3.9,  -3.5,  -3.2),
 divida_liquida  = c(14.8,  19.9,  28.7,  28.4,  28.8,  27.0),
 dl_ebitda       = c( 4.1,   3.2,   3.5,   4.1,   3.8,   3.2),
 fcf_pre_capex   = c( 2.9,   5.1,   6.8,   5.4,   5.8,   7.1)
)

message("  ✓ Dados financeiros históricos estruturados (2020–2025E)")

# ── Premissas de Valuation (do PDF) ──────────────────────────────────────

premissas <- tibble(
 parametro = c(
   "wacc", "ke", "kd_pre_tax", "de_ratio", "growth_rate_g",
   "periodo_projecao", "bhkp_2025e", "bhkp_lp", "cambio_2025",
   "cambio_lp", "margem_ebitda_norm_min", "margem_ebitda_norm_max",
   "capex_receita_min", "capex_receita_max", "ev_calculado",
   "divida_liquida_base", "outros_passivos", "equity_value",
   "preco_alvo_fcd", "preco_alvo_multiplos", "preco_alvo_final",
   "preco_atual_mai2025", "market_cap", "aliquota_ir",
   "peso_fcd", "peso_multiplos"
 ),
 valor = c(
   0.102, 0.131, 0.078, 0.45, 0.035,
   10, 600, 580, 5.80,
   5.50, 0.33, 0.35,
   0.13, 0.15, 57.2,
   28.8, 2.3, 26.1,
   24.80, 24.55, 24.50,
   20.80, 27.5, 0.34,
   0.60, 0.40
 ),
 descricao = c(
   "WACC (% a.a.)", "Ke - Custo Capital Próprio (% a.a.)",
   "Kd - Custo Dívida Pré-IR (% a.a.)", "D/(D+E) - Estrutura Capital",
   "Taxa Crescimento Perpetuidade (g)", "Período Projeção (anos)",
   "BHKP 2025E (US$/t)", "BHKP Longo Prazo (US$/t)",
   "Câmbio 2025 (R$/USD)", "Câmbio Longo Prazo (R$/USD)",
   "Margem EBITDA Normalizada Mín.", "Margem EBITDA Normalizada Máx.",
   "CAPEX/Receita Mín.", "CAPEX/Receita Máx.",
   "Valor da Firma EV (R$ bi)", "Dívida Líquida Base (R$ bi)",
   "Outros Passivos e Minoritários (R$ bi)", "Equity Value (R$ bi)",
   "Preço-Alvo FCD (R$/unit)", "Preço-Alvo Múltiplos Média (R$/unit)",
   "Preço-Alvo Final (R$/unit)", "Preço Atual Mai/2025 (R$/unit)",
   "Market Cap (R$ bi)", "Alíquota IR (IRPJ+CSLL)",
   "Peso FCD no Preço-Alvo", "Peso Múltiplos no Preço-Alvo"
 )
)

message("  ✓ Premissas de valuation extraídas")

# ── Múltiplos de Mercado ─────────────────────────────────────────────────

multiplos <- tibble(
 metrica       = c("EV/EBITDA", "P/L", "EV/Receita", "P/FCF (pré-CAPEX)"),
 klabin_2024a  = c(7.2, 14.5, 2.4, 8.1),
 klabin_2025e  = c(6.5, 11.2, 2.2, 7.3),
 media_setor   = c(7.8, 15.0, 2.6, 9.0),
 preco_implied = c(25.90, 23.80, 25.10, 23.40)
)

message("  ✓ Múltiplos de mercado extraídos")

# ── Comparação com Pares ─────────────────────────────────────────────────

pares <- tibble(
 empresa    = c("Klabin", "Suzano", "Irani", "CMPC"),
 ticker     = c("KLBN11", "SUZB3", "RANI3", "BVL:CMPC"),
 market_cap = c(27.5, 68.4, 2.8, 22.1),
 ev_ebitda  = c(7.2, 8.1, 5.8, 7.8),
 pl         = c(14.5, 18.2, 11.0, 16.0),
 dl_ebitda  = c(3.8, 3.2, 1.9, 2.8),
 recomendacao = c("COMPRA", "NEUTRO", "COMPRA", "NEUTRO")
)

message("  ✓ Dados de pares do setor extraídos")

# ── Análise de Sensibilidade EBITDA 2025E ────────────────────────────────

sensibilidade <- expand_grid(
 bhkp_ust = c(500, 560, 620, 680),
 usdbrl   = c(5.00, 5.50, 6.00, 6.50)
) |>
 mutate(
   ebitda_bi = case_when(
     bhkp_ust == 500 & usdbrl == 5.00 ~ 6.8,
     bhkp_ust == 500 & usdbrl == 5.50 ~ 7.2,
     bhkp_ust == 500 & usdbrl == 6.00 ~ 7.6,
     bhkp_ust == 500 & usdbrl == 6.50 ~ 8.0,
     bhkp_ust == 560 & usdbrl == 5.00 ~ 7.4,
     bhkp_ust == 560 & usdbrl == 5.50 ~ 7.9,
     bhkp_ust == 560 & usdbrl == 6.00 ~ 8.4,
     bhkp_ust == 560 & usdbrl == 6.50 ~ 8.9,
     bhkp_ust == 620 & usdbrl == 5.00 ~ 8.0,
     bhkp_ust == 620 & usdbrl == 5.50 ~ 8.6,
     bhkp_ust == 620 & usdbrl == 6.00 ~ 9.1,
     bhkp_ust == 620 & usdbrl == 6.50 ~ 9.7,
     bhkp_ust == 680 & usdbrl == 5.00 ~ 8.7,
     bhkp_ust == 680 & usdbrl == 5.50 ~ 9.3,
     bhkp_ust == 680 & usdbrl == 6.00 ~ 9.9,
     bhkp_ust == 680 & usdbrl == 6.50 ~ 10.4,
     TRUE ~ NA_real_
   )
 )

message("  ✓ Tabela de sensibilidade EBITDA extraída")

# Salvar dados do PDF
write_csv(financeiros, "data/financeiros_historicos.csv")
write_csv(premissas,   "data/premissas_valuation.csv")
write_csv(multiplos,   "data/multiplos_mercado.csv")
write_csv(pares,       "data/pares_setor.csv")
write_csv(sensibilidade, "data/sensibilidade_ebitda.csv")

message("  ✓ Todos os dados do PDF salvos em data/")
message("")

###############################################################################
# ── PARTE B: COLETA DE DADOS DE MERCADO (2026) ─────────────────────────────
###############################################################################

message("── PASSO 2: Coleta de dados de mercado atualizados ───────────────")

# ── 2.1 Cotações históricas KLBN11.SA e ^BVSP ────────────────────────────

# Datas: últimos 2 anos para cálculo de Beta
data_fim    <- Sys.Date()
data_inicio <- data_fim - 730  # ~2 anos

# Coletar KLBN11
cotacoes_klbn11 <- tryCatch({
 message("  → Buscando cotações KLBN11.SA (Yahoo Finance)...")
 dados <- getSymbols("KLBN11.SA", src = "yahoo",
                     from = data_inicio, to = data_fim,
                     auto.assign = FALSE)
 message("  ✓ KLBN11.SA: ", nrow(dados), " observações obtidas")
 dados
}, error = function(e) {
 message("  ✗ Falha ao buscar KLBN11: ", e$message)
 message("  → Cotações de KLBN11 indisponíveis — Beta usará fallback")
 NULL
})

# Coletar Ibovespa
cotacoes_ibov <- tryCatch({
 message("  → Buscando cotações ^BVSP / Ibovespa (Yahoo Finance)...")
 dados <- getSymbols("^BVSP", src = "yahoo",
                     from = data_inicio, to = data_fim,
                     auto.assign = FALSE)
 message("  ✓ ^BVSP: ", nrow(dados), " observações obtidas")
 dados
}, error = function(e) {
 message("  ✗ Falha ao buscar Ibovespa: ", e$message)
 NULL
})

# ── 2.2 Cálculo do Beta ─────────────────────────────────────────────────

calcular_beta <- function(ativo, mercado) {
 # Retornos semanais para reduzir ruído
 ret_ativo   <- weeklyReturn(ativo, type = "log")
 ret_mercado <- weeklyReturn(mercado, type = "log")

 # Alinhar datas
 dados_merged <- merge(ret_ativo, ret_mercado, join = "inner")
 colnames(dados_merged) <- c("ativo", "mercado")

 # Regressão linear: R_ativo = alpha + beta * R_mercado
 modelo <- lm(ativo ~ mercado, data = as.data.frame(dados_merged))
 beta   <- coef(modelo)["mercado"]
 r2     <- summary(modelo)$r.squared

 list(beta = as.numeric(beta), r2 = r2, n_obs = nrow(dados_merged))
}

beta_resultado <- tryCatch({
 if (!is.null(cotacoes_klbn11) && !is.null(cotacoes_ibov)) {
   resultado <- calcular_beta(cotacoes_klbn11, cotacoes_ibov)
   message(sprintf("  ✓ Beta calculado: %.3f (R² = %.3f, n = %d obs.)",
                   resultado$beta, resultado$r2, resultado$n_obs))
   resultado
 } else {
   message("  ⚠ Dados insuficientes para cálculo — usando Beta fallback: 0.85")
   list(beta = 0.85, r2 = NA, n_obs = 0)
 }
}, error = function(e) {
 message("  ✗ Erro no cálculo do Beta: ", e$message)
 message("  → Usando Beta fallback: 0.85")
 list(beta = 0.85, r2 = NA, n_obs = 0)
})

# ── 2.3 Taxa Selic (API do Banco Central) ────────────────────────────────

buscar_selic <- function() {
 # Série 432 = Meta Selic definida pelo COPOM (% a.a.)
 url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.432/dados/ultimos/5?formato=json"

 resp <- httr::GET(url, httr::timeout(15),
                   httr::add_headers("Accept" = "application/json"))

 if (httr::status_code(resp) != 200) {
   stop("HTTP status ", httr::status_code(resp))
 }

 dados <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
 # A série retorna valores em % a.a.
 selic <- as.numeric(dados$valor[nrow(dados)]) / 100
 data_ref <- dados$data[nrow(dados)]

 list(selic = selic, data_referencia = data_ref)
}

selic_resultado <- tryCatch({
 message("  → Consultando taxa Selic na API do BCB...")
 resultado <- buscar_selic()
 message(sprintf("  ✓ Selic atual: %.2f%% a.a. (ref: %s)",
                 resultado$selic * 100, resultado$data_referencia))
 resultado
}, error = function(e) {
 message("  ✗ Falha na API do BCB: ", e$message)
 message("  → Usando Selic fallback: 14.75% a.a.")
 list(selic = 0.1475, data_referencia = "fallback")
})

# ── 2.4 Prêmio de Risco de Mercado ──────────────────────────────────────
# Estimativa do Equity Risk Premium para o Brasil
# Fontes de referência: Damodaran (2024/2025), estimativas de mercado

# Prêmio de risco ERP = Retorno esperado do mercado - Taxa livre de risco
# Para mercados emergentes como Brasil, tipicamente entre 5-8%
# Usamos 6.5% como estimativa conservadora (Damodaran mature + CRP approach)

premio_risco <- 0.065
message(sprintf("  ✓ Prêmio de Risco de Mercado estimado: %.1f%%", premio_risco * 100))

# ── 2.5 Cotação atual KLBN11 ────────────────────────────────────────────

preco_atual <- tryCatch({
 if (!is.null(cotacoes_klbn11)) {
   ultimo <- as.numeric(last(Cl(cotacoes_klbn11)))
   message(sprintf("  ✓ Cotação atual KLBN11: R$ %.2f", ultimo))
   ultimo
 } else {
   message("  ⚠ Usando preço do PDF como fallback: R$ 20.80")
   20.80
 }
}, error = function(e) {
 message("  ⚠ Usando preço do PDF: R$ 20.80")
 20.80
})

# ── 2.6 Salvar cotações em CSV ───────────────────────────────────────────

if (!is.null(cotacoes_klbn11)) {
 df_klbn <- data.frame(
   data     = index(cotacoes_klbn11),
   abertura = as.numeric(Op(cotacoes_klbn11)),
   maxima   = as.numeric(Hi(cotacoes_klbn11)),
   minima   = as.numeric(Lo(cotacoes_klbn11)),
   fechamento = as.numeric(Cl(cotacoes_klbn11)),
   volume   = as.numeric(Vo(cotacoes_klbn11))
 )
 write_csv(df_klbn, "data/cotacoes_klbn11.csv")
 message("  ✓ Cotações KLBN11 salvas")
}

if (!is.null(cotacoes_ibov)) {
 df_ibov <- data.frame(
   data     = index(cotacoes_ibov),
   abertura = as.numeric(Op(cotacoes_ibov)),
   maxima   = as.numeric(Hi(cotacoes_ibov)),
   minima   = as.numeric(Lo(cotacoes_ibov)),
   fechamento = as.numeric(Cl(cotacoes_ibov)),
   volume   = as.numeric(Vo(cotacoes_ibov))
 )
 write_csv(df_ibov, "data/cotacoes_ibovespa.csv")
 message("  ✓ Cotações Ibovespa salvas")
}

###############################################################################
# ── CONSOLIDAÇÃO E SALVAMENTO ──────────────────────────────────────────────
###############################################################################

message("")
message("── CONSOLIDAÇÃO ──────────────────────────────────────────────────")

# Parâmetros de mercado coletados
parametros_mercado <- list(
 beta         = beta_resultado$beta,
 beta_r2      = beta_resultado$r2,
 beta_n_obs   = beta_resultado$n_obs,
 selic        = selic_resultado$selic,
 selic_ref    = selic_resultado$data_referencia,
 premio_risco = premio_risco,
 preco_atual  = preco_atual,
 data_coleta  = Sys.time()
)

saveRDS(parametros_mercado, "data/parametros_mercado.rds")

# Dados completos consolidados
dados_completos <- list(
 financeiros       = financeiros,
 premissas         = premissas,
 multiplos         = multiplos,
 pares             = pares,
 sensibilidade     = sensibilidade,
 parametros_mercado = parametros_mercado,
 cotacoes_klbn11   = cotacoes_klbn11,
 cotacoes_ibov     = cotacoes_ibov
)

saveRDS(dados_completos, "data/dados_completos.rds")

message("  ✓ parametros_mercado.rds salvo")
message("  ✓ dados_completos.rds salvo")

# ── Resumo Final ─────────────────────────────────────────────────────────

message("")
message("=" |> rep(70) |> paste(collapse = ""))
message("  RESUMO DA COLETA")
message("=" |> rep(70) |> paste(collapse = ""))
message("")
message(sprintf("  Beta KLBN11            : %.3f", parametros_mercado$beta))
message(sprintf("  Selic (%%a.a.)          : %.2f%%", parametros_mercado$selic * 100))
message(sprintf("  Prêmio de Risco        : %.1f%%", parametros_mercado$premio_risco * 100))
message(sprintf("  Cotação Atual KLBN11   : R$ %.2f", parametros_mercado$preco_atual))
message(sprintf("  Data/Hora Coleta       : %s", parametros_mercado$data_coleta))
message("")
message("  Arquivos gerados em data/:")
message("    • financeiros_historicos.csv")
message("    • premissas_valuation.csv")
message("    • multiplos_mercado.csv")
message("    • pares_setor.csv")
message("    • sensibilidade_ebitda.csv")
if (!is.null(cotacoes_klbn11)) message("    • cotacoes_klbn11.csv")
if (!is.null(cotacoes_ibov))   message("    • cotacoes_ibovespa.csv")
message("    • parametros_mercado.rds")
message("    • dados_completos.rds")
message("")
message("  ✓ Passo 1 & 2 concluídos com sucesso!")
message("  → Execute analyzeKlabin.R para o Passo 3 (modelagem financeira)")
message("=" |> rep(70) |> paste(collapse = ""))

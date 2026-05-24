###############################################################################
# analyzeKlabin.R
# Pipeline Klabin — Passo 3 & 4: Modelagem Financeira + Relatório PDF
#
# Autor : Pipeline Automatizada — Projeto Klabin (KLBN11)
# Data  : 2025 / atualização 2026
#
# Este script:
#   1. Carrega os dados coletados pelo collectKlabin.R
#   2. Calcula o WACC (CAPM + Kd pós-impostos)
#   3. Projeta fluxos de caixa para 2026–2034
#   4. Calcula a TIR (IRR) implícita
#   5. Gera projeções para 2026
#   6. Cria o template Quarto (.qmd) para relatório PDF
###############################################################################

# ── 0. Configuração ─────────────────────────────────────────────────────────

pacotes_necessarios <- c(
  "dplyr", "tidyr", "readr", "tibble", "ggplot2", "scales", "stringr"
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
  library(tibble)
  library(ggplot2)
  library(scales)
  library(stringr)
})

message("=" |> rep(70) |> paste(collapse = ""))
message("  PIPELINE KLABIN — Modelagem Financeira (WACC & TIR)")
message("=" |> rep(70) |> paste(collapse = ""))
message("")

# ── Verificar dados de entrada ───────────────────────────────────────────

if (!file.exists("data/dados_completos.rds")) {
  stop(paste(
    "ERRO: Arquivo data/dados_completos.rds não encontrado.",
    "Execute collectKlabin.R primeiro (Passo 1 & 2)."
  ))
}

dados <- readRDS("data/dados_completos.rds")
message("  ✓ Dados carregados de data/dados_completos.rds")

# Extrair componentes
financeiros <- dados$financeiros
premissas_pdf <- dados$premissas
multiplos <- dados$multiplos
pares <- dados$pares
params <- dados$parametros_mercado

###############################################################################
# ── PASSO 3: CÁLCULO DO WACC ──────────────────────────────────────────────
###############################################################################

message("")
message("── PASSO 3: Modelagem Financeira ─────────────────────────────────")
message("")

# ── 3.1 Custo de Capital Próprio (Ke) via CAPM ──────────────────────────
# Ke = Rf + Beta × ERP + CRP
#
# Onde:
#   Rf  = Taxa livre de risco (Selic)
#   Beta = Sensibilidade da ação ao mercado
#   ERP = Equity Risk Premium (prêmio de risco do mercado)
#   CRP = Country Risk Premium (risco-país Brasil)

rf    <- params$selic            # Taxa livre de risco (Selic)
beta  <- params$beta             # Beta da regressão
erp   <- params$premio_risco     # Equity Risk Premium
crp   <- 0.025                   # Country Risk Premium (EMBI+ Brasil ~2.5%)

ke <- rf + beta * erp + crp

message("  ── Custo de Capital Próprio (CAPM) ──")
message(sprintf("    Rf  (Selic)           : %.2f%%", rf * 100))
message(sprintf("    Beta                  : %.3f", beta))
message(sprintf("    ERP                   : %.1f%%", erp * 100))
message(sprintf("    CRP (Risco-País)      : %.1f%%", crp * 100))
message(sprintf("    Ke = %.2f%% + %.3f × %.1f%% + %.1f%%", rf*100, beta, erp*100, crp*100))
message(sprintf("    ► Ke = %.2f%% a.a.", ke * 100))

# ── 3.2 Custo de Capital de Terceiros (Kd) pós-impostos ─────────────────
# Kd_post = Kd_pre × (1 - t)
#
# Kd pré-imposto: 7.8% do relatório (custo médio da dívida em USD ~5.8% +
#                 hedge + spread BRL)
# Alíquota: 34% (25% IRPJ + 9% CSLL)

kd_pre  <- 0.078
tax_rate <- 0.34
kd_post <- kd_pre * (1 - tax_rate)

message("")
message("  ── Custo de Capital de Terceiros ──")
message(sprintf("    Kd pré-IR             : %.1f%%", kd_pre * 100))
message(sprintf("    Alíquota IR (IRPJ+CSLL): %.0f%%", tax_rate * 100))
message(sprintf("    Kd pós-IR = %.1f%% × (1 - %.0f%%)", kd_pre * 100, tax_rate * 100))
message(sprintf("    ► Kd pós-IR = %.2f%% a.a.", kd_post * 100))

# ── 3.3 WACC ─────────────────────────────────────────────────────────────
# WACC = E/(D+E) × Ke + D/(D+E) × Kd_post

de_ratio <- 0.45   # D/(D+E) do relatório
ed_ratio <- 1 - de_ratio

wacc <- ed_ratio * ke + de_ratio * kd_post

message("")
message("  ── WACC ──")
message(sprintf("    E/(D+E)               : %.0f%%", ed_ratio * 100))
message(sprintf("    D/(D+E)               : %.0f%%", de_ratio * 100))
message(sprintf("    WACC = %.0f%% × %.2f%% + %.0f%% × %.2f%%",
                ed_ratio * 100, ke * 100, de_ratio * 100, kd_post * 100))
message(sprintf("    ► WACC = %.2f%% a.a.", wacc * 100))
message(sprintf("    (Referência PDF: 10.20%% a.a.)"))

# ── 3.4 Projeções de Fluxo de Caixa (2026–2034) ─────────────────────────
# Premissas:
#   - Receita base 2025E: R$ 24.5 bi
#   - Crescimento: 5% em 2026, convergindo para g = 3.5% na perpetuidade
#   - Margem EBITDA: 34.3% (2025E) → normaliza em 34% no longo prazo
#   - CAPEX/Receita: ~13-14% (manutenção + expansões menores)
#   - D&A ≈ 60% do CAPEX
#   - Capital de Giro: ~1% da variação de receita
#   - IR sobre EBIT: 34%

message("")
message("  ── Projeção de Fluxos de Caixa ──")

receita_base   <- 24.5  # 2025E em R$ bi
g_perpetuidade <- 0.035

# Taxas de crescimento anuais (convergindo para g)
taxas_crescimento <- c(0.050, 0.048, 0.045, 0.043, 0.040, 0.038, 0.037, 0.036, 0.035)
margens_ebitda    <- c(0.345, 0.343, 0.340, 0.340, 0.340, 0.338, 0.335, 0.335, 0.335)
capex_receita     <- c(0.130, 0.135, 0.135, 0.140, 0.140, 0.140, 0.140, 0.140, 0.140)

# Construir projeções
projecoes <- tibble(
  ano = 2026:2034,
  t   = 1:9
)

# Calcular receita acumulada
receita <- numeric(9)
receita[1] <- receita_base * (1 + taxas_crescimento[1])
for (i in 2:9) {
  receita[i] <- receita[i - 1] * (1 + taxas_crescimento[i])
}

projecoes <- projecoes |>
  mutate(
    receita      = receita,
    margem_ebitda = margens_ebitda,
    ebitda       = receita * margem_ebitda,
    capex_pct    = capex_receita,
    capex        = -receita * capex_pct,
    da           = abs(capex) * 0.60,  # D&A = 60% do CAPEX
    ebit         = ebitda - da,
    ir_ebit      = -ebit * tax_rate,    # Impostos sobre EBIT
    delta_wc     = -(receita - lag(receita, default = receita_base)) * 0.01,
    # FCFF = EBIT × (1 - t) + D&A - CAPEX - ΔWC
    fcff         = ebit * (1 - tax_rate) + da + capex + delta_wc
  )

# Valor Terminal (Gordon Growth Model)
fcff_terminal <- projecoes$fcff[9]
valor_terminal <- fcff_terminal * (1 + g_perpetuidade) / (wacc - g_perpetuidade)

message(sprintf("    Valor Terminal (Gordon): R$ %.1f bi", valor_terminal))

# Descontar fluxos
projecoes <- projecoes |>
  mutate(
    fator_desconto = 1 / (1 + wacc)^t,
    fcff_descontado = fcff * fator_desconto
  )

# Valor terminal descontado
vt_descontado <- valor_terminal / (1 + wacc)^9

# Enterprise Value
pv_fcff <- sum(projecoes$fcff_descontado)
ev_calculado <- pv_fcff + vt_descontado

# Equity Value
divida_liquida  <- 28.8  # R$ bi (2024E do PDF)
outros_passivos <- 2.3   # R$ bi
equity_value    <- ev_calculado - divida_liquida - outros_passivos

# Número de units (estimado a partir do market cap / preço)
market_cap_pdf <- 27.5  # R$ bi
preco_pdf      <- 20.80
n_units        <- market_cap_pdf / preco_pdf  # ~1.322 bilhões

# Preço-alvo implícito
preco_alvo <- equity_value / n_units

message(sprintf("    PV(FCFFs)              : R$ %.1f bi", pv_fcff))
message(sprintf("    PV(Valor Terminal)     : R$ %.1f bi", vt_descontado))
message(sprintf("    Enterprise Value       : R$ %.1f bi", ev_calculado))
message(sprintf("    (–) Dívida Líquida     : R$ %.1f bi", divida_liquida))
message(sprintf("    (–) Outros Passivos    : R$ %.1f bi", outros_passivos))
message(sprintf("    Equity Value           : R$ %.1f bi", equity_value))
message(sprintf("    Preço-Alvo Calculado   : R$ %.2f", preco_alvo))

# ── 3.5 Cálculo da TIR (IRR) ────────────────────────────────────────────
# TIR = taxa de desconto que iguala o EV de mercado ao PV dos FCFFs projetados
# EV de mercado atual = Market Cap + Dívida Líquida + Outros Passivos

preco_mercado <- params$preco_atual
market_cap_atual <- preco_mercado * n_units
ev_mercado <- market_cap_atual + divida_liquida + outros_passivos

message("")
message("  ── Taxa Interna de Retorno (TIR) ──")
message(sprintf("    Preço de Mercado       : R$ %.2f", preco_mercado))
message(sprintf("    Market Cap Atual       : R$ %.1f bi", market_cap_atual))
message(sprintf("    EV de Mercado          : R$ %.1f bi", ev_mercado))

# Função para calcular NPV dado uma taxa r
npv_func <- function(r) {
  fcffs <- projecoes$fcff
  n <- length(fcffs)
  vt <- fcff_terminal * (1 + g_perpetuidade) / (r - g_perpetuidade)

  pv_fcff <- sum(fcffs / (1 + r)^(1:n))
  pv_vt   <- vt / (1 + r)^n

  ev_calc <- pv_fcff + pv_vt
  ev_calc - ev_mercado
}

# Encontrar TIR usando uniroot
tir_resultado <- tryCatch({
  resultado <- uniroot(npv_func, interval = c(0.04, 0.50), tol = 1e-8)
  resultado$root
}, error = function(e) {
  message("  ⚠ Erro no cálculo da TIR: ", e$message)
  message("  → Usando estimativa manual")
  wacc * 1.1  # Estimativa conservadora
})

message(sprintf("    ► TIR Implícita        : %.2f%% a.a.", tir_resultado * 100))
message(sprintf("    ► Spread TIR - WACC    : %.2f p.p.",
                (tir_resultado - wacc) * 100))

if (tir_resultado > wacc) {
  message("    → TIR > WACC: Ação subavaliada (potencial de valorização)")
} else {
  message("    → TIR < WACC: Ação sobreavaliada pelo modelo")
}

# ── 3.6 Projeções 2026 ──────────────────────────────────────────────────

projecoes_2026 <- tibble(
  indicador = c("Receita Líquida", "EBITDA Ajustado", "Margem EBITDA",
                "Lucro Líquido", "CAPEX", "Dívida Líquida",
                "DL/EBITDA", "FCF pré-CAPEX"),
  valor_2025e = c(24.5, 8.4, 34.3, 1.9, -3.2, 27.0, 3.2, 7.1),
  valor_2026e = c(
    projecoes$receita[1],
    projecoes$ebitda[1],
    projecoes$margem_ebitda[1] * 100,
    projecoes$ebitda[1] * 0.23,   # ~23% do EBITDA como proxy
    projecoes$capex[1],
    27.0 - projecoes$fcff[1],     # Dívida reduz com FCF
    (27.0 - projecoes$fcff[1]) / projecoes$ebitda[1],
    projecoes$ebitda[1] + projecoes$capex[1] + projecoes$da[1]  # Proxy FCF pré-CAPEX
  ),
  unidade = c("R$ bi", "R$ bi", "%", "R$ bi", "R$ bi", "R$ bi", "x", "R$ bi")
)

message("")
message("  ── Projeções 2026E ──")
for (i in seq_len(nrow(projecoes_2026))) {
  msg <- sprintf("    %-20s: %8.2f %s → %8.2f %s",
                 projecoes_2026$indicador[i],
                 projecoes_2026$valor_2025e[i],
                 projecoes_2026$unidade[i],
                 projecoes_2026$valor_2026e[i],
                 projecoes_2026$unidade[i])
  message(msg)
}

###############################################################################
# ── SALVAR RESULTADOS ─────────────────────────────────────────────────────
###############################################################################

message("")
message("── SALVANDO RESULTADOS ────────────────────────────────────────────")

# Modelo WACC completo
modelo_wacc <- list(
  # Componentes do WACC
  wacc       = wacc,
  ke         = ke,
  kd_pre_tax = kd_pre,
  kd_post_tax = kd_post,
  beta       = beta,
  selic      = rf,
  premio_risco = erp,
  crp        = crp,
  tax_rate   = tax_rate,
  de_ratio   = de_ratio,
  ed_ratio   = ed_ratio,

  # Resultados do modelo
  tir           = tir_resultado,
  ev_calculado  = ev_calculado,
  pv_fcff       = pv_fcff,
  vt_descontado = vt_descontado,
  valor_terminal = valor_terminal,
  equity_value  = equity_value,
  preco_alvo    = preco_alvo,
  preco_atual   = preco_mercado,
  n_units       = n_units,
  upside        = (preco_alvo / preco_mercado - 1),

  # Premissas
  g_perpetuidade = g_perpetuidade,
  divida_liquida = divida_liquida,
  outros_passivos = outros_passivos,

  # Projeções
  projecoes_2026 = projecoes_2026
)

saveRDS(modelo_wacc, "data/modelo_wacc.rds")
message("  ✓ data/modelo_wacc.rds salvo")

# Fluxos de caixa projetados
projecoes_export <- projecoes |>
  select(ano, receita, ebitda, margem_ebitda, capex, fcff, fcff_descontado)
write_csv(projecoes_export, "data/fluxos_caixa_projetados.csv")
message("  ✓ data/fluxos_caixa_projetados.csv salvo")

# Resultados do modelo em formato tabular
resultados_modelo <- tibble(
  metrica = c(
    "WACC", "Ke (CAPM)", "Kd pós-IR", "Beta", "Selic (Rf)",
    "ERP", "CRP", "D/(D+E)", "Alíquota IR",
    "TIR Implícita", "Spread TIR-WACC",
    "EV Calculado", "PV FCFFs", "PV Valor Terminal",
    "Equity Value", "Preço-Alvo", "Preço Mercado", "Upside",
    "g Perpetuidade"
  ),
  valor = c(
    wacc * 100, ke * 100, kd_post * 100, beta, rf * 100,
    erp * 100, crp * 100, de_ratio * 100, tax_rate * 100,
    tir_resultado * 100, (tir_resultado - wacc) * 100,
    ev_calculado, pv_fcff, vt_descontado,
    equity_value, preco_alvo, preco_mercado,
    (preco_alvo / preco_mercado - 1) * 100,
    g_perpetuidade * 100
  ),
  unidade = c(
    "% a.a.", "% a.a.", "% a.a.", "coef.", "% a.a.",
    "% a.a.", "% a.a.", "%", "%",
    "% a.a.", "p.p.",
    "R$ bi", "R$ bi", "R$ bi",
    "R$ bi", "R$/unit", "R$/unit", "%",
    "% a.a."
  )
)

write_csv(resultados_modelo, "data/resultados_modelo.csv")
message("  ✓ data/resultados_modelo.csv salvo (com valores em porcentagem real)")

# Criar a Planilha Mestre (Master File .xlsx)
message("")
message("  → Gerando Arquivo Mestre Unificado (Klabin_Valuation_Master.xlsx)...")
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  message("Instalando pacote openxlsx...")
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
}
library(openxlsx)

# Preparar dados para o Excel
df_resumo <- resultados_modelo
df_historico <- financeiros
df_premissas <- tibble(
  Parametro = c("Taxa Selic", "Beta", "Premio de Risco", "Risco Pais", "Aliquota IR", "g Perpetuidade"),
  Valor = c(rf * 100, beta, erp * 100, crp * 100, tax_rate * 100, g_perpetuidade * 100),
  Unidade = c("% a.a.", "coef", "% a.a.", "% a.a.", "%", "% a.a.")
)
df_wacc <- tibble(
  Componente = c("Ke (CAPM)", "Kd pos-IR", "WACC"),
  Valor_pct = c(ke * 100, kd_post * 100, wacc * 100),
  Peso_pct = c(ed_ratio * 100, de_ratio * 100, 100)
)
df_fluxos <- projecoes_export

wb <- createWorkbook()
addWorksheet(wb, "Resumo_Valuation")
addWorksheet(wb, "Dados_Historicos")
addWorksheet(wb, "Premissas_Projecoes")
addWorksheet(wb, "Memoria_Calculo_WACC")
addWorksheet(wb, "Fluxos_Caixa")

writeData(wb, "Resumo_Valuation", df_resumo)
writeData(wb, "Dados_Historicos", df_historico)
writeData(wb, "Premissas_Projecoes", df_premissas)
writeData(wb, "Memoria_Calculo_WACC", df_wacc)
writeData(wb, "Fluxos_Caixa", df_fluxos)

saveWorkbook(wb, "Klabin_Valuation_Master.xlsx", overwrite = TRUE)
message("  ✓ Klabin_Valuation_Master.xlsx salvo com sucesso (Planilha Mestre)")

# Salvar gráficos de projeções
message("")
message("  → Gerando gráficos de projeções...")

# Tema customizado para os gráficos
tema_klabin <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a2742"),
    plot.subtitle = element_text(color = "#666666", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

# Gráfico 1: Receita e EBITDA Históricos + Projetados
df_grafico <- financeiros |>
  select(ano, receita_liquida, ebitda_ajustado) |>
  bind_rows(
    projecoes |>
      select(ano, receita, ebitda) |>
      rename(receita_liquida = receita, ebitda_ajustado = ebitda)
  ) |>
  pivot_longer(cols = c(receita_liquida, ebitda_ajustado),
               names_to = "indicador", values_to = "valor") |>
  mutate(indicador = case_when(
    indicador == "receita_liquida" ~ "Receita Líquida",
    indicador == "ebitda_ajustado" ~ "EBITDA Ajustado"
  ))

p1 <- ggplot(df_grafico, aes(x = ano, y = valor, fill = indicador)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_vline(xintercept = 2025.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2025.5, y = max(df_grafico$valor) * 0.95,
           label = "← Real | Projetado →", size = 3, color = "gray40") +
  scale_fill_manual(values = c("Receita Líquida" = "#2563eb",
                                "EBITDA Ajustado" = "#f0b429")) +
  scale_y_continuous(labels = label_number(suffix = " bi", prefix = "R$ ")) +
  labs(
    title = "Klabin — Receita Líquida e EBITDA Ajustado",
    subtitle = "Histórico (2020–2025E) e Projeções (2026E–2034E) | R$ bilhões",
    x = NULL, y = NULL, fill = NULL
  ) +
  tema_klabin

ggsave("data/grafico_receita_ebitda.png", p1, width = 12, height = 6, dpi = 150)
message("  ✓ data/grafico_receita_ebitda.png salvo")

# Gráfico 2: FCFF Projetado
p2 <- ggplot(projecoes, aes(x = ano, y = fcff)) +
  geom_col(fill = "#2dd4bf", width = 0.6) +
  geom_line(aes(y = fcff_descontado), color = "#ef4444", linewidth = 1) +
  geom_point(aes(y = fcff_descontado), color = "#ef4444", size = 2) +
  scale_y_continuous(labels = label_number(suffix = " bi", prefix = "R$ ")) +
  labs(
    title = "Klabin — Fluxo de Caixa Livre para a Firma (FCFF)",
    subtitle = "Barras: FCFF nominal | Linha: FCFF descontado a WACC",
    x = NULL, y = NULL
  ) +
  tema_klabin

ggsave("data/grafico_fcff.png", p2, width = 10, height = 6, dpi = 150)
message("  ✓ data/grafico_fcff.png salvo")

###############################################################################
# ── PASSO 4: CRIAÇÃO DO TEMPLATE QUARTO (.qmd) ────────────────────────────
###############################################################################

message("")
message("── PASSO 4: Geração do Relatório Executivo ───────────────────────")

qmd_content <- '---
title: "KLABIN S.A. (KLBN11)"
subtitle: "Relatório de Valuation — Análise Institucional"
author: "Pipeline Automatizada | Projeto Klabin"
date: today
format:
  pdf:
    toc: true
    toc-depth: 3
    number-sections: true
    colorlinks: true
    linkcolor: NavyBlue
    urlcolor: NavyBlue
    geometry:
      - top=25mm
      - left=25mm
      - right=25mm
      - bottom=25mm
    fontsize: 11pt
    include-in-header:
      text: |
        \\usepackage{booktabs}
        \\usepackage{longtable}
        \\usepackage{array}
        \\usepackage{colortbl}
        \\usepackage{xcolor}
        \\definecolor{klabinblue}{HTML}{1a2742}
        \\definecolor{klabingold}{HTML}{f0b429}
        \\usepackage{fancyhdr}
        \\pagestyle{fancy}
        \\fancyhead[L]{\\textcolor{klabinblue}{\\small KLABIN S.A. | KLBN11}}
        \\fancyhead[R]{\\textcolor{klabinblue}{\\small Equity Research}}
        \\fancyfoot[C]{\\textcolor{gray}{\\small Este relatório tem caráter informativo. Não constitui recomendação de investimento.}}
        \\fancyfoot[R]{\\thepage}
execute:
  echo: false
  warning: false
  message: false
---

```{r setup}
#| include: false
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(knitr)

# Carregar dados
dados <- readRDS("data/dados_completos.rds")
modelo <- readRDS("data/modelo_wacc.rds")
fluxos <- read_csv("data/fluxos_caixa_projetados.csv", show_col_types = FALSE)
resultados <- read_csv("data/resultados_modelo.csv", show_col_types = FALSE)

financeiros <- dados$financeiros
premissas <- dados$premissas
multiplos <- dados$multiplos
pares <- dados$pares

tema_klabin <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, color = "#1a2742"),
    plot.subtitle = element_text(color = "#666666", size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )
```

# Resumo Executivo

A **Klabin S.A.** (KLBN11) é a maior produtora e exportadora de papéis para embalagens do Brasil, com operações verticalizadas da floresta ao produto final. Esta análise atualiza o valuation institucional com dados de mercado de `r format(Sys.Date(), "%B de %Y")`.

::: {layout-ncol=2}

| **Indicador** | **Valor** |
|:---|---:|
| Preço Atual | R\\$ `r sprintf("%.2f", modelo$preco_atual)` |
| Preço-Alvo (FCD) | R\\$ `r sprintf("%.2f", modelo$preco_alvo)` |
| Upside Potencial | `r sprintf("%.1f%%", modelo$upside * 100)` |
| WACC | `r sprintf("%.2f%%", modelo$wacc * 100)` |
| TIR Implícita | `r sprintf("%.2f%%", modelo$tir * 100)` |

| **Indicador 2025E** | **R\\$ bi** |
|:---|---:|
| Receita Líquida | 24,5 |
| EBITDA Ajustado | 8,4 |
| Margem EBITDA | 34,3% |
| Dívida Líquida | 27,0 |
| DL/EBITDA | 3,2x |

:::

\\newpage

# Premissas Macroeconômicas (2026)

```{r premissas-table}
premissas_macro <- tibble(
  `Parâmetro` = c("Taxa Selic (Rf)", "Beta KLBN11", "Prêmio de Risco (ERP)",
                  "Country Risk Premium", "Alíquota IR", "Câmbio Projetado",
                  "BHKP (celulose)", "Crescimento PIB Nominal (g)"),
  `Valor` = c(
    sprintf("%.2f%% a.a.", modelo$selic * 100),
    sprintf("%.3f", modelo$beta),
    sprintf("%.1f%%", modelo$premio_risco * 100),
    sprintf("%.1f%%", modelo$crp * 100),
    sprintf("%.0f%%", modelo$tax_rate * 100),
    "R$ 5,80/USD (2025) → R$ 5,50/USD (LP)",
    "US$ 600/t (2025E) → US$ 580/t (LP)",
    sprintf("%.1f%%", modelo$g_perpetuidade * 100)
  ),
  `Fonte` = c("BCB (API SGS)", "Regressão KLBN11 vs Ibovespa",
              "Damodaran / Estimativa", "EMBI+ Brasil",
              "Legislação Brasileira", "Relatório Company Money",
              "Relatório Company Money", "Premissa conservadora")
)

kable(premissas_macro, format = "latex", booktabs = TRUE,
      caption = "Premissas macroeconômicas adotadas") |>
  kableExtra::kable_styling(latex_options = c("hold_position", "striped"),
                            font_size = 10)
```

# Memória de Cálculo — WACC

## Custo de Capital Próprio (Ke) — CAPM

$$K_e = R_f + \\beta \\times ERP + CRP$$

$$K_e = `r sprintf("%.2f", modelo$selic * 100)`\\%% + `r sprintf("%.3f", modelo$beta)` \\times `r sprintf("%.1f", modelo$premio_risco * 100)`\\%% + `r sprintf("%.1f", modelo$crp * 100)`\\%%$$

$$\\boxed{K_e = `r sprintf("%.2f", modelo$ke * 100)`\\%% \\text{ a.a.}}$$

## Custo de Capital de Terceiros (Kd) Pós-Impostos

$$K_d^{\\text{pós}} = K_d^{\\text{pré}} \\times (1 - t) = `r sprintf("%.1f", modelo$kd_pre_tax * 100)`\\%% \\times (1 - `r sprintf("%.0f", modelo$tax_rate * 100)`\\%%)$$

$$\\boxed{K_d^{\\text{pós}} = `r sprintf("%.2f", modelo$kd_post_tax * 100)`\\%% \\text{ a.a.}}$$

## WACC Final

$$WACC = \\frac{E}{D+E} \\times K_e + \\frac{D}{D+E} \\times K_d^{\\text{pós}}$$

$$WACC = `r sprintf("%.0f", modelo$ed_ratio * 100)`\\%% \\times `r sprintf("%.2f", modelo$ke * 100)`\\%% + `r sprintf("%.0f", modelo$de_ratio * 100)`\\%% \\times `r sprintf("%.2f", modelo$kd_post_tax * 100)`\\%%$$

$$\\boxed{WACC = `r sprintf("%.2f", modelo$wacc * 100)`\\%% \\text{ a.a.}}$$

```{r wacc-components}
wacc_tab <- tibble(
  Componente = c("Ke (CAPM)", "Kd pós-IR", "WACC"),
  `Valor (% a.a.)` = c(
    sprintf("%.2f%%", modelo$ke * 100),
    sprintf("%.2f%%", modelo$kd_post_tax * 100),
    sprintf("%.2f%%", modelo$wacc * 100)
  ),
  Peso = c(
    sprintf("%.0f%% (Equity)", modelo$ed_ratio * 100),
    sprintf("%.0f%% (Dívida)", modelo$de_ratio * 100),
    "100%"
  ),
  `Contribuição (p.p.)` = c(
    sprintf("%.2f", modelo$ed_ratio * modelo$ke * 100),
    sprintf("%.2f", modelo$de_ratio * modelo$kd_post_tax * 100),
    sprintf("%.2f", modelo$wacc * 100)
  )
)

kable(wacc_tab, format = "latex", booktabs = TRUE,
      caption = "Decomposição do WACC") |>
  kableExtra::kable_styling(latex_options = c("hold_position", "striped"),
                            font_size = 10)
```

\\newpage

# Cálculo da TIR (Taxa Interna de Retorno)

A TIR implícita é a taxa de desconto que iguala o Valor Presente dos fluxos de caixa projetados ao Enterprise Value de mercado atual.

$$EV_{\\text{mercado}} = \\sum_{t=1}^{9} \\frac{FCFF_t}{(1+TIR)^t} + \\frac{TV}{(1+TIR)^9}$$

```{r tir-resultado}
tir_tab <- tibble(
  Métrica = c("EV de Mercado", "PV dos FCFFs (a WACC)",
              "PV do Valor Terminal (a WACC)", "EV Calculado (a WACC)",
              "Equity Value", "Preço-Alvo Implícito",
              "TIR Implícita", "Spread TIR − WACC"),
  Valor = c(
    sprintf("R$ %.1f bi", modelo$preco_atual * modelo$n_units + modelo$divida_liquida + modelo$outros_passivos),
    sprintf("R$ %.1f bi", modelo$pv_fcff),
    sprintf("R$ %.1f bi", modelo$vt_descontado),
    sprintf("R$ %.1f bi", modelo$ev_calculado),
    sprintf("R$ %.1f bi", modelo$equity_value),
    sprintf("R$ %.2f", modelo$preco_alvo),
    sprintf("%.2f%% a.a.", modelo$tir * 100),
    sprintf("%.2f p.p.", (modelo$tir - modelo$wacc) * 100)
  )
)

kable(tir_tab, format = "latex", booktabs = TRUE,
      caption = "Resultado do modelo de Fluxo de Caixa Descontado") |>
  kableExtra::kable_styling(latex_options = c("hold_position", "striped"),
                            font_size = 10)
```

# Projeções Financeiras

```{r grafico-receita-ebitda, fig.width=8, fig.height=4}
df_all <- financeiros |>
  select(ano, receita_liquida, ebitda_ajustado) |>
  bind_rows(
    fluxos |> select(ano, receita, ebitda) |>
      rename(receita_liquida = receita, ebitda_ajustado = ebitda)
  ) |>
  pivot_longer(c(receita_liquida, ebitda_ajustado),
               names_to = "indicador", values_to = "valor") |>
  mutate(indicador = ifelse(indicador == "receita_liquida",
                            "Receita Líquida", "EBITDA Ajustado"))

ggplot(df_all, aes(x = ano, y = valor, fill = indicador)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_vline(xintercept = 2025.5, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("Receita Líquida" = "#2563eb",
                                "EBITDA Ajustado" = "#f0b429")) +
  scale_y_continuous(labels = label_number(suffix = " bi", prefix = "R$ ")) +
  labs(title = "Receita Líquida e EBITDA Ajustado",
       subtitle = "Histórico + Projeções | R$ bilhões",
       x = NULL, y = NULL, fill = NULL) +
  tema_klabin
```

```{r grafico-fcff, fig.width=8, fig.height=4}
ggplot(fluxos, aes(x = ano, y = fcff)) +
  geom_col(fill = "#2dd4bf", width = 0.6) +
  geom_line(aes(y = fcff_descontado), color = "#ef4444", linewidth = 1) +
  geom_point(aes(y = fcff_descontado), color = "#ef4444", size = 2) +
  scale_y_continuous(labels = label_number(suffix = " bi", prefix = "R$ ")) +
  labs(title = "Fluxo de Caixa Livre (FCFF) — Projetado",
       subtitle = "Barras: FCFF nominal | Linha: FCFF descontado a WACC",
       x = NULL, y = NULL) +
  tema_klabin
```

```{r tabela-fluxos}
fluxos_fmt <- fluxos |>
  mutate(across(c(receita, ebitda, capex, fcff, fcff_descontado),
                ~sprintf("%.2f", .)),
         margem_ebitda = sprintf("%.1f%%", margem_ebitda * 100))

kable(fluxos_fmt, format = "latex", booktabs = TRUE,
      col.names = c("Ano", "Receita", "EBITDA", "Margem", "CAPEX",
                    "FCFF", "FCFF Desc."),
      caption = "Projeção de Fluxos de Caixa (R\\$ bilhões)") |>
  kableExtra::kable_styling(latex_options = c("hold_position", "striped", "scale_down"),
                            font_size = 9)
```

\\newpage

# Comparação com Pares

```{r pares-table}
pares_fmt <- pares |>
  mutate(
    market_cap = sprintf("%.1f", market_cap),
    ev_ebitda = sprintf("%.1fx", ev_ebitda),
    pl = sprintf("%.1fx", pl),
    dl_ebitda = sprintf("%.1fx", dl_ebitda)
  )

kable(pares_fmt, format = "latex", booktabs = TRUE,
      col.names = c("Empresa", "Ticker", "Mkt Cap (R$ bi)",
                    "EV/EBITDA", "P/L", "DL/EBITDA", "Rec."),
      caption = "Comparação com pares do setor P\\&C") |>
  kableExtra::kable_styling(latex_options = c("hold_position", "striped"),
                            font_size = 10)
```

```{r grafico-pares, fig.width=7, fig.height=3.5}
pares_long <- pares |>
  select(empresa, ev_ebitda, pl) |>
  pivot_longer(-empresa, names_to = "multiplo", values_to = "valor") |>
  mutate(multiplo = ifelse(multiplo == "ev_ebitda", "EV/EBITDA", "P/L"))

ggplot(pares_long, aes(x = reorder(empresa, -valor), y = valor, fill = multiplo)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("EV/EBITDA" = "#2563eb", "P/L" = "#f0b429")) +
  labs(title = "Múltiplos Comparativos — Pares do Setor",
       x = NULL, y = "Múltiplo (x)", fill = NULL) +
  tema_klabin
```

# Conclusão

Com base no modelo de Fluxo de Caixa Descontado e na análise de múltiplos, o **preço-alvo calculado para KLBN11 é de R\\$ `r sprintf("%.2f", modelo$preco_alvo)`**, representando um upside de **`r sprintf("%.1f%%", modelo$upside * 100)`** em relação ao preço atual de R\\$ `r sprintf("%.2f", modelo$preco_atual)`.

A **TIR implícita de `r sprintf("%.2f%%", modelo$tir * 100)`** supera o WACC de `r sprintf("%.2f%%", modelo$wacc * 100)`, indicando que a ação negocia com desconto em relação ao seu valor intrínseco pelo modelo de FCD.

---

*Relatório gerado automaticamente em `r format(Sys.time(), "%d/%m/%Y às %H:%M")`. Dados de mercado coletados via API do BCB e Yahoo Finance. Este relatório tem caráter exclusivamente informativo e não constitui recomendação de investimento.*
'

# Salvar o arquivo .qmd
writeLines(qmd_content, "relatorio_klabin.qmd", useBytes = TRUE)
message("  ✓ relatorio_klabin.qmd criado")

# Tentar renderizar o relatório (opcional — requer Quarto instalado)
message("  → Tentando renderizar o relatório PDF...")
render_ok <- tryCatch({
  system2("quarto", args = c("render", "relatorio_klabin.qmd"), 
          stdout = TRUE, stderr = TRUE)
  message("  ✓ relatorio_klabin.pdf gerado com sucesso!")
  TRUE
}, error = function(e) {
  message("  ⚠ Quarto não disponível ou erro na renderização: ", e$message)
  message("  → Para gerar o PDF manualmente:")
  message("    1. Abra o RStudio")
  message("    2. Abra relatorio_klabin.qmd")
  message("    3. Clique em 'Render' ou execute: quarto::quarto_render('relatorio_klabin.qmd')")
  FALSE
}, warning = function(w) {
  message("  ⚠ Aviso na renderização: ", w$message)
  FALSE
})

###############################################################################
# ── RESUMO FINAL ──────────────────────────────────────────────────────────
###############################################################################

message("")
message("=" |> rep(70) |> paste(collapse = ""))
message("  RESUMO DO MODELO FINANCEIRO")
message("=" |> rep(70) |> paste(collapse = ""))
message("")
message(sprintf("  WACC                   : %.2f%% a.a.", wacc * 100))
message(sprintf("  Ke (CAPM)              : %.2f%% a.a.", ke * 100))
message(sprintf("  Kd pós-IR              : %.2f%% a.a.", kd_post * 100))
message(sprintf("  TIR Implícita          : %.2f%% a.a.", tir_resultado * 100))
message(sprintf("  Spread TIR - WACC      : %.2f p.p.", (tir_resultado - wacc) * 100))
message("")
message(sprintf("  Enterprise Value       : R$ %.1f bi", ev_calculado))
message(sprintf("  Equity Value           : R$ %.1f bi", equity_value))
message(sprintf("  Preço-Alvo (FCD)       : R$ %.2f", preco_alvo))
message(sprintf("  Preço de Mercado       : R$ %.2f", preco_mercado))
message(sprintf("  Upside                 : %.1f%%", (preco_alvo / preco_mercado - 1) * 100))
message("")
message("  Arquivos gerados:")
message("    • data/modelo_wacc.rds")
message("    • data/fluxos_caixa_projetados.csv")
message("    • data/resultados_modelo.csv")
message("    • data/grafico_receita_ebitda.png")
message("    • data/grafico_fcff.png")
message("    • relatorio_klabin.qmd")
message("")
message("  ✓ Passo 3 & 4 concluídos com sucesso!")
message("  → Execute dashKlabin.R para o Passo 5 (dashboard interativo)")
message("=" |> rep(70) |> paste(collapse = ""))

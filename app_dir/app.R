###############################################################################
# dashKlabin.R
# Pipeline Klabin — Passo 5: Dashboard Interativo Shiny
#
# Autor : Pipeline Automatizada — Projeto Klabin (KLBN11)
# Data  : 2025 / atualização 2026
#
# Dashboard interativo com:
#   - Painel Geral: KPIs e gráficos históricos
#   - Valuation: Simulador interativo WACC e TIR
#   - Visualizações: Gráficos detalhados e comparação com pares
###############################################################################

# ── 0. Instalação de Pacotes ────────────────────────────────────────────────

pacotes_necessarios <- c(
  "shiny", "bslib", "bsicons", "plotly", "dplyr", "tidyr",
  "readr", "tibble", "DT", "scales", "htmltools"
)

for (pkg in pacotes_necessarios) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Instalando pacote: ", pkg)
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(DT)
  library(scales)
  library(htmltools)
})

# ── 1. Carregar Dados ───────────────────────────────────────────────────────

carregar_dados <- function() {
  tryCatch({
    dados <- readRDS("data/dados_completos.rds")
    modelo <- readRDS("data/modelo_wacc.rds")
    fluxos <- read_csv("data/fluxos_caixa_projetados.csv", show_col_types = FALSE)
    resultados <- read_csv("data/resultados_modelo.csv", show_col_types = FALSE)

    list(
      dados = dados,
      modelo = modelo,
      fluxos = fluxos,
      resultados = resultados,
      ok = TRUE
    )
  }, error = function(e) {
    list(ok = FALSE, erro = e$message)
  })
}

dados_carregados <- carregar_dados()

if (!dados_carregados$ok) {
  stop(paste(
    "Erro ao carregar dados:", dados_carregados$erro,
    "\nExecute collectKlabin.R e analyzeKlabin.R antes de iniciar o dashboard."
  ))
}

# Extrair dados
dados      <- dados_carregados$dados
modelo     <- dados_carregados$modelo
fluxos     <- dados_carregados$fluxos
resultados <- dados_carregados$resultados

financeiros <- dados$financeiros
multiplos   <- dados$multiplos
pares       <- dados$pares
params      <- dados$parametros_mercado

# ── 2. Configuração de Tema ─────────────────────────────────────────────────

# Paleta de cores premium
cores <- list(
  bg_primary   = "#0a1628",
  bg_secondary = "#1a2742",
  bg_card      = "#1e2d47",
  gold         = "#f0b429",
  teal         = "#2dd4bf",
  blue         = "#3b82f6",
  amber        = "#fbbf24",
  red          = "#ef4444",
  green        = "#22c55e",
  text_primary = "#e2e8f0",
  text_muted   = "#94a3b8",
  border       = "#334155"
)

# Tema bslib premium dark
tema_dash <- bs_theme(
  version = 5,
  preset  = "darkly",
  bg      = cores$bg_primary,
  fg      = cores$text_primary,
  primary = cores$blue,
  secondary = cores$bg_secondary,
  success = cores$green,
  warning = cores$amber,
  danger  = cores$red,
  info    = cores$teal,
  "navbar-bg" = cores$bg_secondary,
  "card-bg"   = cores$bg_card
)

# Layout plotly dark theme
plotly_layout_dark <- function(p, title = "", yaxis_title = "") {
  p |> layout(
    title = list(text = title, font = list(color = cores$text_primary, size = 14)),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(color = cores$text_muted, family = "Segoe UI, sans-serif"),
    xaxis = list(
      gridcolor = cores$border,
      zerolinecolor = cores$border,
      color = cores$text_muted
    ),
    yaxis = list(
      title = yaxis_title,
      gridcolor = cores$border,
      zerolinecolor = cores$border,
      color = cores$text_muted
    ),
    legend = list(
      bgcolor = "rgba(0,0,0,0)",
      font = list(color = cores$text_muted)
    ),
    margin = list(t = 50, b = 40, l = 60, r = 20)
  )
}

# ── 3. UI ────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = tags$span(
    tags$strong("KLABIN S.A."),
    tags$span("| KLBN11 — Dashboard de Valuation",
              style = "font-weight: 300; opacity: 0.8; font-size: 0.85em;")
  ),
  theme = tema_dash,
  fillable = TRUE,

  # ── Aba 1: Painel Geral ──────────────────────────────────────────────────

  nav_panel(
    title = "Painel Geral",
    icon  = bs_icon("speedometer2"),

    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),

      value_box(
        title = "Receita Líquida 2025E",
        value = sprintf("R$ %.1f bi", financeiros$receita_liquida[6]),
        showcase = bs_icon("cash-stack"),
        theme = "primary",
        p(sprintf("Cresc. %.1f%% vs 2024E",
                  (financeiros$receita_liquida[6] / financeiros$receita_liquida[5] - 1) * 100))
      ),
      value_box(
        title = "EBITDA Ajustado 2025E",
        value = sprintf("R$ %.1f bi", financeiros$ebitda_ajustado[6]),
        showcase = bs_icon("graph-up-arrow"),
        theme = "info",
        p(sprintf("Margem: %.1f%%", financeiros$margem_ebitda[6]))
      ),
      value_box(
        title = "Preço-Alvo 12M",
        value = sprintf("R$ %.2f", modelo$preco_alvo),
        showcase = bs_icon("bullseye"),
        theme = "warning",
        p(sprintf("Upside: +%.1f%%", modelo$upside * 100))
      ),
      value_box(
        title = "DL/EBITDA",
        value = sprintf("%.1fx", financeiros$dl_ebitda[6]),
        showcase = bs_icon("shield-check"),
        theme = "success",
        p("Meta: 3.0–3.5x até 2026")
      )
    ),

    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),

      value_box(
        title = "Lucro Líquido 2025E",
        value = sprintf("R$ %.1f bi", financeiros$lucro_liquido[6]),
        showcase = bs_icon("piggy-bank"),
        theme = "primary"
      ),
      value_box(
        title = "Dívida Líquida 2025E",
        value = sprintf("R$ %.1f bi", financeiros$divida_liquida[6]),
        showcase = bs_icon("bank"),
        theme = "danger",
        p(sprintf("vs R$ %.1f bi em 2024E", financeiros$divida_liquida[5]))
      ),
      value_box(
        title = "WACC",
        value = sprintf("%.2f%%", modelo$wacc * 100),
        showcase = bs_icon("percent"),
        theme = "info"
      ),
      value_box(
        title = "TIR Implícita",
        value = sprintf("%.2f%%", modelo$tir * 100),
        showcase = bs_icon("arrow-up-right-circle"),
        theme = "success",
        p(sprintf("Spread: +%.2f p.p.", (modelo$tir - modelo$wacc) * 100))
      )
    ),

    layout_columns(
      col_widths = c(7, 5),

      card(
        card_header("Receita Líquida e EBITDA Ajustado (R$ bilhões)"),
        plotlyOutput("plot_receita_ebitda", height = "350px")
      ),
      card(
        card_header("Evolução das Margens (%)"),
        plotlyOutput("plot_margens", height = "350px")
      )
    ),

    card(
      card_header("Dados Financeiros Históricos (R$ bilhões)"),
      DTOutput("tabela_financeiros")
    )
  ),

  # ── Aba 2: Valuation (WACC & TIR) ────────────────────────────────────────

  nav_panel(
    title = "Valuation",
    icon  = bs_icon("calculator"),

    layout_columns(
      col_widths = c(4, 8),

      # Painel de controles (sliders)
      card(
        card_header(
          tags$span(bs_icon("sliders"), " Premissas do Modelo",
                    style = "font-weight: bold;")
        ),
        sliderInput("beta_input", "Beta",
                    min = 0.50, max = 1.50, value = round(modelo$beta, 2),
                    step = 0.05),
        sliderInput("selic_input", "Selic / Rf (% a.a.)",
                    min = 8, max = 18, value = round(modelo$selic * 100, 2),
                    step = 0.25),
        sliderInput("erp_input", "Prêmio de Risco ERP (%)",
                    min = 4, max = 10, value = round(modelo$premio_risco * 100, 1),
                    step = 0.5),
        sliderInput("crp_input", "Country Risk Premium (%)",
                    min = 1, max = 5, value = round(modelo$crp * 100, 1),
                    step = 0.25),
        sliderInput("de_input", "D/(D+E) — Estrutura de Capital (%)",
                    min = 30, max = 60, value = round(modelo$de_ratio * 100, 0),
                    step = 1),
        sliderInput("tax_input", "Alíquota IR (%)",
                    min = 25, max = 40, value = round(modelo$tax_rate * 100, 0),
                    step = 1),
        sliderInput("g_input", "Taxa de Crescimento g (%)",
                    min = 2, max = 5, value = round(modelo$g_perpetuidade * 100, 1),
                    step = 0.25),

        hr(),
        tags$p(tags$small(
          "Kd pré-IR fixo em 7.8% (custo médio da dívida Klabin).",
          style = paste0("color:", cores$text_muted, ";")
        )),
        actionButton("btn_reset", "Restaurar Padrões",
                     class = "btn-outline-warning btn-sm w-100")
      ),

      # Resultados do modelo
      layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,

        value_box(
          title = "Ke (CAPM)",
          value = textOutput("ke_output"),
          showcase = bs_icon("graph-up"),
          theme = "primary"
        ),
        value_box(
          title = "WACC",
          value = textOutput("wacc_output"),
          showcase = bs_icon("bullseye"),
          theme = "info"
        ),
        value_box(
          title = "TIR Implícita",
          value = textOutput("tir_output"),
          showcase = bs_icon("arrow-up-right-circle"),
          theme = "success"
        )
      ),

      layout_columns(
        col_widths = c(4, 4, 4),
        fill = FALSE,

        value_box(
          title = "Preço-Alvo (FCD)",
          value = textOutput("preco_alvo_output"),
          showcase = bs_icon("currency-dollar"),
          theme = "warning"
        ),
        value_box(
          title = "Upside / Downside",
          value = textOutput("upside_output"),
          showcase = bs_icon("arrow-left-right"),
          theme = "danger"
        ),
        value_box(
          title = "Spread TIR - WACC",
          value = textOutput("spread_output"),
          showcase = bs_icon("lightning"),
          theme = "success"
        )
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Sensibilidade WACC — Beta × Selic"),
          DTOutput("tabela_sensibilidade_wacc")
        ),
        card(
          card_header("Fluxos de Caixa Projetados"),
          plotlyOutput("plot_fcff", height = "300px")
        )
      )
    )
  ),

  # ── Aba 3: Visualizações ─────────────────────────────────────────────────

  nav_panel(
    title = "Visualizações",
    icon  = bs_icon("bar-chart-line"),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Receita Líquida e EBITDA — Histórico + Projeções"),
        plotlyOutput("viz_receita_ebitda", height = "350px")
      ),
      card(
        card_header("Evolução do CAPEX"),
        plotlyOutput("viz_capex", height = "350px")
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Fluxo de Caixa Livre (pré-CAPEX)"),
        plotlyOutput("viz_fcf", height = "350px")
      ),
      card(
        card_header("Alavancagem — DL/EBITDA"),
        plotlyOutput("viz_alavancagem", height = "350px")
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Comparação com Pares — EV/EBITDA e P/L"),
        plotlyOutput("viz_pares", height = "350px")
      ),
      card(
        card_header("Composição do Preço-Alvo"),
        plotlyOutput("viz_preco_alvo", height = "350px")
      )
    )
  ),

  # ── Footer ───────────────────────────────────────────────────────────────

  nav_spacer(),
  nav_item(
    tags$a(
      bs_icon("github"), " Projeto Klabin",
      style = paste0("color:", cores$text_muted, "; text-decoration: none;")
    )
  )
)

# ── 4. Server ────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Restaurar padrões ──────────────────────────────────────────────────
  observeEvent(input$btn_reset, {
    updateSliderInput(session, "beta_input", value = round(modelo$beta, 2))
    updateSliderInput(session, "selic_input", value = round(modelo$selic * 100, 2))
    updateSliderInput(session, "erp_input", value = round(modelo$premio_risco * 100, 1))
    updateSliderInput(session, "crp_input", value = round(modelo$crp * 100, 1))
    updateSliderInput(session, "de_input", value = round(modelo$de_ratio * 100, 0))
    updateSliderInput(session, "tax_input", value = round(modelo$tax_rate * 100, 0))
    updateSliderInput(session, "g_input", value = round(modelo$g_perpetuidade * 100, 1))
  })

  # ── Modelo reativo (recalcula a cada mudança de slider) ────────────────

  modelo_reativo <- reactive({
    # Inputs
    beta_r   <- input$beta_input
    rf_r     <- input$selic_input / 100
    erp_r    <- input$erp_input / 100
    crp_r    <- input$crp_input / 100
    de_r     <- input$de_input / 100
    ed_r     <- 1 - de_r
    tax_r    <- input$tax_input / 100
    g_r      <- input$g_input / 100
    kd_pre_r <- 0.078  # Fixo

    # CAPM
    ke_r <- rf_r + beta_r * erp_r + crp_r

    # Kd pós-IR
    kd_post_r <- kd_pre_r * (1 - tax_r)

    # WACC
    wacc_r <- ed_r * ke_r + de_r * kd_post_r

    # Recalcular projeções com novo WACC e g
    receita_base <- 24.5
    taxas_cresc <- c(0.050, 0.048, 0.045, 0.043, 0.040, 0.038, 0.037, 0.036, 0.035)
    margens <- c(0.345, 0.343, 0.340, 0.340, 0.340, 0.338, 0.335, 0.335, 0.335)
    capex_pct <- c(0.130, 0.135, 0.135, 0.140, 0.140, 0.140, 0.140, 0.140, 0.140)

    receita <- numeric(9)
    receita[1] <- receita_base * (1 + taxas_cresc[1])
    for (i in 2:9) receita[i] <- receita[i - 1] * (1 + taxas_cresc[i])

    ebitda <- receita * margens
    capex <- -receita * capex_pct
    da <- abs(capex) * 0.60
    ebit <- ebitda - da
    delta_wc <- c(-(receita[1] - receita_base), diff(-receita)) * 0.01
    delta_wc <- -(c(receita[1] - receita_base, diff(receita))) * 0.01
    fcff <- ebit * (1 - tax_r) + da + capex + delta_wc

    # Valor Terminal
    if (wacc_r <= g_r) {
      # Evitar divisão por zero ou negativa
      ev_calc <- sum(fcff / (1 + wacc_r)^(1:9)) * 10
    } else {
      vt <- fcff[9] * (1 + g_r) / (wacc_r - g_r)
      vt_desc <- vt / (1 + wacc_r)^9
      pv_fcff <- sum(fcff / (1 + wacc_r)^(1:9))
      ev_calc <- pv_fcff + vt_desc
    }

    # Equity Value e Preço-Alvo
    equity_v <- ev_calc - 28.8 - 2.3
    n_units <- 27.5 / 20.80
    preco_alvo_r <- equity_v / n_units

    # TIR
    ev_mercado <- params$preco_atual * n_units + 28.8 + 2.3

    npv_func <- function(r) {
      if (r <= g_r) return(1e10)
      vt_tir <- fcff[9] * (1 + g_r) / (r - g_r)
      sum(fcff / (1 + r)^(1:9)) + vt_tir / (1 + r)^9 - ev_mercado
    }

    tir_r <- tryCatch({
      uniroot(npv_func, interval = c(0.04, 0.50), tol = 1e-8)$root
    }, error = function(e) NA_real_)

    # Upside
    upside_r <- (preco_alvo_r / params$preco_atual - 1)

    list(
      ke = ke_r, kd_post = kd_post_r, wacc = wacc_r,
      tir = tir_r, ev = ev_calc, equity = equity_v,
      preco_alvo = preco_alvo_r, upside = upside_r,
      spread = ifelse(is.na(tir_r), NA, tir_r - wacc_r),
      fcff = fcff, receita = receita, ebitda = ebitda
    )
  })

  # ── Outputs de texto (value boxes do Valuation) ────────────────────────

  output$ke_output <- renderText({
    sprintf("%.2f%%", modelo_reativo()$ke * 100)
  })
  output$wacc_output <- renderText({
    sprintf("%.2f%%", modelo_reativo()$wacc * 100)
  })
  output$tir_output <- renderText({
    tir <- modelo_reativo()$tir
    if (is.na(tir)) "N/D" else sprintf("%.2f%%", tir * 100)
  })
  output$preco_alvo_output <- renderText({
    sprintf("R$ %.2f", modelo_reativo()$preco_alvo)
  })
  output$upside_output <- renderText({
    up <- modelo_reativo()$upside * 100
    sprintf("%s%.1f%%", ifelse(up >= 0, "+", ""), up)
  })
  output$spread_output <- renderText({
    sp <- modelo_reativo()$spread
    if (is.na(sp)) "N/D" else sprintf("%.2f p.p.", sp * 100)
  })

  # ── Gráficos do Painel Geral ──────────────────────────────────────────

  output$plot_receita_ebitda <- renderPlotly({
    plot_ly(financeiros, x = ~ano) |>
      add_bars(y = ~receita_liquida, name = "Receita",
               marker = list(color = cores$blue), width = 0.35,
               offset = -0.18) |>
      add_bars(y = ~ebitda_ajustado, name = "EBITDA",
               marker = list(color = cores$gold), width = 0.35,
               offset = 0.18) |>
      plotly_layout_dark(yaxis_title = "R$ bilhões") |>
      config(displayModeBar = FALSE)
  })

  output$plot_margens <- renderPlotly({
    plot_ly(financeiros, x = ~ano) |>
      add_lines(y = ~margem_ebitda, name = "Margem EBITDA (%)",
                line = list(color = cores$teal, width = 3)) |>
      add_markers(y = ~margem_ebitda, name = "",
                  marker = list(color = cores$teal, size = 8),
                  showlegend = FALSE) |>
      plotly_layout_dark(yaxis_title = "%") |>
      config(displayModeBar = FALSE)
  })

  output$tabela_financeiros <- renderDT({
    fin_fmt <- financeiros |>
      mutate(
        across(c(receita_liquida, ebitda_ajustado, lucro_liquido,
                 divida_liquida, fcf_pre_capex),
               ~sprintf("%.1f", .)),
        capex = sprintf("(%.1f)", abs(capex)),
        margem_ebitda = sprintf("%.1f%%", margem_ebitda),
        dl_ebitda = sprintf("%.1fx", dl_ebitda)
      )

    datatable(
      fin_fmt,
      colnames = c("Ano", "Tipo", "Receita", "EBITDA", "Margem",
                    "Lucro Liq.", "CAPEX", "Dív. Líq.", "DL/EBITDA", "FCF"),
      options = list(
        dom = "t",
        pageLength = 10,
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE,
      class = "compact stripe"
    ) |>
      formatStyle(columns = 1:10,
                  backgroundColor = cores$bg_card,
                  color = cores$text_primary)
  })

  # ── Gráficos da aba Valuation ─────────────────────────────────────────

  output$tabela_sensibilidade_wacc <- renderDT({
    # Gerar tabela de sensibilidade Beta × Selic
    betas <- seq(0.60, 1.20, by = 0.15)
    selics <- seq(10, 16, by = 1.5)

    sens <- expand.grid(Beta = betas, Selic = selics) |>
      mutate(
        Ke = Selic / 100 + Beta * (input$erp_input / 100) + (input$crp_input / 100),
        Kd_post = 0.078 * (1 - input$tax_input / 100),
        WACC = (1 - input$de_input / 100) * Ke + (input$de_input / 100) * Kd_post,
        WACC_fmt = sprintf("%.1f%%", WACC * 100)
      ) |>
      select(Beta, Selic, WACC_fmt) |>
      pivot_wider(names_from = Selic, values_from = WACC_fmt,
                  names_prefix = "Selic ")

    datatable(
      sens,
      options = list(dom = "t", pageLength = 10, ordering = FALSE,
                     columnDefs = list(
                       list(className = "dt-center", targets = "_all")
                     )),
      rownames = FALSE,
      class = "compact stripe"
    ) |>
      formatStyle(columns = 1:ncol(sens),
                  backgroundColor = cores$bg_card,
                  color = cores$text_primary)
  })

  output$plot_fcff <- renderPlotly({
    m <- modelo_reativo()
    df_fcff <- tibble(
      ano = 2026:2034,
      fcff = m$fcff
    )

    plot_ly(df_fcff, x = ~ano, y = ~fcff, type = "bar",
            marker = list(color = cores$teal)) |>
      plotly_layout_dark(
        title = "FCFF Projetado (R$ bi)",
        yaxis_title = "R$ bilhões"
      ) |>
      config(displayModeBar = FALSE)
  })

  # ── Gráficos da aba Visualizações ─────────────────────────────────────

  # Dados combinados (histórico + projeção)
  dados_combinados <- reactive({
    fluxos_ext <- fluxos |>
      mutate(tipo = "Projeção") |>
      select(ano, receita, ebitda, margem_ebitda, capex, tipo)

    hist_ext <- financeiros |>
      mutate(tipo = ifelse(ano <= 2023, "Real", "Estimado")) |>
      select(ano, receita_liquida, ebitda_ajustado, margem_ebitda, capex, tipo) |>
      rename(receita = receita_liquida, ebitda = ebitda_ajustado) |>
      mutate(margem_ebitda = margem_ebitda / 100)

    bind_rows(hist_ext, fluxos_ext)
  })

  output$viz_receita_ebitda <- renderPlotly({
    df <- dados_combinados()

    plot_ly(df, x = ~ano) |>
      add_bars(y = ~receita, name = "Receita",
               marker = list(color = cores$blue), width = 0.35,
               offset = -0.18) |>
      add_bars(y = ~ebitda, name = "EBITDA",
               marker = list(color = cores$gold), width = 0.35,
               offset = 0.18) |>
      add_lines(y = ~receita, name = "", showlegend = FALSE,
                line = list(color = cores$blue, width = 1, dash = "dot")) |>
      plotly_layout_dark(yaxis_title = "R$ bilhões") |>
      layout(shapes = list(
        list(type = "line", x0 = 2025.5, x1 = 2025.5,
             y0 = 0, y1 = max(df$receita) * 1.1,
             line = list(color = cores$text_muted, dash = "dash", width = 1))
      )) |>
      config(displayModeBar = FALSE)
  })

  output$viz_capex <- renderPlotly({
    plot_ly(financeiros, x = ~ano, y = ~abs(capex), type = "bar",
            marker = list(color = cores$red),
            name = "CAPEX") |>
      plotly_layout_dark(yaxis_title = "R$ bilhões") |>
      config(displayModeBar = FALSE)
  })

  output$viz_fcf <- renderPlotly({
    plot_ly(financeiros, x = ~ano, y = ~fcf_pre_capex, type = "bar",
            marker = list(color = cores$teal, 
                          line = list(color = cores$teal, width = 1)),
            name = "FCF pré-CAPEX") |>
      plotly_layout_dark(yaxis_title = "R$ bilhões") |>
      config(displayModeBar = FALSE)
  })

  output$viz_alavancagem <- renderPlotly({
    plot_ly(financeiros, x = ~ano) |>
      add_bars(y = ~divida_liquida, name = "Dívida Líquida",
               marker = list(color = cores$red, opacity = 0.6)) |>
      add_lines(y = ~dl_ebitda * max(financeiros$divida_liquida) / max(financeiros$dl_ebitda),
                name = "DL/EBITDA (eixo dir.)",
                yaxis = "y2",
                line = list(color = cores$amber, width = 3)) |>
      add_markers(y = ~dl_ebitda * max(financeiros$divida_liquida) / max(financeiros$dl_ebitda),
                  name = "", showlegend = FALSE, yaxis = "y2",
                  marker = list(color = cores$amber, size = 8)) |>
      plotly_layout_dark(yaxis_title = "R$ bilhões") |>
      layout(
        yaxis2 = list(
          title = "DL/EBITDA (x)",
          overlaying = "y",
          side = "right",
          color = cores$amber,
          gridcolor = "rgba(0,0,0,0)"
        )
      ) |>
      config(displayModeBar = FALSE)
  })

  output$viz_pares <- renderPlotly({
    pares_long <- pares |>
      select(empresa, ev_ebitda, pl) |>
      pivot_longer(-empresa, names_to = "multiplo", values_to = "valor") |>
      mutate(multiplo = ifelse(multiplo == "ev_ebitda", "EV/EBITDA", "P/L"))

    plot_ly(pares_long, x = ~empresa, y = ~valor, color = ~multiplo,
            type = "bar",
            colors = c("EV/EBITDA" = cores$blue, "P/L" = cores$gold)) |>
      plotly_layout_dark(yaxis_title = "Múltiplo (x)") |>
      layout(barmode = "group") |>
      config(displayModeBar = FALSE)
  })

  output$viz_preco_alvo <- renderPlotly({
    preco_data <- tibble(
      componente = c("FCD (60%)", "Múltiplos (40%)", "Preço-Alvo"),
      valor = c(24.80, 24.55, 24.50),
      cor = c(cores$blue, cores$gold, cores$teal)
    )

    plot_ly(preco_data, x = ~componente, y = ~valor, type = "bar",
            marker = list(color = c(cores$blue, cores$gold, cores$teal)),
            text = ~sprintf("R$ %.2f", valor),
            textposition = "outside") |>
      plotly_layout_dark(yaxis_title = "R$/unit") |>
      layout(yaxis = list(range = c(0, 28))) |>
      config(displayModeBar = FALSE)
  })
}

# ── 5. Executar App ─────────────────────────────────────────────────────────

shinyApp(ui = ui, server = server)


###############################################################################
# ── GUIA DE PUBLICAÇÃO NO GITHUB PAGES VIA SHINYLIVE ──────────────────────
#
# Siga os passos abaixo para compilar e publicar este dashboard
# gratuitamente no GitHub Pages usando o Shinylive (WebAssembly).
#
# 1. INSTALAR PACOTES NECESSÁRIOS:
#    install.packages("shinylive")
#    install.packages("httpuv")
#
# 2. PREPARAR A PASTA DE EXPORTAÇÃO:
#    Você precisa consolidar todo o código e os dados numa pasta "app_dir".
#    O arquivo principal deve se chamar app.R.
#    Crie a estrutura:
#       app_dir/
#       ├── app.R (Este script, dashKlabin.R renomeado)
#       └── data/ (Copie a pasta data inteira para cá)
#
# 3. COMPILAR COM SHINYLIVE:
#    Rode o seguinte comando para compilar o app em WebAssembly para
#    a pasta "docs":
#
#    shinylive::export(appdir = "app_dir", destdir = "docs")
#
# 4. TESTAR LOCALMENTE:
#    Não abra os arquivos HTML diretamente. Para testar:
#
#    httpuv::runStaticServer("docs")
#
# 5. DEPLOY NO GITHUB PAGES:
#    - Adicione a pasta docs/ gerada no seu repositório Git.
#    - Faça commit e push para o GitHub.
#    - Vá no seu repositório no GitHub -> Settings -> Pages.
#    - Em "Source", selecione "Deploy from a branch".
#    - Selecione sua branch (ex: main) e a pasta /docs.
#    - Clique em Save.
#
# Pronto! O app estará rodando 100% no lado cliente de forma gratuita!
###############################################################################
#    - Para atualizar, basta rodar deployApp() novamente.
###############################################################################

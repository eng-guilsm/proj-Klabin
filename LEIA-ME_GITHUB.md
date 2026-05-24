# Publicação do Dashboard no GitHub Pages via Shinylive

O **Shinylive** permite compilar o aplicativo R Shiny para WebAssembly (wasm), fazendo com que ele rode diretamente no navegador do usuário (client-side). Isso dispensa a necessidade de um servidor R rodando ativamente e possibilita a hospedagem 100% gratuita no **GitHub Pages**.

Siga os passos abaixo para preparar seu repositório:

## Passos no RStudio (Local)

1. Instale os pacotes necessários:
   ```R
   install.packages("shinylive")
   install.packages("httpuv")
   ```

2. Crie uma pasta temporária chamada `app_dir/` e coloque o código fonte do dashboard e os dados:
   - Copie `dashKlabin.R` para `app_dir/` e renomeie-o para `app.R`.
   - Copie a pasta `data/` com todos os `.rds` e `.csv` para dentro de `app_dir/`.

3. Exporte para a pasta `docs/` executando no R:
   ```R
   shinylive::export(appdir = "app_dir", destdir = "docs")
   ```

## Passos no GitHub

1. Certifique-se de que a pasta `docs/` gerada pelo Shinylive no passo anterior foi adicionada ao seu commit e envie para o seu repositório remoto no GitHub.

2. Acesse o seu repositório no GitHub.

3. Vá até a aba **Settings** (Configurações).

4. Na barra lateral esquerda, desça até a seção "Code and automation" e clique em **Pages**.

5. Em **Build and deployment**:
   - Em "Source", selecione **Deploy from a branch**.
   - Na seção "Branch", selecione a sua branch principal (ex: `main` ou `master`).
   - No dropdown ao lado da branch (que por padrão diz `/ (root)`), altere para **`/docs`**.
   - Clique em **Save**.

6. Aguarde alguns minutos. No topo da página do GitHub Pages, aparecerá uma notificação verde dizendo: *Your site is live at https://[seu_usuario].github.io/[seu_repositorio]/*.

Seu dashboard interativo com teste de estresse de Valuation está agora publicado e disponível para o mundo gratuitamente, rodando pelo navegador!

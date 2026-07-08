# masters thesis spare-parts forecasting

Comparative study of intermittent demand forecasting methods for air traffic control (ATC) spare parts. Implements 15+ forecasting techniques with empirical validation on ATC systems data. Master's thesis research in logistics and supply chain management.

# Dissertação em Previsão de Demanda de Sobressalentes

**Dissertação de Mestrado em Logística**

Estudo comparativo de métodos de previsão de demanda intermitente para peças de reposição de controle de tráfego aéreo (ATC). Implementa mais de 15 técnicas de previsão com validação empírica em dados de sistemas ATC. Pesquisa de dissertação de mestrado em logística e gestão da cadeia de suprimentos.

## 👤 Autor

**Luiz Antonio Rezende**\
Mestrando em Logística\
PUC-Rio

## 🎯 Objetivos

### Objetivo Geral

Avaliar comparativamente o desempenho de diferentes métodos de previsão de demanda aplicados a sobressalentes e consumíveis do SISCEAB, visando identificar abordagens que otimizem a disponibilidade operacional dos sistemas críticos de controle de tráfego aéreo.

### Objetivos Específicos

-   Caracterizar o padrão de demanda histórica segundo taxonomia SBC
-   Implementar e parametrizar 15+ métodos de previsão
-   Estabelecer métricas apropriadas para demanda intermitente
-   Conduzir análise comparativa com validação out-of-sample
-   Propor recomendações metodológicas para o DECEA

## 📋 TO-DO LIST! 🟡

-   NIL

## ▶️Script Pipeline

00_setup_project.R

-   Automatiza a inicialização do repositório do projeto.

01_data_preparation.R

-   Carrega os dados brutos necessários e faz o tratamento inicial de verificação da base conforme metodologia estabelecida no trabalho.

02_train_test_split.R

-   Implementa estratégia de validação rolling origin com janela \# expansiva, classificação SBC por origem e validações de integridade

03_exploratory_analysis.R

-   Caracterização dos padrões de demanda segundo taxonomia SBC, análise descritiva por categoria, subsistema e temporal

04_run_all_forecasts.R

-   Executa sequencialmente os três pipelines de forecasting

    -   04a_baseline_models_forecast.R

        -   Implementação de métodos clássicos e de suavização exponencial \# para previsão de demanda intermitente (Famílias 1 e 2)

    -   04b_Intermittent_Demand_Models_Forecast.R

        -   Implementação de métodos especializados para demanda intermitente \# com otimização de hiperparâmetros (Família 3)

    -   04c_probabilistic_ADIDA_forecast.R

        -   Implementa métodos probabilísticos (Poisson, Gamma) e agregação temporal (ADIDA)

    -   04d_annual_forecasts.R

        -   Agrega os dados em observações anuais e implementa a previsão com através dos métodos estudados.

05_consolidate_results.R

-   Consolida forecasts das 3 famílias, calcula métricas de erro e prepara dados para análise comparativa

06_analyze_results.R

-   Análise estatística comparativa dos métodos de previsão com testes de significância e identificação de domínios de superioridade

07a_advanced_error-MAE_visualization.R

07b_multi_metric_analysis.R

08_forecast_visualization.R

## 📊 Estrutura do Projeto

```         
.
├── data/
│   ├── raw/              # Dados SILOMS (CONFIDENCIAIS - não versionados)
│   ├── processed/        # Dados limpos e consolidados
│   ├── interim/          # Dados intermediários
│   └── external/         # Dados externos complementares
├── R/
│   ├── functions/        # Funções customizadas
│   ├── analysis/         # Scripts de análise exploratória
│   ├── modeling/         # Scripts de modelagem
│   ├── validation/       # Scripts de validação
│   └── utils/            # Funções utilitárias
├── scripts/              # Scripts principais (workflow)
├── output/
│   ├── figures/          # Gráficos e visualizações
│   ├── tables/           # Tabelas de resultados
│   ├── models/           # Modelos salvos (.rds)
│   └── reports/          # Relatórios finais
├── docs/                 # Documentação adicional
├── config/               # Arquivos de configuração
│   └── config.yaml       # Configurações do projeto
└── logs/                 # Logs de execução
```

## 🗃️ Dados

-   Colocar dados do SILOMS em `data/raw/`
-   Arquivos não serão versionados (protegidos pelo .gitignore)

## 🔬 Métodos de Previsão

### Família 1: Métodos Clássicos (Benchmarks)

-   Naive
-   Média Simples
-   Média Móvel (k=36 meses)

### Família 2: Suavização Exponencial e Séries Temporais

-   ARIMA (AutoRegressive Integrated Moving Average)
-   ETS (Error, Trend, Seasonal)
-   Holt-Winters Aditivo e Multiplicativo
-   TSLM (Time Series Linear Model)

### Família 3: Métodos Especializados (Demanda Intermitente)

-   **Croston Clássico** (1972)
-   **SBA** - Syntetos-Boylan Approximation (2005)
-   **TSB** - Teunter-Syntetos-Babai (2011)

### Família 4: Métodos Probabilísticos

-   **Distribuição de Poisson** (método atual do DECEA)
-   **Distribuição Gama**

### Família 5: Agregação Temporal

-   **ADIDA** - Aggregate-Disaggregate Intermittent Demand Approach

## 📏 Métricas de Avaliação

-   **MAE** (Mean Absolute Error) - métrica primária
-   **RMSE** (Root Mean Squared Error)
-   **Bias** (Mean Error)
-   **LinLin** (Função de Perda Assimétrica, p=0.85)
-   **MAD/Mean Ratio**

**Perspectivas de avaliação:**

1.  **Mensal**: acurácia mês a mês (12 previsões)

2.  **Anual agregada**: demanda total de 12 meses (planejamento orçamentário)

3.  **Anual nativa**: demanda de um ano prevista pela série nativa anual.

## 🔄 Estratégia de Validação

-   **Método**: Fixed Origin com janela expansiva
-   **Origens**: 6 pontos temporais
-   **Horizonte**: 12 meses (h=12)
-   **Treino mínimo**: 36 meses

## ⚠️ Confidencialidade

**IMPORTANTE**: Este projeto contém dados operacionais confidenciais do DECEA/SISCEAB.

-   Dados em `data/raw/` e `data/interim/` **NÃO** são versionados
-   Arquivos `.csv`, `.xlsx`, `.rds` com dados reais estão no `.gitignore`
-   Apenas código metodológico e estrutura são compartilhados no Git

## 📚 Referências Principais

-   **BOYLAN, J. E.; SYNTETOS, A. A.** Intermittent Demand Forecasting: Context, Methods and Applications. Wiley, 2021.

-   **SYNTETOS, A. A.; BOYLAN, J. E.; CROSTON, J. D.** On the categorization of demand patterns. Journal of the Operational Research Society, v. 56, n. 5, p. 495-503, 2005.

-   **PETROPOULOS, F. et al.** Forecasting: theory and practice. International Journal of Forecasting, v. 38, n. 3, p. 705-871, 2022.

-   **TEUNTER, R. H.; SYNTETOS, A. A.; BABAI, M. Z.** Intermittent demand: Linking forecasting to inventory obsolescence. European Journal of Operational Research, v. 214, n. 3, p. 606-615, 2011.

## 📧 Contato

santosdias@protonmail.com

------------------------------------------------------------------------

**Status**: ⚡🚧 Em análise\
**Última atualização**: 2026-01-28\
**Versão**: 2.1.3

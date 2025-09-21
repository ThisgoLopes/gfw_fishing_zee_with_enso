# Avaliação da Atividade de Pesca na ZEE com APIs do Global Fishing Watch

![R Version](https://img.shields.io/badge/R-%3E%3D4.2.0-blue)
![Status](https://img.shields.io/badge/status-em%20desenvolvimento-yellow)
![Contributions](https://img.shields.io/badge/contributions-bem%20vindas-orange)

---

## 📌 Sobre o Projeto
Este projeto tem como objetivo **analisar e mapear a atividade de pesca na Zona Econômica Exclusiva (ZEE) brasileira** utilizando as **APIs do [Global Fishing Watch](https://globalfishingwatch.org/)**.  

O estudo faz parte de uma linha de pesquisa aplicada à Engenharia de Pesca e ao monitoramento de recursos pesqueiros, permitindo:  
- Avaliar **padrões espaço-temporais da atividade de pesca**.  
- Relacionar a intensidade de esforço pesqueiro a variáveis ambientais (ex.: ENSO).  
- Produzir mapas e análises estatísticas para subsidiar a **gestão pesqueira sustentável**.  

Arquivos mantidos neste repositório:

- **`enso.r`** → Script para manipulação dos dados do **ENSO** (El Niño / La Niña), capturado no site oficial da NOAA, preparando informações ambientais para análise conjunta com a pesca.  
- **`install.r`** → Script para instalar e carregar automaticamente os pacotes necessários no R.  
- **`analise_gfw_enso_4wings.r`** → Script principal de análise, cruzando dados do Global Fishing Watch com variabilidade climática (ENSO).  
- **`eventos_pesca_zee_oni.r`** → Processa e organiza os eventos de pesca na ZEE brasileira, estruturando os dados para análise temporal e espacial.  
---

## ⚙️ Tecnologias Utilizadas
- **Linguagem**: R  
- **Principais pacotes**:  
  - `gfwr` (client R para a API do Global Fishing Watch)  
  - `dplyr`, `tidyr`, `ggplot2` (tratamento e visualização de dados)  
  - `sf` (manipulação espacial)  
  - `rnaturalearth` (mapas de referência)  

---

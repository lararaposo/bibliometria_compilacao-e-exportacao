# Script completo para análise bibliométrica no R com Bibliometrix
[![DOI](https://zenodo.org/badge/1335457929.svg)]( )
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%E2%89%A5%204.1-blue.svg)](https://www.r-project.org/)

Script em R para análise bibliométrica (compilação de bases e exportação automática) com o pacote [bibliometrix](https://github.com/massimoaria/bibliometrix).
---

## Conteúdo do repositório

| Arquivo | Função |
| --- | --- |
| `script_com_exportacao.R` | Compila bases da Web of Science e da Scopus, harmoniza referências citadas, deduplica documentos, consolida metadados e executa um fluxo amplo de análise bibliométrica com exportação automática dos resultados |

---

## `script_com_exportacao.R`

Executa um fluxo completo de análise bibliométrica no R com o `bibliometrix`, desde a compilação dos arquivos brutos da **Web of Science** e da **Scopus** até a geração automática de tabelas, redes, mapas temáticos, análise de cocitação, RPYS, historiograph, Three-Field Plot e outras saídas.

O script foi desenvolvido para reduzir a necessidade de edição manual de código. A execução é guiada por janelas no RStudio e pode ser utilizada tanto por quem já possui um corpus compilado quanto por quem ainda precisa integrar os arquivos brutos da Scopus e da Web of Science.

### Por que este script

Combinar Scopus e Web of Science não consiste apenas em concatenar registros e remover documentos duplicados.

As duas bases frequentemente apresentam informações complementares sobre o mesmo documento, incluindo *abstract*, *author keywords*, afiliações e referências citadas. Além disso, uma mesma referência bibliográfica pode aparecer em formatos diferentes nas duas bases.

A versão atual do `bibliometrix` reconhece esse problema e evita utilizar diretamente o campo `CR` de coleções provenientes de múltiplas bases sem uma etapa adicional de harmonização.

Este script implementa essa etapa explicitamente.

O fluxo utilizado é:

```text
importação
→ harmonização das referências
→ identificação de documentos duplicados
→ consolidação de metadados
→ união das referências
→ análise bibliométrica
```

Assim, as informações provenientes da Scopus e da Web of Science são utilizadas de forma complementar.

---

## Harmonização das referências citadas

Um dos principais diferenciais deste script é o tratamento do campo `CR`, que contém as referências citadas por cada documento.

A mesma referência pode aparecer de formas diferentes na WoS e na Scopus. Em vez de simplesmente concatenar essas strings, o script utiliza o mecanismo de **Reference Matching** do `bibliometrix`.

O matching considera múltiplos critérios, incluindo:

1. DOI.
2. Normalização textual.
3. Correspondência exata após normalização.
4. Padronização de nomes de periódicos.
5. Similaridade fuzzy.
6. Pós-processamento dos metadados da referência.

O threshold padrão de similaridade é `0.90`.

Esse parâmetro permanece fixado internamente para garantir consistência entre execuções.

Após a harmonização, referências reconhecidas como o mesmo trabalho passam a compartilhar uma representação canônica.

Exemplo conceitual:

```text
Web of Science:
A
B
C
D
E

Scopus:
A
B
C
D
F
G
```

Depois da harmonização e consolidação do documento:

```text
A
B
C
D
E
F
G
```

As referências exclusivas de cada base são preservadas, enquanto referências equivalentes não são contadas duas vezes.

Isso é especialmente importante para análises como cocitação, RPYS, historiograph e citações locais.

---

## Deduplicação dos documentos

Os documentos são agrupados por equivalência de DOI e título + ano normalizados.

A lógica usa componentes conexos, permitindo resolver encadeamentos como:

```text
A = B por DOI
B = C por título + ano
```

Nesse caso, A, B e C são tratados como um único grupo documental.

Para cada grupo, o script seleciona um registro-base e complementa seus campos com informações disponíveis nos demais registros.

No campo `CR`, entretanto, a lógica é diferente: as referências harmonizadas de todos os registros do grupo são combinadas e deduplicadas.

Assim, referências exclusivas de uma das bases não são descartadas.

---

## Arquivos de auditoria do Reference Matching

Para tornar a harmonização rastreável, o script gera arquivos específicos de auditoria:

| Arquivo | Conteúdo |
| --- | --- |
| `reference_matching_summary.csv` | Resumo das referências canônicas identificadas |
| `reference_matching_variants.csv` | Relação entre variantes encontradas e referências harmonizadas |
| `reference_matching_diagnostics.csv` | Diagnóstico geral do processo de matching |

Esses arquivos permitem verificar como as referências provenientes das duas bases foram reconciliadas.

---

## Requisitos

- R ≥ 4.1
- Pacotes: `bibliometrix`, `dplyr`, `tidyr`, `writexl`, `ggplot2`, `htmlwidgets`, `ineq`, `rstudioapi`

O bloco inicial instala automaticamente o que estiver faltando.

O uso do RStudio é recomendado, mas não obrigatório.

No RStudio, as escolhas de arquivos, pastas e parâmetros são feitas por caixas de diálogo nativas. Fora do RStudio, o script utiliza o console como alternativa.

O script não utiliza `tcltk` para as interfaces de seleção, evitando dependência direta de X11/XQuartz no macOS.

---

## Exportando os arquivos brutos

**Web of Science** → `Export` → `Plain text file` → *Record Content*: **Full Record and Cited References** → salvar como `.txt`.

Se a exportação precisar ser dividida em vários arquivos, todos podem ser colocados na mesma pasta.

**Scopus** → `Export` → `CSV` → marcar todos os grupos de campos relevantes, em especial *Abstract & keywords* e *References*.

Sem *References* marcado, o campo `CR` vem vazio e análises como cocitação, RPYS, historiograph e citações locais ficam comprometidas.

Coloque todos os `.txt` da WoS e `.csv` da Scopus em **uma única pasta**.

---

## Uso

Abra o script no RStudio e execute o arquivo inteiro:

```r
source("script_com_exportacao.R")
```

Ou utilize **Source** / **Run All** no RStudio.

No macOS, também é possível executar o arquivo inteiro com:

```text
Cmd + Shift + Enter
```

Não execute apenas a linha atual com `Run`, pois o script depende de funções definidas nas etapas iniciais.

---

## Configuração inicial

Antes das análises, o script solicita apenas quatro decisões metodológicas ao usuário.

### Período da análise

Padrão:

```text
2001–2025
```

O usuário informa o ano inicial e o ano final.

### Cortes da Thematic Evolution

Padrão:

```text
2005, 2010, 2015, 2020
```

Os anos definem os períodos usados na análise de evolução temática.

### Número de itens nos rankings

Padrão:

```text
20
```

Controla o número de itens exibidos nas principais tabelas e rankings.

### Itens por campo no Three-Field Plot

Padrão:

```text
10
```

Define quantos elementos serão apresentados em cada campo do Three-Field Plot.

Os demais parâmetros técnicos permanecem definidos internamente para manter consistência entre execuções.

Todos os parâmetros utilizados são registrados automaticamente no arquivo:

```text
00_parametros_execucao.csv
```

Isso permite documentar e reproduzir posteriormente as condições da análise.

---

## Escolha da origem do corpus

Depois da configuração inicial, o script pergunta se o usuário:

```text
1 = Já possui o arquivo final.rds
2 = Precisa compilar Scopus + WoS
```

### Opção 1: utilizar um corpus existente

O usuário seleciona o arquivo:

```text
final.rds
```

O script carrega o corpus previamente compilado e segue para as análises.

### Opção 2: compilar as bases

O usuário seleciona a pasta onde estão os arquivos brutos:

```text
Scopus → .csv
Web of Science → .txt
```

O script então:

```text
importa os arquivos
harmoniza as referências
identifica duplicados
consolida os metadados
gera o resumo PRISMA
salva o corpus final
```

São criados:

```text
final.rds
final.csv
```

O `final.rds` é o objeto principal para análises futuras no `bibliometrix` ou `biblioshiny`.

---

## Fluxo em duas rodadas para palavras-chave

Algumas análises dependem da harmonização de sinônimos das palavras-chave.

Por isso, o script foi desenhado para funcionar em duas rodadas.

### Primeira rodada

Quando o script perguntar pelo arquivo de sinônimos, cancele a seleção caso ainda não possua um.

O script executará todas as análises que não dependem dessa harmonização e gerará:

```text
keywords_frequencias.csv
PROMPT_para_gerar_sinonimos.txt
```

O primeiro arquivo contém a frequência dos termos encontrados no corpus.

O segundo contém um prompt que pode ser utilizado em uma IA generativa para auxiliar na identificação de sinônimos, variantes ortográficas, singular/plural e outras formas semanticamente equivalentes.

### Segunda rodada

Depois de produzir e revisar o arquivo:

```text
sinonimos.txt
```

execute o script novamente.

Desta vez:

```text
1. escolha "Já possuo o arquivo final.rds"
2. carregue o final.rds gerado anteriormente
3. selecione sinonimos.txt
```

O script executará então as análises dependentes da harmonização das palavras-chave.

---

## Estrutura das saídas

Todos os resultados analíticos são gravados em uma pasta de exportação criada pelo script.

O script também mantém um log de eventuais erros:

```text
_log_erros.txt
```

Cada bloco analítico é executado de forma independente. Se uma análise falhar, as demais continuam sendo executadas.

Isso é especialmente útil em corpora nos quais determinada informação não está disponível em quantidade suficiente para uma análise específica.

---

## Principais análises produzidas

O script gera automaticamente resultados de análise descritiva, produção científica, autores, periódicos, países, citações, redes e estrutura conceitual.

Entre as saídas estão:

1. Principais informações do corpus.
2. Produção científica anual.
3. Média de citações por ano.
4. Autores mais produtivos.
5. Produção dos autores ao longo do tempo.
6. Lei de Lotka.
7. Lei de Bradford.
8. Afiliações.
9. Países.
10. Documentos globalmente mais citados.
11. Citações locais.
12. Colaboração entre autores.
13. Colaboração entre países.
14. Rede de cocitação.
15. Historiograph.
16. Frequência de palavras-chave.
17. WordCloud.
18. Keyword Growth.
19. Trend Topics.
20. Análise por quinquênios.
21. Rede de coocorrência.
22. Estrutura conceitual.
23. Thematic Map.
24. Thematic Evolution.
25. RPYS.
26. Three-Field Plot.

Os formatos de saída incluem `.csv`, `.xlsx`, `.png` e `.html`, conforme o tipo de análise.

---

## RPYS

O script inclui **Reference Publication Year Spectroscopy — RPYS**.

A análise identifica anos em que ocorre concentração anormal de referências citadas, permitindo localizar trabalhos históricos potencialmente relevantes para a formação intelectual do campo.

Além da tabela completa, o script identifica anos-pico e tenta recuperar as referências associadas a esses anos.

Entre as saídas estão:

```text
blocoA2_rpys_tabela.csv
blocoA2_rpys_anos_pico.csv
blocoA2_referencias_seminais.csv
blocoA2_rpys.png
```

---

## Three-Field Plot

O script produz um Three-Field Plot relacionando:

```text
CR → AU → DE
```

ou seja:

```text
Cited References
→ Authors
→ Author Keywords
```

Isso permite observar conexões entre as bases intelectuais do campo, os autores do corpus e os temas investigados.

O resultado é salvo em HTML interativo.

---

## Thematic Map e Thematic Evolution

O script inclui análises de estrutura e evolução temática baseadas nas *author keywords*.

A **Thematic Map** posiciona os clusters segundo medidas de centralidade e densidade.

A **Thematic Evolution** acompanha como os temas se reorganizam ao longo dos períodos definidos pelo usuário na abertura do script.

Os cortes temporais são, portanto, uma decisão metodológica explícita e ficam registrados no arquivo de parâmetros.

---

## Resumo PRISMA

Quando o corpus é compilado a partir dos arquivos brutos, o script apresenta no console e salva um resumo contendo informações úteis para o fluxograma PRISMA, incluindo registros identificados nas duas bases, documentos removidos durante a deduplicação e tamanho do corpus final.

O resumo também inclui informações relativas à harmonização das referências.

Uma cópia é salva como:

```text
00_Resumo_PRISMA.txt
```

---

## `final.rds` e `final.csv`

Após a compilação, são gerados dois formatos do corpus.

### `final.rds`

Objeto R utilizado diretamente pelo script e compatível com o fluxo de análise do `bibliometrix`.

É o formato recomendado para reutilizar o corpus sem recompilar as bases.

### `final.csv`

Versão tabular do mesmo corpus, exportada em UTF-8 com BOM para facilitar a abertura no Excel, inclusive no macOS.

É útil para auditoria, inspeção dos metadados e análises externas.

---

## Reprodutibilidade

A cada execução, o script salva:

```text
00_parametros_execucao.csv
```

com os parâmetros metodológicos e técnicos utilizados.

Isso permite documentar a configuração exata que produziu determinada análise.

Além disso, a preservação dos arquivos de Reference Matching permite auditar o tratamento das referências entre WoS e Scopus.

---

## Notas metodológicas

**Sobre `CR` e `CR_raw`.** Em coleções provenientes de mais de uma base, simplesmente concatenar ou restaurar as referências originais da WoS e Scopus pode fazer com que a mesma referência seja tratada como dois trabalhos diferentes. Por isso, este script realiza uma etapa explícita de **Reference Matching antes da deduplicação dos documentos**.

**Sobre metadados complementares.** Scopus e Web of Science não devem ser tratadas necessariamente como fontes redundantes. Um mesmo documento pode apresentar determinados metadados em uma base e não na outra. A consolidação campo a campo permite aproveitar essas informações complementares.

**Sobre Author Keywords.** Alguns tipos documentais não possuem *author keywords* por natureza. A ausência do campo `DE`, portanto, não significa necessariamente erro de importação.

**Sobre Index Keywords.** O campo equivalente às *Index Keywords* pode apresentar cobertura bastante diferente entre WoS e Scopus e entre áreas do conhecimento. Isso deve ser considerado antes de utilizar esse campo como base para análises temáticas.

**Sobre o threshold do Reference Matching.** O script utiliza como padrão:

```text
0.90
```

Esse parâmetro foi mantido como configuração técnica fixa para evitar que usuários alterem inadvertidamente um parâmetro que pode produzir mudanças importantes na estrutura das redes de citação.

---

## Limitações conhecidas

- A qualidade da análise depende da qualidade dos metadados exportados pelas bases.
- DOIs incorretamente registrados na fonte podem produzir associações equivocadas.
- O matching fuzzy reduz diferenças de formatação entre referências, mas nenhum algoritmo automático garante identificação perfeita em 100% dos casos. Por isso, os arquivos de auditoria são preservados.
- O casamento por título+ano usa normalização agressiva. Títulos genéricos e curtos publicados no mesmo ano podem, em tese, colidir.
- O script foi desenvolvido especificamente para exportações da Web of Science em *plain text* e da Scopus em CSV.
- Outras fontes, como Dimensions, Lens ou OpenAlex, exigem adaptação da etapa de importação.
- Análises baseadas em palavras-chave dependem da qualidade do arquivo de sinônimos fornecido pelo pesquisador. A lista gerada com apoio de IA deve ser revisada antes de ser utilizada.

---

## Biblioshiny

Ao final do fluxo, o script pode abrir:

```r
biblioshiny()
```

Isso permite continuar a exploração do corpus na interface gráfica do `bibliometrix`.

O corpus utilizado pode ser reutilizado posteriormente carregando o `final.rds`.

---

## Como citar

Este repositório está arquivado no Zenodo e possui DOI permanente.

**APA 7:**

> Raposo, L. (2026). *bibliometria: scripts em R para análise bibliométrica com bibliometrix* (Versão 1) [Computer software]. Zenodo. [https://doi.org/10.5281/zenodo.21959591](https://doi.org/10.5281/zenodo.21959591)

**BibTeX:**

```bibtex
@software{raposo_bibliometria_2026,
  author    = {Raposo, Larissa},
  title     = {bibliometria: scripts em R para análise bibliométrica com bibliometrix},
  year      = {2026},
  version   = {v1},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.21959591},
  url       = {https://github.com/lararaposo/bibliometria}
}
```

O DOI `10.5281/zenodo.21959591` é o **DOI de conceito**: aponta sempre para a versão mais recente. Para citar uma versão específica, use o DOI daquela versão (v1: `10.5281/zenodo.21959592`).

O arquivo `CITATION.cff` na raiz do repositório alimenta o botão **"Cite this repository"** do GitHub.

---

## Licença

[MIT](LICENSE) — uso, modificação e redistribuição livres, inclusive comercial, mantido o aviso de autoria.

Observação: o `bibliometrix`, do qual estes scripts dependem, é licenciado sob GPL-3. Os scripts deste repositório não incorporam código do pacote; apenas o invocam.

---

## Autoria e contribuições

Desenvolvido por **Prof.ª Dr.ª Larissa Raposo** (ESPM) para fins acadêmicos.

*Issues*, *pull requests* e *forks* são bem-vindos. Se você usar os scripts em um trabalho publicado, a citação acima é a forma mais útil de retorno.

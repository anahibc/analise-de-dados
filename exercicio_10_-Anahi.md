Exercicio 10
================

``` r
setwd("D:\\dados")
```

### Continuaremos com a utilização dos dados do ESEB2018. Carregue o banco da mesma forma que nos exercicios anteriores

``` r
library(tidyverse)
library(haven)
library(pscl)
library(sjPlot)
library(margins)
library(dotwhisker)
library(dplyr)


link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
  mutate(D10 = as_factor(D10)) %>%
  filter(Q18 < 11,
         D9 < 9999998,
         Q1501 < 11,
         Q12P2_B < 3) %>%
  mutate(Q12P2_B = case_when(Q12P2_B == 1 ~ 0,  # Quem votou em Haddad = 0
                             Q12P2_B == 2 ~ 1)) # Quem votou em Bolsonaro = 1
```

### Crie a mesma variável de religião utilizada no exercício anterior

``` r
Outras <- levels(banco$D10)[-c(3,5,13)]


banco <- banco %>%
  mutate(religiao = case_when(D10 %in% Outras ~ "Outras",
                              D10 == "Católica" ~ "Católica",
                              D10 == "Evangélica" ~ "Evangélica",
                              D10 == "Não tem religião" ~ "Não tem religião"))
```

### Faça uma regressão linear utilizando as mesmas variáveis do exercício 9 - idade(D1A\_ID), educação (D3\_ESCOLA), renda (D9), nota atribuída ao PT (Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião (variável criada no passo anterior) - explicam o voto em Bolsonaro (Q12P2\_B).

``` r
r1 <- lm(Q12P2_B ~ D1A_ID + D3_ESCOLA +  D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco)

summary (r1)
```

    ## 
    ## Call:
    ## lm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.05532 -0.19854  0.01565  0.16182  0.96682 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               7.067e-01  6.469e-02  10.924  < 2e-16 ***
    ## D1A_ID                    1.140e-03  7.539e-04   1.512  0.13074    
    ## D3_ESCOLA                 5.547e-03  5.226e-03   1.061  0.28873    
    ## D9                       -9.837e-07  3.196e-06  -0.308  0.75832    
    ## Q1501                    -7.728e-02  2.799e-03 -27.610  < 2e-16 ***
    ## Q18                       2.651e-02  3.093e-03   8.570  < 2e-16 ***
    ## D2_SEXO                  -5.286e-02  2.089e-02  -2.530  0.01154 *  
    ## religiaoEvangélica        7.684e-02  2.363e-02   3.251  0.00118 ** 
    ## religiaoNão tem religião -2.746e-03  4.238e-02  -0.065  0.94835    
    ## religiaoOutras           -7.263e-02  3.678e-02  -1.975  0.04855 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3489 on 1138 degrees of freedom
    ## Multiple R-squared:  0.5028, Adjusted R-squared:  0.4989 
    ## F-statistic: 127.9 on 9 and 1138 DF,  p-value: < 2.2e-16

``` r
confint(r1)
```

    ##                                  2.5 %        97.5 %
    ## (Intercept)               5.797291e-01  8.335766e-01
    ## D1A_ID                   -3.390684e-04  2.619271e-03
    ## D3_ESCOLA                -4.706619e-03  1.579994e-02
    ## D9                       -7.254959e-06  5.287609e-06
    ## Q1501                    -8.277435e-02 -7.179045e-02
    ## Q18                       2.044034e-02  3.257857e-02
    ## D2_SEXO                  -9.385894e-02 -1.186744e-02
    ## religiaoEvangélica        3.046538e-02  1.232064e-01
    ## religiaoNão tem religião -8.589165e-02  8.039968e-02
    ## religiaoOutras           -1.447902e-01 -4.633807e-04

``` r
dwplot(r1, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))
```

![](exercicio_10_-Anahi_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Interprete o resultado dos coeficientes

### Esse modelo possui como variável dependente, uma variável dummy que pode receber valores 0 para eleitores de Haddad e 1 para eleitores de Bolsonaro. Ademais dentre as variáveis independentes há duas variáveis dummies: sexo e religião. Dessa forma, o intercepto é um valor que vai representar a variável de referência quando todas as demais variáveis forem igual a 0, isto é, homem, católico, mínima idade, mínima escolaridade, mínima renda, sem aceitação ao PT, sem identificação ideológica.

### O intercepto é 7.07e-01 e é estatísticamente significante.A variável idade (D1A\_ID) indica uma relação diretamente proporcional (β = 1.14e-03), mas não é estatisticamente significativa (p valor=0,1307); o que é semelhante com a variável D3\_Escola que indica escolaridade (β = 5.55e-03;p valor 0,2887); variável renda - D9 - com p valor 0,7583 - muito embora indique haver uma relação inversamente proporcional, o que é indicado com sinal negativo do β D9 (-9.84e-07).

### A variável de apreciação ao PT (β Q1501 = -7.73e-02) possui uma relação inversamente proporcional, diminuindo a probabilidade de apreciação a Bolsonaro, estatisticamente significativo com p valor \<2e-16. A variável Q18 (β Q18 = 2.65e-02) mostra que quanto mais a pessoa se identifica com ideologia de direita, maior a probabilidade de aumentar a aprenciação a Bolsonaro, também é estatisticamente significativo com p valor \<2e-16.

### A variável D2\_SEXO representa que a cada aumento da presença feminina, aumenta a probabilidade de rejeição a Bolsonaro (β= -5.29e-02 e p valor = 0.0115 ).

### Sobre a variável religião, o valor β da religião evangélica indica que a mudança em y para cada unidade de X foi de 7.68e-02 , com p valor de 0.0012, isto é, estatisticamente significativo, afetando positivamente a probabilidade da apreciação a Bolsonaro. Dentre os que não têm religião, o valor β -2.75e-03, mas p valor insignificante, muito embora a variável religião tenha p valor menor que 0,05. Por fim, com relação a outras religiões, a aprovação de Bolsonaro se comporta dirimindo a probabilidade de aceitação, mas é significante estatistamente para a variável dependente dummy de voto em Bolsonaro.

\#\#\#r quadrado : 0,503. De acordo com o *residual standard error*
(RMSE) a distância entre valores observados e estimados Ã© de 0.349 para
1138 graus de liberdade. Vale salientar que o p valor em que a variável
dependente é categórica não é confiável, uma vez que há alta
heterocedasticidade.Muito embora os coeficientes sejam, em geral,
parecidos, p valor é mais confiável em regressões logísticas.

### O resultado difere dos encontrados anteriormente, quando a variavel dependente era a aprovação de Bolsonaro?

\#\#\#Sim.Como já ressaltado, a variável Q12P2\_B é categórica com duas
categorias (0 ou 1), portanto, é uma dummy. Nesse caso, seria mais
adequado uma regressão logística para interpretar os resultados. Por
outro lado, a variável Q1607 era contínua nos exercícios anteriores, de
modo que o modelo de regressão linear se mosrtrava mais adequado à
pesquisa. \#\#\# No modelo anterior, a variável de escolaridade
apresenta uma relação inversamente proporcional, isto é, traz impacto
negativo no modelo e é estatítisticamente significante (β=1,134e-01 e p
valor =0.0117) . No modelo presente, o coeficiente muda bastante com β=
5.55e-03 e p valor = 0,2887. A variável renda (D9) om p valor 0,7583 -
muito embora indique haver uma relação inversamente proporcional, o que
é indicado com sinal negativo do β D9 (-9.84e-07) tinha um coeficiente
de valor no modelo anterior -3.632e-05 ambos signficantes.A variável de
apreciação ao PT Q1501 muda de -3,956e-01 para -7,728e-02, com
significancia estatística; assim como na variável Q18 o a mudança do
valor do coeficiente vai de 3,150e-01 para 2,651e-02 e tem significância
estatística.A variável de sexo (D2\_SEXO) também teve a mesma direção
(negativa) e teve um coeficiente menor no exercício atual (diferença de
uma casa decimal). Em ambos os casos, o p-valor foi estatisticamente
significante. A variável de religião, no exercício anterior, não foi
estatisticamente significante nos três grupos (evangélicos, sem religião
e outras religiões) em relação aos católicos, mas havia interação entre
as variáveis. No modelo presente, tanto o grupo evangélico quanto o de
outras religiões foram estatisticamente significativos, mas não há
interação entre variáveis.

### Faça uma regressão logistica com as mesmas variaveis

``` r
r2 <- glm(Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco, family = "binomial")
```

``` r
summary(r2)
```

    ## 
    ## Call:
    ## glm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, family = "binomial", data = banco)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7529  -0.5625   0.2518   0.4744   2.5830  
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)               8.209e-01  5.298e-01   1.550  0.12124    
    ## D1A_ID                    1.001e-02  6.337e-03   1.580  0.11405    
    ## D3_ESCOLA                 5.634e-02  4.358e-02   1.293  0.19602    
    ## D9                       -4.635e-06  2.396e-05  -0.193  0.84660    
    ## Q1501                    -4.678e-01  2.666e-02 -17.545  < 2e-16 ***
    ## Q18                       2.242e-01  2.748e-02   8.159 3.37e-16 ***
    ## D2_SEXO                  -4.497e-01  1.739e-01  -2.586  0.00971 ** 
    ## religiaoEvangélica        6.217e-01  1.985e-01   3.132  0.00173 ** 
    ## religiaoNão tem religião -2.106e-02  3.478e-01  -0.061  0.95172    
    ## religiaoOutras           -6.736e-01  3.122e-01  -2.158  0.03096 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1557.84  on 1147  degrees of freedom
    ## Residual deviance:  862.45  on 1138  degrees of freedom
    ## AIC: 882.45
    ## 
    ## Number of Fisher Scoring iterations: 5

### Transforme os coeficientes estimados em probabilidade

``` r
library(margins)
margins(r2)
```

    ##    D1A_ID D3_ESCOLA         D9    Q1501     Q18 D2_SEXO religiaoEvangélica
    ##  0.001171  0.006589 -5.421e-07 -0.05471 0.02622 -0.0526            0.07346
    ##  religiaoNão tem religião religiaoOutras
    ##                 -0.002521       -0.08172

``` r
summary(margins(r2))
```

    ##                    factor     AME     SE        z      p   lower   upper
    ##                    D1A_ID  0.0012 0.0007   1.5849 0.1130 -0.0003  0.0026
    ##                   D2_SEXO -0.0526 0.0202  -2.6078 0.0091 -0.0921 -0.0131
    ##                 D3_ESCOLA  0.0066 0.0051   1.2949 0.1953 -0.0034  0.0166
    ##                        D9 -0.0000 0.0000  -0.1935 0.8466 -0.0000  0.0000
    ##                     Q1501 -0.0547 0.0009 -57.9079 0.0000 -0.0566 -0.0529
    ##                       Q18  0.0262 0.0030   8.8434 0.0000  0.0204  0.0320
    ##        religiaoEvangélica  0.0735 0.0235   3.1280 0.0018  0.0274  0.1195
    ##  religiaoNão tem religião -0.0025 0.0417  -0.0605 0.9517 -0.0842  0.0791
    ##            religiaoOutras -0.0817 0.0379  -2.1574 0.0310 -0.1560 -0.0075

``` r
pR2(r2)
```

    ## fitting null model for pseudo-r2

    ##          llh      llhNull           G2     McFadden         r2ML         r2CU 
    ## -431.2245843 -778.9190068  695.3888450    0.4463807    0.4543292    0.6118347

``` r
efeito_log <- summary(margins(r2)) %>%
  rename(term = factor,
  estimate = AME,
  std.error = SE,
  statistic = z,
  p.value = p) %>%
  arrange(estimate)
dwplot(efeito_log)
```

![](exercicio_10_-Anahi_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
\#\#\# Quais foram as diferenças no resultado entre usar a regressão
linear e a logistica?

\#\#\#A regressão logística apresenta o efeito médio sobre o voto em
Bolsonaro em termos percentuais para cada aumento unitário na variável
independente. Assim, na regressão logística temos que as variáveis de
*idade* (efeito positivo de 0.12%), *renda* (com coeficiente -0.00% e
p-valor 0.8466) e *escolaridade* (relação positiva de 0.66%) sem
significância estatística. A variável de *sexo* tem relação negativa, o
que significa que ser mulher diminui a chance de votar em Bolsonaro em
-5.26% e p-valor de 0.0091, portanto, significante estatisticamente. Já
a *apreciação do PT* tem efeito negativo de -5.47% sobre o voto em
Bolsonaro com p-valor 0.000. A variável Q18 *autoidentificação
ideológica* tem efeito positivo de 2.62% sobre o voto no candidato com
significância estatística . Por fim, a variável *religião* que tem por
categoria de referência a religião católica. Caso a pessoa seja
*evangélica* há um efeito positivo de 7.35% sobre a preferência por
Bolsonaro em comparação aos católicos, estatisticamente significante (p
valor 0.0018). Pessoas *sem religião* tem efeito negativo de -0.25%, no
entanto, sem significância estatística visto o p-valor de 0.9517. Por
fim, as pessoas que se declaram perterncentes a *outras religiões* tem
efeito negativo de -8.17% no voto em Bolsonaro comparado aos católicos
com significância estatística de 0.0310. \#\#\#O pseudo r2 calculado foi
de foi de 0,4463, ou 44,63%. \#\#\#A regressão logística, como visto no
exemplo desse exercício, pode resolver as questões ligadas a
pressupostos da linearidade e problemas de heterocedasticidade nas
regressões lineares para variáveis dependentes categóricas dummy.

### Verifique a quantidade de classificações corretas da regressao logistica e avalie o resultado

``` r
library(InformationValue)
```

``` r
predicted_prob <- predict(r2, type = "response")
```

``` r
1 - misClassError(banco$Q12P2_B, 
                  predicted_prob, 
                  threshold = 0.5)
```

    ## [1] 0.8301

``` r
opt_cutoff <- optimalCutoff(banco$Q12P2_B, 
                            predicted_prob)
```

``` r
1 - misClassError(banco$Q12P2_B, 
                  predicted_prob, 
                  threshold = 0.556687512770529)
```

    ## [1] 0.8362

``` r
confusionMatrix(banco$Q12P2_B, 
              predicted_prob, 
              threshold = opt_cutoff)
```

    ##     0   1
    ## 0 393 105
    ## 1  83 567

``` r
prop.table(confusionMatrix(banco$Q12P2_B, 
                predicted_prob, 
                threshold = opt_cutoff))
```

    ##            0          1
    ## 0 0.34233449 0.09146341
    ## 1 0.07229965 0.49390244

``` r
table(banco$Q12P2_B)
```

    ## 
    ##   0   1 
    ## 476 672

``` r
prop.table(table(banco$Q12P2_B))
```

    ## 
    ##         0         1 
    ## 0.4146341 0.5853659

``` r
0.8362/0.5853659
```

    ## [1] 1.428508

\#\#\#A quantidade de classificações corretas é 83,01%. Observando os
votos em Bolsonaro, o modelo previu corretamente 49,39% votos em
Bolsonaro e erroneamente 9,14% dos votos. Já observando os votos em
Haddad, o modelo previu corretamente 34,23% votos e erroneamente 7,22%
votos dos em Haddad.

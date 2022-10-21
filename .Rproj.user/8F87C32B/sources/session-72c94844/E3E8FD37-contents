#OBS: https://cran.r-project.org/web/packages/qcc/vignettes/qcc_a_quick_tour.html
#OBS: https://swirlstats.com/
#comentario de varias linhas: control+shift+C
#x<-2+2
#log(10)
x<-c(23,56,34,23,12,56)
y<-mean(x)
z<-sd(x)
w<-var(x)

summary(x)

#Gerador de dados Aleatorios:
#rnorm(média, desvio_padrão)
rnorm(100,3,1)
norm<-rnorm(100,3,1)
mean(norm)
#Plotar(imprimir grafico)
plot(rnorm(100,3,1))
rexp(1000,1)
plot(rexp(1000,1))

#FERRAMENTAS DA QUALIDADE
#Instalando a biblioteca: DiagrammeR
install.packages("DiagrammeR")
library(DiagrammeR)

DiagrammeR::grViz("digraph{
                  graph[layout=dot,randir=RR]
                  node[shape=oval]
                  1 [label='Inicio']
                  node[shape=rectangle]
                  2 [label='Etapa 1']
                  3 [label='Etapa 2']
                  node[shape=oval]
                  4 [label='Fim']
                  1->2->4
                  1->3->4 }", height=300)

colors()#pesquisa de cores

#Tempo para chegar em casa
set.seed(36)
dados<-rnorm(200,20,5)
hist(dados, col="green", border = "yellow",
     xlab = "Tempo",
     ylab = "Frequência",
     main = "Tempo de retorno para casa")


#Exemplo 1 Consumo de combustivel:
data(mtcars)
head(mtcars)
hd<-hist(mtcars$mpg,
         main = "Consumo de Combustivel")
summary(mtcars$mpg)
boxplot(mtcars$mpg)

#Folha de Verificação:
#Identificando osdefeitos
folha<-rbind(data.frame(Defeito = "Alinhamento", Item = "Componente X"),
             data.frame(Defeito = "Solda", Item = "Componente Y"),
             data.frame(Defeito = "Parafuso", Item = "Componente X"),
             data.frame(Defeito = "Junção", Item = "Componente Y"))
#Incluindoa Frequencia por inspetor
folha$Inspetor_A <- c(0,0,4,3)

folha$Inspetor_B <- c(5,0,5,3)

folha$Inspetor_C <- c(2,1,6,4)

folha$Total <- folha$Inspetor_A +
  folha$Inspetor_B +
  folha$Inspetor_C

folha

install.packages("writexl")
library(writexl)
write_xlsx(folha, "C:\\folha.xlsx")

install.packages("readxl")
library(readxl)
read_excel("/cloud/project/file_show.xlsx")

planilha <-read_excel("/cloud/project/file_show.xlsx")
planilha$Inspetor_B

#Média Inspetor_B
mean(planilha$Inspetor_B)

install.packages("qcc")
library(qcc)
defeitos = c(80,27,66,94,33)
names(defeitos) = c("código errado", "atraso na entrega",
                    "erro do fornecedor", "erro de contato",
                    "Número de Série Errado")
pareto.chart(defeitos,ylab = "Frequência")

###########################################################
#####################   2° DIA  ###########################
###########################################################

#DIAGRAMA DE ISHIKAWA
library(qcc)

#Construindo odiagrama de Ishikawa
cause.and.effect(cause = list(
  Medida = c("Falta de calibração","Baixa Precisão"),
  Material = c("Trincas","Baixa dureza"),
  Pessoas = c("Indiciplina","Falta de treinamento"),
  Ambientes = c("Alta temperatura"),
  Método = c("Falta de Procedimento"),
  Maquina = c("Falta de Manutenção")),
  effect = c("Peça Quebrada"),
  title = c("Diagrama de Causa e Efeito")
)
################################################################

################Diagrama de Dispersão###########################

head(iris)
dim(iris)#Verifica a dimenção do banco de dados.

#Analisando Bancode dados.
summary(iris$Petal.Length)
summary(iris$Species)

#Verificando se há correlação entre comprimento e largura da sepala
plot(iris$Sepal.Width, iris$Sepal.Length,
     ylab = "Largura", xlab = "Comprimento",
     col=iris$Species, main = "Iris", pch=20)

#Verificar correlação(Positiva, Negativa ou Nula)
cor(iris$Sepal.Width, iris$Sepal.Length)


################Grafico de Controle###########################

library(qcc)
#Chamando Banco de dados de pistão
data(pistonrings)
#Exibindo Banco de dados de pistão
head(pistonrings)
#
summary(pistonrings$sample)
#
dados_variavel <- with(pistonrings, qcc.groups(diameter,sample))
carta_xbar <- qcc(dados_variavel, type = "xbar")

carta_r <- qcc(dados_variavel,type = "R")

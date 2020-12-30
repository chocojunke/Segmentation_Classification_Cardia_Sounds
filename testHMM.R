library(arules)  # Necessaria para discretizar o sinal
library(tuneR)   # Importar ficheiro wave
library(seewave) # Necessaria para calcular o envelope
library(HMM)
library(beepr)
library(xlsx)

# Numero de intervalos e metodo utilizado (igual aos utilizados no treino do modelo)
numberOfBreaks = 13
method = "fixed"

# Path necessario para importar os dados provenientes do treino do modelo
#workspacePath = dirname(rstudioapi::getSourceEditorContext()$path) # Pasta PROEST
#workspacePath = paste(workspacePath, "/workspace", method, numberOfBreaks, "comprocess.RData", sep="")
#workspacePath = gsub("/", "\\\\", workspacePath)
# Importar o workspace onde constam os dados provenientes do treino do modelo
load("C:\\Users\\paulo\\Desktop\\APP\\workspace.RData")

# Ja definidos no workspace
#numStates = 4
#states = c("S1", "siSys", "S2", "siDia")
#symbols = letters[1:numberOfBreaks] # a, b, c, d, e, f, g, h, i, j

indicesTestes = c(60:63, 65:90)

# Caminho do ficheiro .csv onde constam os nomes dos ficheiros audio e os locais de S1 e S2
excel = "C:\\Users\\paulo\\Desktop\\ISEP\\3_Ano\\2_Semestre\\PROEST\\Project\\Btraining_normal_seg.csv"
# Importar os dados do ficheiro .csv
dados <- read.csv(file = excel)

paths = c() # Vetor para guardar todos os caminhos dos ficheiros com identificacao de S1 e S2
S1S2values = list() # Vetor para guardar todos os pontos S1 e S2
for (i in indicesTestes) {
  dadosTemp = dados[i,] # Valores de cada linha do ficheiro .csv
  paths[i] = gsub(".aiff", ".wav", dadosTemp[1]) # Substituir a extensao do ficheiro
  S1S2valuesTemp = dadosTemp[1:length(dadosTemp)] # Vetor com os valores de S1 e S2
  S1S2valuesTemp = S1S2valuesTemp[!is.na(S1S2valuesTemp)] # Remover os NA's do vetor
  
  S1S2values[[i]] = S1S2valuesTemp[2:length(S1S2valuesTemp)]
}

s1values = list() # Valores dos estados S1
s2values = list() # Valores dos estados S2
for (i in indicesTestes ) {
  s1s2tempValues = S1S2values[[i]]
  s1tempValues = c()
  s2tempValues = c()
  for (j in 1:length(s1s2tempValues)) {
    if ( (j %% 2) == 0)
      s1tempValues[j/2] = s1s2tempValues[j]
    else
      s2tempValues[ (j+1)/2 ] = s1s2tempValues[j]
  }
  s1values[[i]] = strtoi( s1tempValues [ s1tempValues!= 0] )
  s2values[[i]] = strtoi( s2tempValues [ s2tempValues!= 0] )
}


signalsWave = list() # Sinais iniciais
envelopesSignals = list() # Sinais apos envelope
discretizedSignals = list() # Sinais apos envelope e discretizacao
for (i in indicesTestes) {
  
  # Path completo para cada um dos ficheiros audio
  pathTemp = paste("C:\\Users\\paulo\\Desktop\\ISEP\\3_Ano\\2_Semestre\\PROEST\\EVERYTHING\\Dataset B Wav\\Btraining_normal_filtered\\", paths[i], sep = "")
  # Importar cada um dos ficheiros wave
  signal = readWave(pathTemp, from=1, to=Inf, units=c("samples"), header=FALSE, toWaveMC=NULL)
  
  # Guardar na lista correspondente, dos sinais iniciais
  signalsWave[[i]] = signal
  
  # Calcular envelope de cada sinal
  # ssmooth corresponde a uma suavizacao atraves de soma de uma janela, neste caso de 300 pontos
  y = env(signal, norm = TRUE, ssmooth=300, plot = FALSE)
  
  envelopesSignals[[i]] = y # Guardar na lista correspondente ( Sinais apos envelope )
  plot(y, type="l", main=paste("Sinal", paths[i]), xlab="Indice", ylab="Amplitude", col = "black")
  verticallines = S1S2values[[i]]
  abline(v = verticallines , col = "red", lty = "dashed", lwd = 0.5)
  
  # Discretizar o sinal atraves do mesmo metodo e numero de intervalos do treino
  dx<-discretize(y, method = method, breaks = c(-0.05,0.025,0.05,0.075,0.1,0.15,0.2,0.4,0.5,0.6,0.7,0.8,0.9,1.05) )
  levels(dx)<-letters[1:numberOfBreaks] # N?veis da divis?o
  discretizedSignals[[i]] = dx # Guardar na lista correspondente ( Sinais apos envelope e discretizacao)
  
  signalLevels = attr(dx, "discretized:breaks") # Obtencao dos limites dos intervalos
  plot(y, type="l", main="Sinal", xlab="Tempo (s)", ylab="Amplitude", col = "black", ylim = c(-0.05,1.05))
  par(new = TRUE)
  abline(h = signalLevels , col = "red", lty = "dashed", lwd = 0.5) #Desenho de linhas horizontais correspondentes aos limites dos intervalos
  
}


# Tempo de  S1 e S2
s1period = 0.11 #s
s2period = 0.1 #s

# Calcular todos os estados com base: - No ponto medio de S1 e S2
# - No tempo de S1 e S2 e correspondente numero de amostras com determinada amostragem
# - Dividir S1 e S2 para a esquerda e direita igualmente
statesSeqs = list()
for (i in indicesTestes) {
  statesTemp = c()
  frequency = signalsWave[[i]]@samp.rate
  s1SampleNumber = frequency*s1period
  s2SampleNumber = frequency*s2period
  s1ValuesTemp = s1values[[i]]
  s2ValuesTemp = s2values[[i]]
  
  s1ValuesTemp = strtoi( s1ValuesTemp[ s1ValuesTemp != 0] ) # Remover 0's
  s2ValuesTemp = strtoi( s2ValuesTemp[ s2ValuesTemp != 0] )# Remover 0's
  
  for (j in 1:length(s1ValuesTemp) ) {
    if ( (s1ValuesTemp[j]-s1SampleNumber/2) >= 1 && (s1ValuesTemp[j]+s1SampleNumber/2) <= length(signalsWave[[i]]@left) )
      statesTemp[ (s1ValuesTemp[j]-s1SampleNumber/2):(s1ValuesTemp[j]+s1SampleNumber/2) ] = 1 # S1
    else {
      if ( (s1ValuesTemp[j]-s1SampleNumber/2) < 1 )
        statesTemp[ 1:(s1ValuesTemp[j]+s1SampleNumber) ] = 1 # S1
      else {
        statesTemp[ (s1ValuesTemp[j]+s1SampleNumber):length(signalsWave[[i]]@left) ] = 1 } # S1
    }
  }
  
  for (j in 1:length(s2ValuesTemp) ) {
    if ( (s2ValuesTemp[j]-s2SampleNumber/2) >= 1 && (s2ValuesTemp[j]+s2SampleNumber/2) <= length(signalsWave[[i]]@left) )
      statesTemp[ (s2ValuesTemp[j]-s2SampleNumber/2):(s2ValuesTemp[j]+s2SampleNumber/2) ] = 3 # S1
    else {
      if ( (s2ValuesTemp[j]-s2SampleNumber/2) < 1 )
        statesTemp[ 1:(s2ValuesTemp[j]+s2SampleNumber) ] = 3 # S1
      else 
        statesTemp[ (s2ValuesTemp[j]+s2SampleNumber):length(signalsWave[[i]]@left) ] = 3 # S1
    }
  }
  
  
  if (s1ValuesTemp[1] < s2ValuesTemp[1]) {
    if ( (s1ValuesTemp[1]-s1SampleNumber/2) > 1 )
      statesTemp[ 1:(s1ValuesTemp[1]-s1SampleNumber/2) ] = 4
  } else {
    if ( (s2ValuesTemp[1]-s2SampleNumber/2) > 1 ) {
      statesTemp[ 1:(s2ValuesTemp[1]-s2SampleNumber/2) ] = 2 
    }
  }
  
  
  if ( s1ValuesTemp[1] < s2ValuesTemp[1]) {
    if ( (s1ValuesTemp[1]-s1SampleNumber/2) > 1 ) {
      statesTemp[ 1:(s1ValuesTemp[1]-s1SampleNumber/2) ] = 4 } } # siDia
  else {
    if ( (s2ValuesTemp[1]-s2SampleNumber/2) > 1 ) {
      statesTemp[ 1:(s2ValuesTemp[1]-s2SampleNumber/2) ] = 2 } } # siSys
  
  for (j in 2:length(signalsWave[[i]]@left) ) {
    if ( statesTemp[j-1]==1 &&  is.na(statesTemp[j])==TRUE )
    {statesTemp[j] = 2} # siSys
    if ( statesTemp[j-1]==2 &&  is.na(statesTemp[j])==TRUE )
    {statesTemp[j] = 2} # siDia
    if ( statesTemp[j-1]==3 &&  is.na(statesTemp[j])==TRUE )
    {statesTemp[j] = 4} # siSys
    if ( statesTemp[j-1]==4 &&  is.na(statesTemp[j])==TRUE )
    {statesTemp[j] = 4} # siDia
  }
  
  statesSeqs[[i]]=statesTemp
}

# Obter apenas a sequencia de simbolos de cada uma das sequencias em estudo
obs = list()
for (i in indicesTestes)
  obs[[i]] = letters[ discretizedSignals[[i]] ]


# Calcular as probabilidades posteriores de cada estado para cada instante temporal
probabilities = list()
for (i in indicesTestes) # 60:90
  probabilities[[i]] = posterior(hmm, obs[[i]])

# Para cada instante temporal, calcular qual o estado que apresenta a maior probabilidade
predictedStatesPosterior = list() # "S1", "siSys", "S2", "siDia"
predictedValuesPosterior = list() # 1, 2, 3, 4
for (i in indicesTestes) {  # 60:90
  tempStatesSeq = c()
  tempValuesSeq = c()
  probabilitiesTempSeq = probabilities[[i]]
  for (j in 1:length(obs[[i]])) {
    tempStatesSeq[j] = states [ which.max( probabilitiesTempSeq[,j] ) ]
    tempValuesSeq[j] = which.max( probabilitiesTempSeq[,j] )
  }
  predictedStatesPosterior[[i]] = tempStatesSeq
  predictedValuesPosterior[[i]] = tempValuesSeq
}

# MATRIZ, 4:4, LINHAS CORRESPONDEM AO ESTADO CORRETO, COLUNAS AO ESTADO OBTIDO
testMatrix = matrix(data = 0, nrow = numStates, ncol = numStates)
rownames(testMatrix) = states
colnames(testMatrix) = states
for (i in indicesTestes) {
  # TRUE STATES
  trueStates = statesSeqs[[i]]
  # PREDICTED STATES (POSTERIOR)
  predictedStates = predictedValuesPosterior[[i]]
  
  for (j in 1:length(trueStates)) {
    testMatrix[ trueStates[j], predictedStates[j] ] = testMatrix[ trueStates[j], predictedStates[j] ] + 1 # Matriz [ estadoCorreto, estadoPrevisto ]
  }
}

s1ConfusionMatrix = matrix(data = 0, nrow = 2, ncol = 2)
rownames(s1ConfusionMatrix) = c("S1", "Não S1")
colnames(s1ConfusionMatrix) = c("S1", "Não S1")
s1ConfusionMatrix[1,1]=testMatrix[1,1] #TP - TRUE POSITIVE
s1ConfusionMatrix[2,1]=sum(testMatrix[2:4,1]) # FP - FALSO POSITIVO
s1ConfusionMatrix[1,2]=sum(testMatrix[1,2:4]) #FN - FALSE NEGATIVE
s1ConfusionMatrix[2,2]=sum(testMatrix[2:4,2:4]) # FP - TRUE NEGATIVE
print(s1ConfusionMatrix)

siSysConfusionMatrix = matrix(data = 0, nrow = 2, ncol = 2)
rownames(siSysConfusionMatrix) = c("siSys", "Não siSys")
colnames(siSysConfusionMatrix) = c("siSys", "Não siSys")
siSysConfusionMatrix[1,1]=testMatrix[2,2] #TP - TRUE POSITIVE
siSysConfusionMatrix[2,1]=sum(testMatrix[c(1,3,4),2]) # FP - FALSO POSITIVO
siSysConfusionMatrix[1,2]=sum(testMatrix[2,c(1,3,4)]) #FN - FALSE NEGATIVE
siSysConfusionMatrix[2,2]=sum(testMatrix[c(1,3,4),c(1,3,4)]) # FP - TRUE NEGATIVE
print(siSysConfusionMatrix)

s2ConfusionMatrix = matrix(data = 0, nrow = 2, ncol = 2)
rownames(s2ConfusionMatrix) = c("S2", "Não S2")
colnames(s2ConfusionMatrix) = c("S2", "Não S2")
s2ConfusionMatrix[1,1]=testMatrix[3,3] #TP - TRUE POSITIVE
s2ConfusionMatrix[2,1]=sum(testMatrix[c(1,2,4),3]) # FP - FALSO POSITIVO
s2ConfusionMatrix[1,2]=sum(testMatrix[3,c(1,2,4)]) #FN - FALSE NEGATIVE
s2ConfusionMatrix[2,2]=sum(testMatrix[c(1,2,4),c(1,2,4)]) # FP - TRUE NEGATIVE
print(s2ConfusionMatrix)

siDiaConfusionMatrix = matrix(data = 0, nrow = 2, ncol = 2)
rownames(siDiaConfusionMatrix) = c("siDia", "Não siDia")
colnames(siDiaConfusionMatrix) = c("siDia", "Não siDia")
siDiaConfusionMatrix[1,1]=testMatrix[4,4] #TP - TRUE POSITIVE
siDiaConfusionMatrix[2,1]=sum(testMatrix[c(1,2,3),4]) # FP - FALSO POSITIVO
siDiaConfusionMatrix[1,2]=sum(testMatrix[4,c(1,2,3)]) #FN - FALSE NEGATIVE
siDiaConfusionMatrix[2,2]=sum(testMatrix[c(1,2,3),c(1,2,3)]) # FP - TRUE NEGATIVE
print(siDiaConfusionMatrix)

total = (s1ConfusionMatrix+siSysConfusionMatrix+s2ConfusionMatrix+siDiaConfusionMatrix)/4
rownames(siDiaConfusionMatrix) = c("Positivo", "Negativo")
colnames(siDiaConfusionMatrix) = c("Positivo", "Negativo")
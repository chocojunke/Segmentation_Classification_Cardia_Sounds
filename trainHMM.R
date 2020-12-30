library(arules)  # Necessaria para discretizar o sinal
library(tuneR)   # Importar ficheiro wave
library(seewave) # Necessaria para calcular o envelope
library(HMM)

# Numero de intervalos e metodo utilizado 
numberOfBreaks = 1
method = "interval"

# Para cada instante do sinal, e identificado o intervalo / nivel ao qual pertence, sendo definido o numero de intervalos e o metodo de divisao
# Ao utilizar "method = "interval"", todos os intervalos apresentam a mesma amplitude. 
# Ao usar "method = "frequency"" (default), todos os intervalos apresentam aproximadamente a mesma frequencia
# Com "method = "fixed"", podem ser definidos os intervalos desejados
# com "method = "cluster"", os dados sao divididos segundo o algoritmo "k-means clustering", que organiza os pontos em grupos
numStates = 4
states = c("S1", "siSys", "S2", "siDia")
symbols = letters[1:numberOfBreaks] # a, b, c, d, e, f, g, h, i, j

numberOfTrainingSet = 2

# Funcao para calcular as probabilidades inicias atraves da frequencia de cada estado possivel
# no primeiro estado de cada sequencia
getstartprobs <- function(statesSeqs) {
  startprobs = matrix(data = 0, nrow = numStates, ncol = 1)
  rownames(startprobs) = states
  for (i in 1:numberOfTrainingSet) {
    statesseqTemp = statesSeqs[[i]]
    startprobs[ statesseqTemp[1], 1 ] = startprobs[ statesseqTemp[1], 1 ] + 1
  }
  startprobs = startprobs/sum(startprobs)
  return(startprobs)
}

# Calculo da matriz de transicao atraves das sequencias de estados importadas
gettransitionmatrix <- function(statesseq, numStates, states) {
  transitionmatrix = matrix(data = 0, nrow = numStates, ncol = numStates)
  rownames(transitionmatrix) = states
  colnames(transitionmatrix) = states
  # Calculo frequencia absoluta
  for (i in 2:length(statesseq)) {
    transitionmatrix[statesseq[i-1], statesseq[i]] = transitionmatrix[statesseq[i-1], statesseq[i]] + 1
  }
  # Calculo frequencia relativa
  for (i in 1:numStates) {
    transitionmatrix[i,] = round(transitionmatrix[i,] / sum(transitionmatrix[i,]), 9)
  }
  print(transitionmatrix)
  return(transitionmatrix)
}

getemissionmatrix <- function(statesseq, symbolsseq, numStates, states, symbols) {
  emissionmatrix = matrix(data = 0, nrow = numStates, ncol = numberOfBreaks)
  rownames(emissionmatrix) = states
  colnames(emissionmatrix) = symbols
  # Frequencia absoluta
  for (i in 1:length(statesseq)) {
    emissionmatrix[statesseq[i], symbolsseq[i]] = emissionmatrix[statesseq[i], symbolsseq[i]] + 1
  }
  # Frequencia relativa
  for (i in 1:numStates) {
    emissionmatrix[i,] = round(emissionmatrix[i,] / sum(emissionmatrix[i,]), 9)
  }
  print(emissionmatrix)
  return(emissionmatrix)
}

# Caminho do ficheiro .csv onde constam os nomes dos ficheiros audio e os locais de S1 e S2
excel = "C:\\Users\\paulo\\Desktop\\ISEP\\3_Ano\\2_Semestre\\PROEST\\Project\\Btraining_normal_seg.csv"
# Importar os dados do ficheiro .csv
dados <- read.csv(file = excel)

paths = c() # Vetor para guardar todos os caminhos dos ficheiros com identificacao de S1 e S2
S1S2values = list() # Vetor para guardar todos os pontos S1 e S2
for (i in 1:numberOfTrainingSet) {
  dadosTemp = dados[i,] # Valores de cada linha do ficheiro .csv
  paths[i] = gsub(".aiff", ".wav", dadosTemp[1]) # Substituir a extensao do ficheiro
  S1S2valuesTemp = dadosTemp[1:length(dadosTemp)] # Vetor com os valores de S1 e S2
  S1S2valuesTemp = S1S2valuesTemp[!is.na(S1S2valuesTemp)] # Remover os NA's do vetor
  
  S1S2values[[i]] = S1S2valuesTemp[2:length(S1S2valuesTemp)]
}

s1values = list() # Valores dos estados S1
s2values = list() # Valores dos estados S2
for (i in 1:numberOfTrainingSet) {
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
for (i in 1:numberOfTrainingSet) {

  # Path completo para cada um dos ficheiros audio
  pathTemp = paste("C:\\Users\\paulo\\Desktop\\ISEP\\3_Ano\\2_Semestre\\PROEST\\EVERYTHING\\Dataset B Wav\\Training B Normal\\", paths[i], sep = "")
  # Importar cada um dos ficheiros wave
  signal = readWave(pathTemp, from=1, to=Inf, units=c("samples"), header=FALSE, toWaveMC=NULL)
  
  
  # Guardar na lista correspondente
  signalsWave[[i]] = signal
  
  # Calcular envelope de cada som
  # ssmooth corresponde a uma suavizacao atraves de soma de uma janela, neste caso de 300 pontos
  y = env(signal, norm = TRUE, ssmooth=300, plot = FALSE)
  y[1:149] = y[150]
  y[(length(y)-149):length(y)] = y[length(y)-150]
  y = y - min(y)
  y = y / max(y)
  
  envelopesSignals[[i]] = y # Guardar na lista correspondente ( Sinais apos envelope )
  plot(y, type="l", main=paste("Sinal", paths[i]), xlab="Indice", ylab="Amplitude", col = "black")
  verticallines = S1S2values[[i]]
  abline(v = verticallines , col = "red", lty = "dashed", lwd = 0.5)
  
  # Discretizar o sinal atraves do metodo e do numero de intervalos indicados anteriormente
  dx<-discretize(y, method = method, breaks = numberOfBreaks )
  levels(dx)<-letters[1:numberOfBreaks] # Niveis da divisao
  discretizedSignals[[i]] = dx # Guardar na lista correspondente
  
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
for (i in 1:numberOfTrainingSet) {
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

# Obter probabilidades iniciais
startprobs <- getstartprobs(statesSeqs)

# Obter matriz de transicao
transitionmatrix = gettransitionmatrix(states[statesSeqs[[1]]], numStates, states)
transitionmatrix[ transitionmatrix == 0 ] = 1E-12

# Obter matriz de emissao
emissionmatrix = getemissionmatrix(states[statesSeqs[[1]]], discretizedSignals[[1]], numStates, states, symbols)
emissionmatrix[ emissionmatrix == 0 ] = 1E-12

# Iniciar o modelo com as probabilidades calculadas anteriormente
hmm = initHMM(states, symbols, startProbs = startprobs, transProbs=transitionmatrix, emissionProbs=emissionmatrix)
# Ajustar as probabilidades atraves do algoritmo EM
bw = baumWelch(hmm, discretizedSignals[[1]], maxIterations=100, delta=1E-9, pseudoCount = 1E-9)

# Ajustar o valor de hmm de acordo com o algoritmo EM para todas as amostras de treino
for (i in 2:numberOfTrainingSet) {
  print(i)
  print(bw$hmm)
  seqnew = discretizedSignals[[i]]
  bw = baumWelch(hmm, seqnew, maxIterations=100, delta=1E-9, pseudoCount = 1E-9)
  bw$hmm$transProbs[ bw$hmm$transProbs == 0 ] = 1E-12
  bw$hmm$emissionProbs[ bw$hmm$emissionProbs == 0 ] = 1E-12
  hmm = bw$hmm
} 
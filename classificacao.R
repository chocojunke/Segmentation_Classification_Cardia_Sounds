library(e1071) # Utilizada para calcular STFT
library(class) # Utilizada para o classificador KNN
library(tuneR) # Utilizada para importar ficheiro .wav e calcular MFCCs

numberOfCoefs = 96 # NUmero de MFCCs e coeficientes de Fourier

# Obter os caminhos para todos os ficheiros .wav normais e com murmurios
path = dirname(rstudioapi::getSourceEditorContext()$path) # Path do ficheiro atual
pathNormal = paste(path, "/Dataset B WAV/Training B Normal", sep="") # Path da pasta dos ficheiros normais
pathNormal = gsub("/", "\\\\", pathNormal)
pathMurmor = paste(path, "/Dataset B WAV/Btraining_murmur", sep="") # Path da pasta dos ficheiros com murmUrios
pathMurmor = gsub("/", "\\\\", pathMurmor)
pathsNormal = list.files(pathNormal, full.names = TRUE, pattern=".wav") # Obter todos path dos ficheiros .wav normais
pathsMurmor = list.files(pathMurmor, full.names = TRUE, pattern=".wav") # Obter todos path dos ficheiros .wav com murmurios
trainNormalIndex = sample(1:length(pathsNormal), (2/3)*length(pathsNormal)) # Obter, aleatoriamente, os indices de treino dos ficheiros normais
trainMurmorIndex = sample(1:length(pathsMurmor), (2/3)*length(pathsMurmor)) # Obter, aleatoriamente, os indices de treino dos ficheiros com murmurios
# Calcular os indices das amostras de teste normais (restantes)
x = rep(TRUE, length(pathsNormal))
for (i in 1:length(pathsNormal)) {
  flag = FALSE
  for (j in trainNormalIndex)
    if ( i == j)
      flag = TRUE
    if (flag == TRUE)
      x[i] = FALSE
}
testNormalIndex = (1:length(pathsNormal))[x]
# Calcular os indices das amostras de teste com murmurios (restantes)
x = rep(TRUE, length(pathsMurmor))
for (i in 1:length(pathsMurmor)) {
  flag = FALSE
  for (j in trainMurmorIndex)
    if ( i == j)
      flag = TRUE
    if (flag == TRUE)
      x[i] = FALSE
}
testMurmorIndex = (1:length(pathsMurmor))[x]

# OBTER SINAIS NORMAIS
normalSignalsWave = list() # Sinais normais
normalSTFT = list() # STFTs dos sinais normais
normalMFCC = list() # MFCCs dos sinais normais
for (i in 1:length(pathsNormal)) {
  
  # Path completo para cada um dos ficheiros audio normais
  pathTemp = pathsNormal[i]
  
  # Importar cada um dos ficheiros .wav normais
  signal = readWave(pathTemp, from=1, to=Inf, units=c("samples"), header=FALSE, toWaveMC=NULL)
  
  # Guardar na lista correspondente
  normalSignalsWave[[i]] = signal
  
  # Calcular STFT para cada um dos sinais normais
  x = c()
  stftsTemp = stft(signal@left, win=96, inc=24, coef=numberOfCoefs, wtype="hanning.window")
  # Calcular a media de cada coeficiente
  for (j in 1:numberOfCoefs) {
    x[j] = sum(stftsTemp$values[,j])/length(stftsTemp$values[,j])
  }
  normalSTFT[[i]] = x
  
  # Calcular MFCC para cada um dos sinais normais
  x = c()
  MFCCtemp = melfcc(signal, sr = signal@samp.rate, wintime = 0.025, hoptime = 0.01, numcep = numberOfCoefs)  
  # Calcular a media de cada coeficiente
  for (j in 1:numberOfCoefs) {
    x[j] = sum(MFCCtemp[,j])/length(MFCCtemp[,j])
  }
  normalMFCC[[i]] = x
}

# OBTER SINAIS MURMOR
murmorSignalsWave = list() # Sinais iniciais
murmorSTFT = list() # STFTs
murmorMFCC = list() # MFCCs
for (i in 1:length(pathsMurmor)) {
  
  # Path completo para cada um dos ficheiros audio com murmurios
  pathTemp = pathsMurmor[i]
  
  # Importar cada um dos ficheiros .wav com murmurios
  signal = readWave(pathTemp, from=1, to=Inf, units=c("samples"), header=FALSE, toWaveMC=NULL)
  
  # Guardar na lista correspondente
  murmorSignalsWave[[i]] = signal
  
  # Calcular STFT para cada um dos sinais com murmurios
  x = c()
  stftsTemp = stft(signal@left, win=96, inc=24, coef=numberOfCoefs, wtype="hanning.window")  
  # Calcular a media de cada coeficiente
  for (j in 1:numberOfCoefs) {
    x[j] = sum(stftsTemp$values[,j])/length(stftsTemp$values[,j])
  }
  murmorSTFT[[i]] = x
  
  # Calcular MFCC para cada um dos sinais com murmurios
  x = c()
  MFCCtemp = melfcc(signal, sr = signal@samp.rate, wintime = 0.025, hoptime = 0.01, numcep = numberOfCoefs)  
  # Calcular a media de cada coeficiente
  for (j in 1:numberOfCoefs) {
    x[j] = sum(MFCCtemp[,j])/length(MFCCtemp[,j])
  }
  murmorMFCC[[i]] = x
}


normalSTFTtrain = list()
normalMFCCtrain = list()
murmorSTFTtrain = list()
murmorMFCCtrain = list()
normalSTFTtest = list()
normalMFCCtest = list()
murmorSTFTtest = list()
murmorMFCCtest = list()
# Guardar apenas os coeficientes normais de treino
for (i in trainNormalIndex) {
  normalSTFTtrain[[length(normalSTFTtrain)+1]] = normalSTFT[[i]]
  normalMFCCtrain[[length(normalMFCCtrain)+1]] = normalMFCC[[i]]
}
# Guardar apenas os coeficientes com murmurios de treino
for (i in trainMurmorIndex) {
  murmorSTFTtrain[[length(murmorSTFTtrain)+1]] = murmorSTFT[[i]]
  murmorMFCCtrain[[length(murmorMFCCtrain)+1]] = murmorMFCC[[i]]
}
# Guardar apenas os coeficientes normais de teste
for (i in testNormalIndex) {
  normalSTFTtest[[length(normalSTFTtest)+1]] = normalSTFT[[i]]
  normalMFCCtest[[length(normalMFCCtest)+1]] = normalMFCC[[i]]
}
# Guardar apenas os coeficientes com murmurios de teste
for (i in testMurmorIndex) {
  murmorSTFTtest[[length(murmorSTFTtest)+1]] = murmorSTFT[[i]]
  murmorMFCCtest[[length(murmorMFCCtest)+1]] = murmorMFCC[[i]]
}

trainSTFT = c(normalSTFTtrain, murmorSTFTtrain)
testSTFT = c(normalSTFTtest, murmorSTFTtest)
trainMFCC = c(normalMFCCtrain, murmorMFCCtrain)
testMFCC = c(normalMFCCtest, murmorMFCCtest)
train = matrix(data = 0, nrow = length(trainSTFT), ncol = numberOfCoefs*2) # COM MFCC E STFT
test = matrix(data = 0, nrow = length(testSTFT), ncol = numberOfCoefs*2) # COM MFCC E STFT
# Juntar na mesma linha os coeficientes de treino correspondentes ao mesmo sinal
for (i in 1:length(trainSTFT)) {
  train[i,] = c (trainSTFT[[i]], trainMFCC[[i]] )
}
# Juntar na mesma linha os coeficientes de teste correspondentes ao mesmo sinal
for (i in 1:length(testMFCC)) {
  test[i,] = c (testSTFT[[i]], testMFCC[[i]] )
}

# Etiquesta correspondente a cada ficheiro de treino
classTrain = c ( rep("Normal", length(normalSTFTtrain)) , rep("Doente", length(murmorSTFTtrain)))

# Guardar o ficheiro do workspace na pasta deste ficheiro
savePath = dirname(rstudioapi::getSourceEditorContext()$path) # Pasta PROEST
savePath = paste(savePath, "/workspace_classification.RData", sep="")
savePath = gsub("/", "\\\\", savePath)
save.image(file=savePath)

# Etiqueta correta dos ficheiros de teste
classeCorreta= c ( rep("Normal", length(normalSTFTtest)) , rep("Doente", length(murmorSTFTtest)))
# Etiqueta prevista pelo classificador
# nrow(train) = numero de amostras de treino
classePrevista = knn(train, test, classTrain, k = sqrt(nrow(train)))

# MATRIZ, 2x2, linhas correspondem ao valor correto, colunas ao valor previsto
testMatrix = matrix(data = 0, nrow = 2, ncol = 2)
rownames(testMatrix) = c ("Doente", "Normal")
colnames(testMatrix) = c ("Doente", "Normal")
for (i in 1:length(classeCorreta)) {
  testMatrix[classeCorreta[i],classePrevista[i]] = testMatrix[classeCorreta[i],classePrevista[i]]+1
}
print(testMatrix)
# Встановіть необхідну бібліотеку, якщо її ще не встановлено
if (!require("MCMCpack")) {
  install.packages("MCMCpack")
  library(MCMCpack)
}

# Функція для генерації розподілу Діріхле
generateDirichletDistribution <- function(alphabetPower, alpha) {
  distribution <- rdirichlet(1, alpha)
  return(distribution)
}

# Функція для сортування масиву
sortArray <- function(array) {
  return(sort(array))
}

# Функція для отримання індексу інтервалу для значення
getIntervalIndex <- function(value, intervals) {
  return(max(which(intervals <= value)))
}

# Функція для визначення інтервалів
defineIntervals <- function(distribution) {
  intervals <- cumsum(c(0, distribution))
  return(intervals)
}

# Функція для перетворення числової послідовності в лінгвістичну послідовність
convertToLinguisticSeries <- function(numericSeries, intervals) {
  n <- length(numericSeries)
  linguisticSeries <- character(n)
  for (i in 1:n) {
    index <- getIntervalIndex(numericSeries[i], intervals)
    linguisticSeries[i] <- LETTERS[index]
  }
  return(linguisticSeries)
}

# Функція для виведення лінгвістичної послідовності
printLinguisticSeries <- function(linguisticSeries) {
  cat("Лінгвістичний ряд: ", paste(linguisticSeries, collapse = " "), "\n")
}

# Функція для побудови матриці переходів
buildTransitionMatrix <- function(linguisticSeries, alphabetPower) {
  matrixSize <- alphabetPower
  transitionMatrix <- matrix(0, nrow = matrixSize, ncol = matrixSize)
  symbols <- LETTERS[1:alphabetPower]
  
  for (i in 1:(length(linguisticSeries) - 1)) {
    currentSymbol <- linguisticSeries[i]
    nextSymbol <- linguisticSeries[i + 1]
    rowIndex <- which(symbols == currentSymbol)
    colIndex <- which(symbols == nextSymbol)
    transitionMatrix[rowIndex, colIndex] <- transitionMatrix[rowIndex, colIndex] + 1
  }
  
  return(transitionMatrix)
}

# Функція для виведення матриці передування
printTransitionMatrix <- function(transitionMatrix) {
  cat("Матриця переходів:\n")
  print(transitionMatrix)
}

# Головна функція
main <- function() {
  file_path <- "/Users/nazarodemchuk/Desktop/column_D.csv" # Шлях до файлу з числовою послідовністю
  numericSeries <- read.csv(file_path, header = FALSE)$V1 # Читання числової послідовності з файлу
  n <- length(numericSeries) # Кількість елементів у числовій послідовності
  alphabetPower <- 26 # Потужність алфавіту
  alpha <- rep(1, alphabetPower) # Параметри розподілу Діріхле
  
  # Генеруємо розподіл Діріхле
  distribution <- generateDirichletDistribution(alphabetPower, alpha)
  
  # Визначаємо інтервали
  intervals <- defineIntervals(distribution)
  
  # Перетворюємо числову послідовність в лінгвістичну послідовність
  sortedNumericSeries <- sortArray(numericSeries)
  minVal <- min(sortedNumericSeries)
  maxVal <- max(sortedNumericSeries)
  normalizedNumericSeries <- (sortedNumericSeries - minVal) / (maxVal - minVal)
  linguisticSeries <- convertToLinguisticSeries(normalizedNumericSeries, intervals)
  
  # Виводимо лінгвістичну послідовність
  printLinguisticSeries(linguisticSeries)
  
  # Будуємо і виводимо матрицю передування
  transitionMatrix <- buildTransitionMatrix(linguisticSeries, alphabetPower)
  printTransitionMatrix(transitionMatrix)
}

# Запускаємо головну функцію
main()


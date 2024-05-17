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
defineIntervals <- function(distribution, alphabetPower) {
  intervals <- numeric(alphabetPower + 1)
  intervals[1] <- 0.0
  for (i in 2:(alphabetPower + 1)) {
    intervals[i] <- intervals[i - 1] + distribution[i - 1]
  }
  return(intervals)
}

# Функція для перетворення числової послідовності в лінгвістичну послідовність
convertToLinguisticSeries <- function(numericSeries, intervals, alphabetPower) {
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
  n <- 20 # Кількість елементів у числовій послідовності
  numericSeries <- c(0.5, 0.9, 2.1, 0.3, 3.4, 1.5, 1.9, 1.1, 1.3,
                     1.4, 2.5, 2.9, 3.1, 4.3, 4.4, 5.5, 5.9, 5.1, 5.3, 5.4) # Числова послідовність
  alphabetPower <- 8 # Потужність алфавіту
  alpha <- rep(1, alphabetPower) # Параметри розподілу Діріхле
  
  # Генеруємо розподіл Діріхле
  distribution <- generateDirichletDistribution(alphabetPower, alpha)
  
  # Сортуємо числову послідовність
  sortedNumericSeries <- sortArray(numericSeries)
  
  # Визначаємо інтервали
  intervals <- defineIntervals(distribution, alphabetPower-1)
  
  # Перетворюємо числову послідовність в лінгвістичну послідовність
  linguisticSeries <- convertToLinguisticSeries(sortedNumericSeries, intervals, alphabetPower-1)
  
  # Виводимо лінгвістичну послідовність
  printLinguisticSeries(linguisticSeries)
  
  # Будуємо і виводимо матрицю передування
  transitionMatrix <- buildTransitionMatrix(linguisticSeries, alphabetPower)
  printTransitionMatrix(transitionMatrix)
}

# Запускаємо головну функцію
main()


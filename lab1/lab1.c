#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

// Функція для генерації розподілу Діріхле
void generateDirichletDistribution(double *distribution, int alphabetPower, double *alpha) {
    double sum = 0.0;
    for (int i = 0; i < alphabetPower; i++) {
        double x = rand() / (RAND_MAX + 1.0);
        distribution[i] = pow(x, alpha[i]);
        sum += distribution[i];
    }
    for (int i = 0; i < alphabetPower; i++) {
        distribution[i] /= sum;
    }
}

// Функція для сортування масиву
void sortArray(double *array, int size) {
    for (int i = 0; i < size - 1; i++) {
        for (int j = 0; j < size - i - 1; j++) {
            if (array[j] > array[j + 1]) {
                double temp = array[j];
                array[j] = array[j + 1];
                array[j + 1] = temp;
            }
        }
    }
}

// Функція для отримання індексу інтервалу за значенням
int getIntervalIndex(double value, double *intervals, int intervalsCount) {
    for (int i = 0; i < intervalsCount; i++) {
        if (value <= intervals[i]) {
            return i;
        }
    }
    return intervalsCount - 1;
}
// Функція для визначення інтервалів
void defineIntervals(double *intervals, double *distribution, int alphabetPower) {
    intervals[0] = 0.0;
    for (int i = 1; i <= alphabetPower; i++) {
        intervals[i] = intervals[i - 1] + distribution[i - 1];
    }
}

// Функція для перетворення чисельного ряду у лінгвістичний
void convertToLinguisticSeries(char *linguisticSeries, double *numericSeries, double *intervals, int n, int alphabetPower) {
    for (int i = 0; i < n; i++) {
        int index = getIntervalIndex(numericSeries[i], intervals, alphabetPower);
        linguisticSeries[i] = 'A' + index;
    }
}

// Функція для виводу лінгвістичного ряду
void printLinguisticSeries(char *linguisticSeries, int n) {
    printf("Лінгвістичний ряд: ");
    for (int i = 0; i < n; i++) {
        printf("%c ", linguisticSeries[i]);
    }
    printf("\n");
}

// Функція для будування матриці передування
void buildTransitionMatrix(int **transitionMatrix, char *linguisticSeries, int n, int alphabetPower) {
    for (int i = 0; i < alphabetPower; i++) {
        for (int j = 0; j < alphabetPower; j++) {
            transitionMatrix[i][j] = 0;
        }
    }

    for (int i = 0; i < n - 1; i++) {
        int current = linguisticSeries[i] - 'A';
        int next = linguisticSeries[i + 1] - 'A';
        transitionMatrix[current][next]++;
    }
}

// Функція для виводу матриці передування
void printTransitionMatrix(int **transitionMatrix, int alphabetPower) {
    printf("Матриця передування:\n");
    for (int i = 0; i < alphabetPower; i++) {
        for (int j = 0; j < alphabetPower; j++) {
            printf("%d ", transitionMatrix[i][j]);
        }
        printf("\n");
    }
}

// Головна функція
int main() {
    srand(time(NULL));
    int n = 10; // Кількість елементів у чисельному ряду
    double numericSeries[] = {0.5, 0.9, 0.1, 0.3, 0.4, 0.6, 0.7, 0.2, 0.8, 0.0}; // Чисельний ряд
    int alphabetPower = 5; // Потужність алфавіту
    double alpha[] = {1, 1, 1, 1, 1}; // Параметри для розподілу Діріхле
    double distribution[alphabetPower]; // Розподіл ймовірностей
    char linguisticSeries[n]; // Лінгвістичний ряд
    double intervals[alphabetPower + 1]; // Інтервали
    int **transitionMatrix; // Матриця передування

    // Ініціалізація матриці передування
    transitionMatrix = (int **)malloc(alphabetPower * sizeof(int *));
    for (int i = 0; i < alphabetPower; i++) {
        transitionMatrix[i] = (int *)malloc(alphabetPower * sizeof(int));
    }

    // Генеруємо розподіл Діріхле
    generateDirichletDistribution(distribution, alphabetPower, alpha);

    // Сортуємо чисельний ряд
    sortArray(numericSeries, n);

    // Визначаємо інтервали
    defineIntervals(intervals, distribution, alphabetPower);

    // Перетворюємо чисельний ряд у лінгвістичний
    convertToLinguisticSeries(linguisticSeries, numericSeries, intervals, n, alphabetPower);

    // Виводимо лінгвістичний ряд
    printLinguisticSeries(linguisticSeries, n);

    // Будуємо матрицю передування
    buildTransitionMatrix(transitionMatrix, linguisticSeries, n, alphabetPower);

    // Виводимо матрицю передування
    printTransitionMatrix(transitionMatrix, alphabetPower);

    // Очищення пам'яті
    for (int i = 0; i < alphabetPower; i++) {
        free(transitionMatrix[i]);
    }
    free(transitionMatrix);

    return 0;
}
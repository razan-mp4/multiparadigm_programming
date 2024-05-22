#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

// Функція для зчитування числового ряду з файлу
int readNumericSeriesFromFile(const char *filename, double **numericSeries) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        return -1;
    }

    int capacity = 10;
    int size = 0;
    *numericSeries = (double *)malloc(capacity * sizeof(double));

    double value;
    while (fscanf(file, "%lf", &value) != EOF) {
        if (size >= capacity) {
            capacity *= 2;
            *numericSeries = (double *)realloc(*numericSeries, capacity * sizeof(double));
        }
        (*numericSeries)[size++] = value;
    }

    fclose(file);
    return size;
}

// Функція для знаходження мінімального та максимального значень в масиві
void findMinMax(double *array, int size, double *min, double *max) {
    *min = array[0];
    *max = array[0];
    for (int i = 1; i < size; i++) {
        if (array[i] < *min) {
            *min = array[i];
        }
        if (array[i] > *max) {
            *max = array[i];
        }
    }
}

// Функція для визначення інтервалів на основі мінімальних та максимальних значень
void defineUniformIntervals(double *intervals, double min, double max, int alphabetPower) {
    double step = (max - min) / alphabetPower;
    for (int i = 0; i <= alphabetPower; i++) {
        intervals[i] = min + i * step;
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
    for (int i = 1; i <= intervalsCount; i++) {
        if (value < intervals[i]) {
            return i - 1;
        }
    }
    return intervalsCount - 1;
}

// Функція для перетворення числового ряду у лінгвістичний
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

// Функція для побудови матриці передування
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

    const char *filename = "/Users/nazarodemchuk/Desktop/l/column_D.csv"; // Оновіть шлях до файлу
    double *fileNumericSeries;
    int fileSize = readNumericSeriesFromFile(filename, &fileNumericSeries);

    if (fileSize < 0) {
        return 1;
    }

    double initialNumericSeries[] = {0.5, 0.9, 0.1, 0.3, 0.4, 0.6, 0.7, 0.2, 0.8, 0.0};
    int initialSize = sizeof(initialNumericSeries) / sizeof(initialNumericSeries[0]);

    int totalSize = initialSize + fileSize;
    double *numericSeries = (double *)malloc(totalSize * sizeof(double));

    memcpy(numericSeries, initialNumericSeries, initialSize * sizeof(double));
    memcpy(numericSeries + initialSize, fileNumericSeries, fileSize * sizeof(double));

    double min, max;
    findMinMax(numericSeries, totalSize, &min, &max);

    int alphabetPower = 26;
    double intervals[alphabetPower + 1];
    char linguisticSeries[totalSize];
    int **transitionMatrix;

    transitionMatrix = (int **)malloc(alphabetPower * sizeof(int *));
    for (int i = 0; i < alphabetPower; i++) {
        transitionMatrix[i] = (int *)malloc(alphabetPower * sizeof(int));
    }

    defineUniformIntervals(intervals, min, max, alphabetPower);
    sortArray(numericSeries, totalSize);
    convertToLinguisticSeries(linguisticSeries, numericSeries, intervals, totalSize, alphabetPower);
    printLinguisticSeries(linguisticSeries, totalSize);
    buildTransitionMatrix(transitionMatrix, linguisticSeries, totalSize, alphabetPower);
    printTransitionMatrix(transitionMatrix, alphabetPower);

    for (int i = 0; i < alphabetPower; i++) {
        free(transitionMatrix[i]);
    }
    free(transitionMatrix);
    free(fileNumericSeries);
    free(numericSeries);

    return 0;
}

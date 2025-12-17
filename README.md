### Examples

#### Fibonacci

```
import printf;

long fibRecursive(int n) {
    if (n <= 1) {
        return long(n);
    }
    return fibRecursive(n - 1) + fibRecursive(n - 2);
}

long fibIterative(int n) {
    long prev;
    long curr;
    long next;
    int i;
    
    if (n <= 1) {
        return long(n);
    }
    
    prev = 0L;
    curr = 1L;
    
    for (i = 2; i <= n; i++) {
        next = prev + curr;
        prev = curr;
        curr = next;
    }
    
    return curr;
}

long fibMemoized(int n) {
    long[50] memo;
    int i;
    
    // Initialize memo array
    memo[0] = 0L;
    memo[1] = 1L;
    
    for (i = 2; i <= n; i++) {
        memo[i] = memo[i - 1] + memo[i - 2];
    }
    
    return memo[n];
}

void printFibonacciSequence(int count) {
    int i;
    long fib;
    
    printf("First %d Fibonacci numbers:\n", count);
    for (i = 0; i < count; i++) {
        fib = fibIterative(i);
        printf("F(%d) = %ld\n", i, fib);
    }
}

void main() {
    int n;
    long result;
    
    n = 20;
    
    printf("Computing Fibonacci numbers up to F(%d)\n\n", n);
    
    // Compare methods for smaller values
    printf("Recursive: F(10) = %ld\n", fibRecursive(10));
    printf("Iterative: F(10) = %ld\n", fibIterative(10));
    printf("Memoized:  F(10) = %ld\n", fibMemoized(10));
    
    printf("\n");
    
    // Print sequence
    printFibonacciSequence(15);
    
    printf("\n");
}
```

### BubbleSort
```
import printf;

int[1000] numbers;

void swap(int i, int j) {
    int temp;
    temp = numbers[i];
    numbers[i] = numbers[j];
    numbers[j] = temp;
}

void bubbleSort() {
    int i;
    int j;
    int n;
    bool swapped;
    
    n = len(numbers);
    
    for (i = 0; i < n - 1; i++) {
        swapped = false;
        
        for (j = 0; j < n - i - 1; j++) {
            if (numbers[j] > numbers[j + 1]) {
                swap(j, j + 1);
                swapped = true;
            }
        }
        
        // If no swaps were made, array is sorted
        if (!swapped) {
            break;
        }
    }
}

void printArray() {
    int i;
    printf("Array: ");
    for (i = 0; i < len(numbers); i++) {
        printf("%d ", numbers[i]);
    }
    printf("\n");
}

void main() {
    // Initialize array with unsorted values
    numbers[0] = 64;
    numbers[1] = 34;
    numbers[2] = 25;
    numbers[3] = 12;
    numbers[4] = 22;
    numbers[5] = 11;
    numbers[6] = 90;
    numbers[7] = 88;
    numbers[8] = 45;
    numbers[9] = 50;
    
    printf("Before sorting:\n");
    printArray();
    
    bubbleSort();
    
    printf("After sorting:\n");
    printArray();
}
```

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> // might have to delete these
#include <pthread.h>
#include <math.h>
#include <time.h>

#define bool int
#define TRUE 1
#define FALSE 0

#define PORT_NUMBER 60000
#define MAX_SAMPLES 10000
#define MAX_FEATURES 100
#define STRING_BUFFER_LIMIT 100
#define PREPOC_THREAD_LIMIT 128
#define COEFF_THREAD_LIMIT 128
// READ CSV--------------------------------------------------------------------

typedef struct
{
    char **values;  // array of strings for row values
    int num_fields; // number of fields in the row
} Row;

char **getFieldNames(char *line, int *num_fields)
{

    int field_no = 0;
    int max_fields = MAX_FEATURES;
    char **field_names = malloc(max_fields * sizeof(char *));
    if (!field_names)
    {
        perror("malloc");
        exit(1);
    }

    char *token = strtok(line, ",\n");
    while (token != NULL)
    {

        field_names[field_no++] = strdup(token);

        if (field_no >= max_fields)
        {
            perror("MAXIMUM FIELDS HAVE BEEN EXCEEDED");
            exit(1);
        }

        token = strtok(NULL, ",\n");
    }

    *num_fields = field_no;

    return field_names;
}

Row readRow(char *line, int number_of_fields)
{

    Row row;
    int field_no = 0;
    row.num_fields = number_of_fields;

    row.values = malloc(number_of_fields * sizeof(char *));
    if (!row.values)
    {
        perror("malloc");
        exit(1);
    }

    char *token = strtok(line, ",\n");
    while (token != NULL)
    {

        row.values[field_no] = strdup(token);
        field_no++;
        token = strtok(NULL, ",\n");
    }

    if (field_no != number_of_fields)
    {

        // what are we supposed to do with missing values?
    }

    return row;
}

Row *getTable(FILE *stream, int *row_count, char ***headers)
{

    char line[1024]; // is this supposed to be the string buffer limit or is that supposed to be for single values?
    int number_of_fields = 0;
    *row_count = 0;

    int capacity = MAX_SAMPLES;
    Row *table = malloc(capacity * sizeof(Row));
    if (!table)
    {
        perror("malloc");
        exit(1);
    }

    if (fgets(line, sizeof(line), stream))
    {
        line[strcspn(line, "\r\n")] = 0;

        *headers = getFieldNames(line, &number_of_fields); // the first row is attribute names
        // we need to pass number of fields' addres so we can manipulate it
    }

    while (fgets(line, sizeof(line), stream))
    {
        line[strcspn(line, "\r\n")] = 0;

        Row row = readRow(line, number_of_fields); // get each row from the table

        if (*row_count >= capacity)
        {
            perror("MAXIMUM NUMBER OF ROWS HAVE BEEN EXCEEDED");
            exit(1);
        }

        table[(*row_count)++] = row;
    }

    return table;
}
//--------------------------------------------------------------------------------

// normalization-----------------------------------------------------------------------

typedef struct
{
    char **catNames;  // array of strings for the unnormalized values of categorical attributes
    double *numValue; // array of normalized values for an attribute
    bool is_num;
    double x_min;
    double x_max;
} norm;

typedef struct // this is necessary for multithreadding
{
    char **column_values;
    int rowcount;
} preproc_thread_arg;

#include <ctype.h>
#include <stdlib.h>

int is_num(const char *str)
{
    if (!str)
        return 0;

    // Skip leading whitespace
    while (isspace((unsigned char)*str))
        str++;

    if (*str == '\0')
        return 0; // empty or only spaces

    char *endptr;
    strtod(str, &endptr); // convert the string into a double. If the values are the same its not numeric.

    if (endptr == str)
        return 0; // no conversion happened

    // Skip trailing whitespace
    while (isspace((unsigned char)*endptr))
        endptr++;

    return *endptr == '\0'; // must end exactly
}

void *normalize_num(void *arg_ptr)
{
    preproc_thread_arg *arg = arg_ptr; // struct for thread arguments
    char **values = arg->column_values;
    int row_count = arg->rowcount;

    norm *Norm = malloc(sizeof(norm));
    Norm->is_num = TRUE;
    Norm->x_max = -__INT32_MAX__;
    Norm->x_min = __INT32_MAX__;
    Norm->numValue = malloc(row_count * sizeof(double));
    double value;
    double *values_d = malloc(row_count * sizeof(double));
    if (!values_d)
    {
        perror("malloc");
        exit(1);
    }

    if (!Norm->numValue)
    {
        perror("malloc");
        exit(1);
    }

    for (int i = 0; i < row_count; i++)
    {
        sscanf(values[i], "%lf", &value);
        values_d[i] = value;
        if (value > Norm->x_max) // check min-max
        {
            Norm->x_max = value;
        }
        if (value < Norm->x_min)
        {
            Norm->x_min = value;
        }
    }

    for (int i = 0; i < row_count; i++)
    {
        double denom = (Norm->x_max - Norm->x_min);
        if (denom == 0) // if x_min = x_max
        {
            Norm->numValue[i] = 0; // Just give 0 to all ofthem Idk what else to do
        }
        else
        {
            Norm->numValue[i] = (values_d[i] - Norm->x_min) / denom; // min-max normalization
        }
    }


    free(values_d);
    return Norm;
}

void *normalize_cat(void *arg_ptr)
{
    preproc_thread_arg *arg = arg_ptr; // arguments for mulitethreading
    char **values = arg->column_values;
    int row_count = arg->rowcount;
    int last_ind = 0;
    norm *Norm = malloc(sizeof(norm));
    Norm->is_num = FALSE;
    Norm->catNames = calloc(row_count, sizeof(char *));
    Norm->numValue = malloc(row_count * sizeof(double));
    if (!Norm->catNames)
    {
        perror("malloc");
        exit(1);
    }
    if (!Norm->numValue)
    {
        perror("malloc");
        exit(1);
    }
    int indexVal = 0;
    int number_of_cats = 0;
    for (int i = 0; i < row_count; i++)
    {
        if (strcmp(values[i], "semi-furnished") == 0) // special rule
        {
            Norm->numValue[i] = 0.5;

            continue;
        }
        else if (strcmp(values[i], "no") == 0 || strcmp(values[i], "unfurnished") == 0)
        {
            Norm->numValue[i] = 0;
            continue;
        }
        else if (strcmp(values[i], "furnished") == 0 || strcmp(values[i], "yes") == 0 )
        {
            Norm->numValue[i] = 1;
            continue;
        }

        bool found = FALSE;
        for (int j = 0; j < number_of_cats; j++)
        {
            if (strcmp(Norm->catNames[j], values[i]) == 0) // if the value was already encountered
            {
                Norm->numValue[i] = j;
                found = TRUE;
                break;
            }
        }
        if (!found) // if there is no special rules for the values just number them according to the order they come
        {
            Norm->numValue[i] = indexVal;
            Norm->catNames[indexVal] = strdup(values[i]);
            last_ind = indexVal;

            number_of_cats++;
            indexVal++;
        }
    }

    Norm->x_min = 0.0;
    Norm->x_max = last_ind;
    if (Norm->x_max == 0)
    {
        return Norm;
    }
    for (int i = 0; i < row_count; i++)
    {
        Norm->numValue[i] /= Norm->x_max;
    }
    return Norm;
}

norm *getNormTable(int number_of_fields, Row *table, int row_count, norm *target_norm)
{
    norm *normalTable = malloc((number_of_fields) * sizeof(norm));
    if (!normalTable)
    {
        perror("malloc");
        exit(1);
    }
    normalTable[0].is_num = TRUE;
    normalTable[0].numValue = malloc(row_count * sizeof(double));
    if (!normalTable[0].numValue)
    {
        perror("malloc");
        exit(1);
    }

    for (int i = 0; i < row_count; i++)
    {
        normalTable[0].numValue[i] = 1.0;
    }

    pthread_t attribute_threads[PREPOC_THREAD_LIMIT];
    preproc_thread_arg *args[number_of_fields];
    for (int i = 0; i < number_of_fields; i++) // multithreading babyyyy
    {
        norm norm;
        int max = 0;
        int min = __INT_MAX__;
        char **column_values = malloc(row_count * sizeof(char *)); // gotta iterate over the attributes so roll down the table
        // there is probably a more efficient way to do this
        for (int row = 0; row < row_count; row++)
        {
            column_values[row] = table[row].values[i];
        }
        bool num_flag = is_num(table[0].values[i]);

        args[i] = malloc(sizeof(preproc_thread_arg));
        args[i]->rowcount = row_count;
        args[i]->column_values = column_values;
        preproc_thread_arg arg = {column_values, row_count};
        if (num_flag) // chec if the attribute is numerical
        {
            pthread_create(&(attribute_threads[i]), NULL, normalize_num, (void *)args[i]);
        }
        else
        {
            pthread_create(&(attribute_threads[i]), NULL, normalize_cat, (void *)args[i]);
        }
    }
    for (int i = 0; i < number_of_fields; i++)
    {
        norm *result;
        if (pthread_join(attribute_threads[i], (void **)&result) != 0) // pthread_create returns wheter the create was actually created
        // we write the actual values into the result variable.
        {
            perror("pthread_join");
            exit(1);
        }

        if (i == number_of_fields - 1)
        {
            *target_norm = *result; // assign the actual norm pointer
        }
        else
        {
            normalTable[i+1] = *result;
        }

        free(args[i]->column_values);
        free(args[i]);
    }

    fflush(stdout);
    return normalTable;
}
//-----------------------------------------------------------------------

// garbage collection------------------------------------------------------

// this sections is made up of free functions for our various matrices.
// Since some of them are 2 dimensionall arrays (i.e. pointers of pointers)
// and others structs that have arrays in them, we can't just free them with the free() function.
void freeTable(Row *table, int major_index)
{
    for (int r = 0; r < major_index; r++)
    {
        for (int c = 0; c < table[r].num_fields; c++)
        {
            free(table[r].values[c]);
        }
        free(table[r].values);
    }
    free(table);
}

void freeHeaders(char **headers, int index)
{
    for (int i = 0; i < index; i++)
    {
        free(headers[i]);
    }
    free(headers);
}

void freeTargetNorm(norm target_norm, int row_count)
{
    if (target_norm.is_num)
    {
        free(target_norm.numValue);
    }
    else
    {
        for (int i = 0; i < row_count; i++)
        {
            if (target_norm.catNames[i])
                free(target_norm.catNames[i]);
        }
        free(target_norm.catNames);
        free(target_norm.numValue);
    }
}

void freeNorm(norm *n, int index)
{

    if (n->is_num)
    {
        free(n->numValue);
    }
    else
    {
        for (int i = 0; i < index; i++)
        {
            if (n->catNames[i])
                free(n->catNames[i]);
        }
        free(n->catNames);
        free(n->numValue);
    }
}

void freeNormTable(norm *normalTable, int major_index, int index)
{

    for (int i = 0; i < major_index; i++)
    {
        freeNorm(&normalTable[i], index);
    }
    free(normalTable);
}

//---------------------------------------------------------------------------------

// matrix calculations--------------------------------------------------------------

// created 3 different matrix multipllications so that we didn't need to just turn all of them into 2d double arrays
// it wasn't worth it
// they all work the same way, iterate over the rows annd columns and summ all of them and put them in their place in the new matrix yada yada

typedef struct Matrix {
    double **data;
    int rows;
    int cols;
} Matrix;

Matrix *createMatrix(int rows, int cols)
{
    Matrix *matrix = malloc(sizeof(struct Matrix));
    matrix->data = malloc(rows * sizeof(double *));
    for (int i = 0; i < rows; i++) {
        matrix->data[i] = malloc(cols * sizeof(double));
        memset(matrix->data[i], 0, cols * sizeof(double));
    }
    matrix->rows = rows;
    matrix->cols = cols;
    return matrix;
}

void matrix_free(Matrix *m)
{
    if (!m) return;
    for (int i = 0; i < m->rows; i++)
        free(m->data[i]);
    free(m->data);
    free(m);
}

Matrix* solveLinearSystem(Matrix* A, Matrix* B) {
    int n = A->rows;

    Matrix *aug = createMatrix(n, n + 1);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++)
            aug->data[i][j] = A->data[i][j];
        aug->data[i][n] = B->data[i][0];
    }

    for (int k = 0; k < n; k++) {
        int pivot = k;
        double max = fabs(aug->data[k][k]);
        for (int i = k + 1; i < n; i++) {
            double v = fabs(aug->data[i][k]);
            if (v > max) {
                max = v;
                pivot = i;
            }
        }

        if (max == 0.0) {
            return NULL;
        }

        if (pivot != k) {
            double *tmp = aug->data[k];
            aug->data[k] = aug->data[pivot];
            aug->data[pivot] = tmp;
        }

        for (int i = k + 1; i < n; i++) {
            double factor = aug->data[i][k] / aug->data[k][k];
            for (int j = k; j <= n; j++)
                aug->data[i][j] -= factor * aug->data[k][j];
        }
    }

    Matrix *x = createMatrix(n, 1);
    for (int i = n - 1; i >= 0; i--) {
        double sum = aug->data[i][n];
        for (int j = i + 1; j < n; j++)
            sum -= aug->data[i][j] * x->data[j][0];

        x->data[i][0] = sum / aug->data[i][i];
    }

    matrix_free(aug);
    return x;
}

Matrix *getMatrix(norm *table, int rows, int cols) {
    Matrix* result = createMatrix(rows, cols);

    for (int c = 0; c < cols; c++) {
        for (int r = 0; r < rows; r++) {
            result->data[r][c] = table[c].numValue[r];
        }
    }

    return result;
}

Matrix *getVector(norm vector, int rows) {
    Matrix* result = createMatrix(rows, 1);

    for (int r = 0; r < rows; r++) {
        result->data[r][0] = vector.numValue[r];
    }

    return result;
}

//---------------------------------------------------------------------------------
// Matrix calculations all multi threaded as much as possible

typedef struct {
    const Matrix *X;
    const Matrix *Y;
    int row_start;
    int row_end;
    Matrix *XTX_local;
    Matrix *XTY_local;
} NormalEqTask;

void* normal_eq_worker(void *arg) {
    NormalEqTask *t = (NormalEqTask*)arg;
    int d = t->X->cols;

    for (int i = t->row_start; i < t->row_end; i++) {
        double yi = t->Y->data[i][0];

        for (int j = 0; j < d; j++) {
            double xij = t->X->data[i][j];
            t->XTY_local->data[j][0] += xij * yi;

            for (int k = j; k < d; k++) {
                t->XTX_local->data[j][k] += xij * t->X->data[i][k];
            }
        }
    }
    return NULL;
}

Matrix* solveNormalEquation_parallel(
    const Matrix *X,
    const Matrix *Y,
    int num_threads
) {
    int n = X->rows;
    int d = X->cols;

    pthread_t threads[num_threads];
    NormalEqTask tasks[num_threads];

    Matrix *XTX = createMatrix(d, d);
    Matrix *XTY = createMatrix(d, 1);

    int rows_per_thread = n / num_threads;
    if (rows_per_thread == 0) {
        rows_per_thread = 1;
        num_threads = n;
    }

    for (int t = 0; t < num_threads; t++) {
        tasks[t].X = X;
        tasks[t].Y = Y;
        tasks[t].row_start = t * rows_per_thread;
        tasks[t].row_end =
            (t == num_threads - 1) ? n : (t + 1) * rows_per_thread;

        tasks[t].XTX_local = createMatrix(d, d);
        tasks[t].XTY_local = createMatrix(d, 1);

        pthread_create(&threads[t], NULL, normal_eq_worker, &tasks[t]);
    }

    for (int t = 0; t < num_threads; t++)
        pthread_join(threads[t], NULL);

    /* Reduction */
    for (int t = 0; t < num_threads; t++) {
        for (int i = 0; i < d; i++) {
            XTY->data[i][0] += tasks[t].XTY_local->data[i][0];
            for (int j = 0; j < d; j++) {
                XTX->data[i][j] += tasks[t].XTX_local->data[i][j];
            }
        }
        matrix_free(tasks[t].XTX_local);
        matrix_free(tasks[t].XTY_local);
    }

    for (int i = 0; i < d; i++) {
        for (int j = i + 1; j < d; j++) {
            XTX->data[j][i] = XTX->data[i][j];
        }
    }

    Matrix *B = solveLinearSystem(XTX, XTY);

    matrix_free(XTX);
    matrix_free(XTY);

    return B;
}

//---------------------------------------------------------------------------------

int main()
{

    FILE *stream = fopen("Housing.csv", "r"); // read the file
    if (!stream)
    {
        perror("fopen");
        return 1;
    }

    char **headers;
    int row_count;
    // we need to pass the adresses of row_count and headers so we can manipulate them inside the functi0n
    // since c does not support returning more than one value this is necessary
    Row *table = getTable(stream, &row_count, &headers);
    norm target_norm;

    norm *normalization_table = getNormTable(table->num_fields, table, row_count, &target_norm);
    Matrix* X = getMatrix(normalization_table, row_count, table->num_fields);
    Matrix* Y = getVector(target_norm, row_count);

    Matrix* B = solveNormalEquation_parallel(X, Y, COEFF_THREAD_LIMIT);

    freeNormTable(normalization_table, table->num_fields, row_count);
    freeHeaders(headers, table->num_fields);
    freeTargetNorm(target_norm, row_count);
    matrix_free(X);
    matrix_free(Y);
    matrix_free(B);
    freeTable(table, row_count);
}

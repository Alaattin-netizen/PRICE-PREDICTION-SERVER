#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> // might have to delete these
#include <pthread.h>

#define bool int
#define TRUE 1
#define FALSE 0

#define  PORT_NUMBER 60000
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
    int max_fields = 100;
    char **field_names = malloc(max_fields * sizeof(char*));
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

    *num_fields = field_no; // store number of fields

    return field_names;
}

Row readRow(char *line, int number_of_fields)
{

    Row row;
    int field_no = 0;
    row.num_fields = number_of_fields;
    // Allocate initial space for up to 100 fields

    row.values = malloc(number_of_fields * sizeof(char*));
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

        // Fill missing fields with empty strings to avoid garbage
        for (int k = field_no; k < number_of_fields; ++k)
        {
            row.values[k] = strdup("");
        }
    }

    return row;
}

Row *getTable(FILE *stream, int *row_count, char ***headers)
{
    char line[1024];
    int number_of_fields = 0;
    *row_count = 0;

    int capacity = 100; // initial row capacity
    Row *table = malloc(capacity * sizeof(Row));
    if (!table)
    {
        perror("malloc");
        exit(1);
    }

    if (fgets(line, sizeof(line), stream))
    {
        line[strcspn(line, "\r\n")] = 0;

        *headers = getFieldNames(line, &number_of_fields);
    }

    while (fgets(line, sizeof(line), stream))
    {
        line[strcspn(line, "\r\n")] = 0;

        Row row = readRow(line, number_of_fields);

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
    char **catNames;  // array of strings for row values
    double *numValue; // number of fields in the row
    bool is_num;
    int x_min;
    int x_max;
} norm;

typedef struct
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
    strtod(str, &endptr);

    if (endptr == str)
        return 0; // no conversion happened

    // Skip trailing whitespace
    while (isspace((unsigned char)*endptr))
        endptr++;

    return *endptr == '\0'; // must end exactly
}

void *normalize_num(void *arg_ptr)
{
    preproc_thread_arg *arg = arg_ptr;
    char **values = arg->column_values;
    int row_count = arg->rowcount;

    norm *Norm = malloc(sizeof(norm));   // return pointer
    Norm->is_num = TRUE;
    Norm->x_max = -__INT32_MAX__;
    Norm->x_min = __INT32_MAX__;
    Norm->numValue = malloc(row_count*sizeof(double));
    double value;
    double *values_d = malloc(row_count * sizeof(double));
    if (!values_d)
    {
        perror("malloc");
        exit(1);
    }
    Norm->numValue = malloc(row_count * sizeof(double));
    if (!Norm->numValue)
    {
        perror("malloc");
        exit(1);
    }
    for (int i = 0; i < row_count; i++)
    {
        sscanf(values[i], "%lf", &value);
        values_d[i] = value;
        if (value > Norm->x_max)
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
            Norm->numValue[i] = (values_d[i] - Norm->x_min) / denom;
        
    }

    free(values_d);
    return Norm;

   
}

void *normalize_cat(void* arg_ptr)
{
    preproc_thread_arg *arg = arg_ptr;
    char **values = arg->column_values;
    int row_count = arg->rowcount;

    norm *Norm = malloc(sizeof(norm));
    Norm->is_num = FALSE;
    Norm->catNames = calloc(row_count, sizeof(char*));
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

        if (strcmp(values[i], "yes") == 0 || strcmp(values[i], "semi-furnished") == 0) // special rule
        {
            Norm->numValue[i] = 1;
            continue;
        }
        else if (strcmp(values[i], "no") == 0 || strcmp(values[i], "unfurnished") == 0)
        {
            Norm->numValue[i] = 0;
            continue;
        }
        else if (strcmp(values[i], "furnished") == 0)
        {
            Norm->numValue[i] = 2;
            continue;
        }

        bool found = FALSE;
        for (int j = 0; j < number_of_cats; j++)
        {
            if (strcmp(Norm->catNames[j], values[i]) == 0)
            {
                Norm->numValue[i] = j;
                found = TRUE;
                break;
            }
        }
        if (1 - found)
        {
            Norm->numValue[i] = indexVal;
            Norm->catNames[indexVal] = strdup(values[i]);

            number_of_cats++;
            indexVal++;
        }
    }
    return Norm;

    
}

norm *getNormTable(int number_of_fields, Row *table, int row_count)
{
    norm *normalTable = malloc(number_of_fields * sizeof(norm));
    if (!normalTable)
    {
        perror("malloc");
        exit(1);
    }
    pthread_t attribute_threads[PREPOC_THREAD_LIMIT];
    preproc_thread_arg *args[number_of_fields];
    for (int i = 0; i < number_of_fields; i++)
    {
        norm norm;
        int max = 0;
        int min = __INT_MAX__;
        char **column_values = malloc(row_count * sizeof(char*));
        if (!column_values)
        {
            perror("malloc");
            exit(1);
        }
        for (int row = 0; row < row_count; row++)
        {
            column_values[row] = table[row].values[i];
        }
        bool num_flag = is_num(table[0].values[i]);

        args[i] = malloc(sizeof(preproc_thread_arg));
        args[i]->rowcount = row_count;
        args[i]->column_values = column_values;
        preproc_thread_arg arg = {column_values, row_count};

        if (num_flag)
        {
            pthread_create(&(attribute_threads[i]), NULL, normalize_num,(void*)args[i]);
        }
        else
        {
            pthread_create(&(attribute_threads[i]), NULL, normalize_cat,(void*)args[i]);
        }    
        
    }
    for (int i = 0; i < number_of_fields; i++)
        {
            norm *result;
            if (pthread_join(attribute_threads[i], (void**)&result) != 0) { perror("pthread_join"); exit(1); }
            normalTable[i] = *result;   
            free(result);
    
            free(args[i]->column_values);
            free(args[i]);
        }

    return normalTable;
}
//-----------------------------------------------------------------------

// garbage collection------------------------------------------------------
void freeTable(Row *table, int row_count)
{
    for (int r = 0; r < row_count; r++)
    {
        for (int c = 0; c < table[r].num_fields; c++)
        {
            free(table[r].values[c]);
        }
        free(table[r].values);
    }
    free(table);
}

void freeHeaders(char **headers, int num_fields)
{
    for (int i = 0; i < num_fields; i++)
    {
        free(headers[i]);
    }
    free(headers);
}

void freeNorm(norm *n, int row_count)
{
    if (n->is_num)
    {
        free(n->numValue);
    }
    else
    {
        for (int i = 0; i < row_count; i++)
        {
            if (n->catNames[i])
                free(n->catNames[i]);
        }
        free(n->catNames);
        free(n->numValue);
    }
}

void freeNormTable(norm *normalTable, int number_of_fields, int row_count)
{
    for (int i = 0; i < number_of_fields; i++)
    {
        freeNorm(&normalTable[i], row_count);
    }
    free(normalTable);
}

void freeMatrix(double** matrix, int field_count)
{
    for (int i = 0; i < field_count; i++)
    {
        free(matrix[i]);
    }
    free(matrix);
}
//---------------------------------------------------------------------------------

//matrix calculations--------------------------------------------------------------

double** getTansposeMatrix(norm* norm_table, int row_count, int field_count)
{
    double **transpose = malloc(field_count * sizeof(double*));

    for (int r = 0; r < field_count; r++)
        transpose[r] = malloc(row_count * sizeof(double));

    for (int r = 0; r < row_count; r++)
        for (int c = 0; c < field_count; c++)
            transpose[c][r] = norm_table[c].numValue[r];   

    return transpose;
}




//---------------------------------------------------------------------------------

int main()
{
    FILE *stream = fopen("Housing.csv", "r");
    if (!stream)
    {
        perror("fopen");
        return 1;
    }

    char **headers;
    int row_count;
    Row *table = getTable(stream, &row_count, &headers);

    norm *normalization_table = getNormTable(table->num_fields, table, row_count);
    double** transpose_matrix = getTansposeMatrix(normalization_table, row_count, table->num_fields);
    for (int i = 0; i < row_count; i++)
    {
        for (int j = 0; j < table->num_fields; j++)
        {
            printf(" %lf\t",normalization_table[j].numValue[i]);
        }
        printf("\n");
    }
    printf("\n \n \n \n");
    for (int i = 0; i < table->num_fields; i++)
    {
        for (int j = 0; j < row_count; j++)
        {
            printf(" %lf\t",transpose_matrix[i][j]);
        }
        printf("\n");
    }

    freeNormTable(normalization_table, table->num_fields,row_count);
    freeHeaders(headers, table->num_fields);
    freeMatrix(transpose_matrix, table->num_fields);
    freeTable(table, row_count);
    
}

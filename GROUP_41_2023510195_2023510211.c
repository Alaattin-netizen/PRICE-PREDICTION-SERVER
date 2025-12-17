#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h> // might have to delete these
#include <pthread.h>
#include <math.h>
#include <arpa/inet.h> // for inet_addr

#define bool int
#define TRUE 1
#define FALSE 0

#define MAX_SAMPLES 10000
#define MAX_FEATURES 100
#define STRING_BUFFER_LIMIT 100
#define PREPOC_THREAD_LIMIT 128
#define COEFF_THREAD_LIMIT 128

// Port-----------------------------------------------------------------------------
#include <sys/socket.h>
#include <arpa/inet.h> // for inet_addr
#include <netinet/in.h>
#include <unistd.h>
#define port_no 60000

//---------------------------------------------------------------------------------

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
        if (sizeof(token) > STRING_BUFFER_LIMIT * sizeof(char))
        {
            perror("MAXIMUM STRING LENGTH HAS BEEN EXCEEDED");
            exit(1);
        }
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
    int thread_index;
    int client_fd;
    char *col_name;
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

    char msgbuf[STRING_BUFFER_LIMIT];
    snprintf(msgbuf, sizeof(msgbuf),
             "[Thread N%d] Normalizing %s... xmin=%.2f xmax=%.2f\n",
             arg->thread_index,
             arg->col_name,
             Norm->x_min,
             Norm->x_max);

    send(arg->client_fd, msgbuf, strlen(msgbuf), 0);

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
    int last_ind;
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
        else if (strcmp(values[i], "furnished") == 0 || strcmp(values[i], "yes") == 0)
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
        if (1 - found) // if there is no special rules for the values just number them according to the order they come
        {
            Norm->numValue[i] = indexVal;
            Norm->catNames[indexVal] = strdup(values[i]);
            last_ind = indexVal;

            number_of_cats++;
            indexVal++;
        }
    }

    char msgbuf[STRING_BUFFER_LIMIT];
    snprintf(msgbuf, sizeof(msgbuf),
             "[Thread N%d] Normalizing %s\n",
             arg->thread_index,
             arg->col_name);

    send(arg->client_fd, msgbuf, strlen(msgbuf), 0);

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

norm *getNormTable(int number_of_fields, Row *table, int row_count, norm **target_norm, int client_fd, char **headers)
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
        args[i]->thread_index = i;
        args[i]->client_fd = client_fd;
        args[i]->col_name = headers[i];
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
            *target_norm = result; // assign the actual norm pointer
        }
        else
        {
            normalTable[i + 1] = *result;
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

void freeMatrix(double **matrix, int major_index)
{
    for (int i = 0; i < major_index; i++)
    {
        free(matrix[i]);
    }
    free(matrix);
}

//---------------------------------------------------------------------------------

// matrix calculations--------------------------------------------------------------

// created 3 different matrix multipllications so that we didn't need to just turn all of them into 2d double arrays
// it wasn't worth it
// they all work the same way, iterate over the rows annd columns and summ all of them and put them in their place in the new matrix yada yada

double **getTansposeMatrix(norm *norm_table, int row_count, int field_count)
{
    double **transpose = malloc(field_count * sizeof(double *));

    for (int r = 0; r < field_count; r++)
        transpose[r] = malloc(row_count * sizeof(double));

    for (int r = 0; r < row_count; r++)
    {

        for (int c = 0; c < field_count; c++)
        {
            transpose[c][r] = norm_table[c].numValue[r];
        }
    }

    return transpose;
}

double **matrixMultiplication_nm(double **trans_matrix, norm *norm_table, int field_count, int row_count)
{
    double **mutliplied_matrix = malloc(sizeof(double *) * field_count);
    for (int r = 0; r < field_count; r++)
    {
        mutliplied_matrix[r] = calloc(field_count, sizeof(double));
    }

    //  if(field_count*row_count*row_count <= COEFF_THREAD_LIMIT){
    for (int r = 0; r < field_count; r++)
    {
        for (int c = 0; c < field_count; c++)
        {
            for (int k = 0; k < row_count; k++)
                mutliplied_matrix[r][c] += trans_matrix[r][k] * norm_table[c].numValue[k];
        }
    }
    //  }

    return mutliplied_matrix;
}

double **matrixMultiplication_mn(double **matrix, norm *y_norm, int rows, int cols)
{
    double **result = malloc(rows * sizeof(double *));
    if (!result)
    {
        perror("malloc");
        exit(1);
    }
    for (int i = 0; i < rows; i++)
    {
        result[i] = calloc(1, sizeof(double));
        if (!result[i])
        {
            perror("calloc");
            exit(1);
        }
    }
    for (int i = 0; i < rows; i++)
    {
        for (int j = 0; j < cols; j++)
        {
            result[i][0] += matrix[i][j] * y_norm->numValue[j];
        }
    }
    return result;
}

double **matrixMultiplication_mm(double **M1, double **M2, int M1_rows, int M1_cols, int M2_cols)
{

    double **C = malloc(M1_rows * sizeof(double *));
    for (int i = 0; i < M1_rows; i++)
        C[i] = calloc(M2_cols, sizeof(double));

    for (int i = 0; i < M1_rows; i++)
    {
        for (int j = 0; j < M2_cols; j++)
        {
            for (int k = 0; k < M1_cols; k++)
            {
                C[i][j] += M1[i][k] * M2[k][j];
            }
        }
    }
    return C;
}

double **matrixInversion(double **matrix_org, int field_count)
{

    double **matrix = malloc(sizeof(double *) * field_count); // had to crate a clone matrix so the original one didnt get corrupted
    for (int r = 0; r < field_count; r++)
    {
        matrix[r] = malloc(field_count * sizeof(double));
        for (int c = 0; c < field_count; c++)
        {
            matrix[r][c] = matrix_org[r][c];
        }
    }
    double **id_matrix = malloc(sizeof(double *) * field_count);
    for (int r = 0; r < field_count; r++)
    {
        id_matrix[r] = calloc(field_count, sizeof(double)); // create identity matrix
        id_matrix[r][r] = 1;
    }

    // Using gaussian elemination.
    for (int r = 0; r < field_count; r++)
    {
        bool invertable = (matrix[r][r] != 0);

        if (1 - invertable)
        {
            for (int k = r + 1; k < field_count; k++)
            {
                if (matrix[k][r] != 0)
                {
                    invertable = TRUE;
                    double *tmp = matrix[r];
                    matrix[r] = matrix[k];
                    matrix[k] = tmp;

                    tmp = id_matrix[r];
                    id_matrix[r] = id_matrix[k];
                    id_matrix[k] = tmp;
                    break;
                }
            }
        }
        if (1 - invertable)
        {
            fprintf(stderr, "Matrix is singular or nearly singular!\n");
            exit(1);
        }
        for (int r2 = r + 1; r2 < field_count; r2++)
        {
            double mult = matrix[r2][r] / matrix[r][r];
            for (int c = 0; c < field_count; c++)
            {
                matrix[r2][c] -= matrix[r][c] * mult;
                id_matrix[r2][c] -= id_matrix[r][c] * mult;
            }
        }
    }

    // Back substitution
    for (int r = field_count - 1; r >= 0; r--)
    {
        for (int r2 = r - 1; r2 >= 0; r2--)
        {
            double mult = matrix[r2][r] / matrix[r][r];
            for (int c = field_count - 1; c >= 0; c--)
            {
                matrix[r2][c] -= matrix[r][c] * mult;
                id_matrix[r2][c] -= id_matrix[r][c] * mult;
            }
        }
    }

    // Divide rows by pivot
    for (int r = 0; r < field_count; r++)
    {
        double div = matrix[r][r];
        for (int c = 0; c < field_count; c++)
        {
            matrix[r][c] /= div;
            id_matrix[r][c] /= div;
        }
    }

    return id_matrix;
}

//---------------------------------------------------------------------------------

int main()
{
    int socket_desc, client_fd;
    struct sockaddr_in server_addr, client_addr;
    socklen_t addr_len = sizeof(client_addr);

    socket_desc = socket(AF_INET, SOCK_STREAM, 0);
    if (socket_desc < 0)
    {
        perror("socket");
        exit(1);
    }

    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(port_no);

    if (bind(socket_desc, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0)
    {
        perror("bind");
        exit(1);
    }

    if (listen(socket_desc, 1) < 0)
    {
        perror("listen");
        exit(1);
    }

    printf("Listening on port %d...\n", port_no);

    client_fd = accept(socket_desc, (struct sockaddr *)&client_addr, &addr_len);
    if (client_fd < 0)
    {
        perror("accept");
        exit(1);
    }

    char *message =
        "WELCOME TO PRICE PREDICTION SERVER\n"
        "Enter CSV file name:";

    send(client_fd, message, strlen(message), 0);

    char buffer[STRING_BUFFER_LIMIT];
    int n = recv(client_fd, buffer, sizeof(buffer) - 1, 0);
    if (n > 0)
    {
        buffer[n] = '\0';
        buffer[strcspn(buffer, "\r\n")] = '\0';
        printf("Client sent: '%s'\n", buffer);
    }

    message = "\n\nChecking dataset...\n";
    send(client_fd, message, strlen(message), 0);

    FILE *stream = fopen(buffer, "r"); // read the file
    if (!stream)
    {
        puts("File couldn't be found");
        perror("fopen");
        return 1;
    }

    char msgbuf[STRING_BUFFER_LIMIT];
    snprintf(msgbuf, sizeof(msgbuf),
             "\n\n[OK] File %s found. Reading file...\n",
             buffer);

    send(client_fd, msgbuf, strlen(msgbuf), 0);

    char **headers;
    int row_count;
    // we need to pass the adresses of row_count and headers so we can manipulate them inside the functi0n
    // since c does not support returning more than one value this is necessary
    Row *table = getTable(stream, &row_count, &headers);
    snprintf(msgbuf, sizeof(msgbuf),
             "\n\n%d rows loaded.\n%d columns detected.\n\ncolumn analysis:\n",
             row_count, table->num_fields);

    send(client_fd, msgbuf, strlen(msgbuf), 0);

    norm *target_norm;
    message = "\n[Building normalized feature matrix X_norm...\nBuilding normalized target vector y_norm...\n";
    send(client_fd, message, strlen(message), 0);
    norm *normalization_table = getNormTable(table->num_fields, table, row_count, &target_norm, client_fd, headers);
    message = "\n[OK] All normalization threads completed.\n";
    send(client_fd, message, strlen(message), 0);

    for (int i = 0; i < table->num_fields; i++)
    {
        char *type = "categorical";
        if (normalization_table[i].is_num)
        {
            type = "numerical";
        }

        snprintf(msgbuf, sizeof(msgbuf),
                 "\n%20s: %s",
                 headers[i], type);

        send(client_fd, msgbuf, strlen(msgbuf), 0);

        if (i == table->num_fields - 1)
        {
            message = " (target)";
            send(client_fd, message, strlen(message), 0);
        }
    }

    double **transpose_matrix = getTansposeMatrix(normalization_table, row_count, table->num_fields);
    double **XTX = matrixMultiplication_nm(transpose_matrix, normalization_table, table->num_fields, row_count);
    double **inv_matrix = matrixInversion(XTX, table->num_fields);

    double **XTX_iXT =
        matrixMultiplication_mm(inv_matrix, transpose_matrix,
                                table->num_fields, // rows of inv_matrix
                                table->num_fields, // cols of inv_matrix = rows of XT
                                row_count);        // cols of XT
    message = "\nSolving (XᵀX)β = Xᵀy ...\n";
    send(client_fd, message, strlen(message), 0);
    double **coefficients = matrixMultiplication_mn(XTX_iXT, target_norm, table->num_fields, row_count);
    message = "\nTraining completed.\n\nFINAL MODEL (Normalized Form)\n";
    send(client_fd, message, strlen(message), 0);
    printf("max = %lf, min = %lf\n", normalization_table[0].x_max, normalization_table[0].x_min);
    printf("\n \n \n \n  X \n");

    // for (int i = 0; i < row_count; i++)
    // {
    //     for (int j = 0; j < table->num_fields; j++)
    //     {
    //         printf(" %f ", normalization_table[j].numValue[i]);
    //     }
    //     printf("\n");
    // }

    for (int j = 0; j < table->num_fields; j++)
    {
        printf(" %f ", normalization_table[j].numValue[2]);
    }
    message = "\nprice_norm =\n";
    send(client_fd, message, strlen(message), 0);
    snprintf(msgbuf, sizeof(msgbuf),
             "%lf\n",
             coefficients[0][0]);
    send(client_fd, msgbuf, strlen(msgbuf), 0);
    printf("\n \n \n \n  y \n");

    for (int i = 0; i < table->num_fields - 1; i++)
    {
        snprintf(msgbuf, sizeof(msgbuf),
                 "%lf * %s\n",
                 coefficients[i + 1][0], headers[i]);

        send(client_fd, msgbuf, strlen(msgbuf), 0);
        printf(" %lf\t", target_norm->numValue[i]);
        printf("\n");
    }

    message = "\n\nEnter new instance for prediction:\n\n";
    send(client_fd, message, strlen(message), 0);
    Row input;
    input.num_fields = table->num_fields - 1;
    input.values = malloc(sizeof(char *) * input.num_fields);
    for (int i = 0; i < table->num_fields - 1; i++)
    {
        if (normalization_table[i + 1].is_num)
        {
            snprintf(msgbuf, sizeof(msgbuf),
                     "%s  (xmin=%lf, xmax=%lf)\n",
                     headers[i], normalization_table[i + 1].x_min, normalization_table[i + 1].x_max);
        }
        else
        {
            snprintf(msgbuf, sizeof(msgbuf),
                     "%s\n",
                     headers[i]);
        }

        send(client_fd, msgbuf, strlen(msgbuf), 0);

        int n = recv(client_fd, buffer, sizeof(buffer) - 1, 0);
        if (n > 0)
        {
            buffer[n] = '\0';
            buffer[strcspn(buffer, "\r\n")] = '\0';
            printf("Client sent: '%s'\n", buffer);
        }
        input.values[i] = strdup(buffer);


        snprintf(msgbuf, sizeof(msgbuf),
                 "%s\n",
                 buffer);
                 send(client_fd, msgbuf, strlen(msgbuf), 0);

    }

    message = "\n\nNormalizing new input...\n";
    send(client_fd, message, strlen(message), 0);


    double *test = malloc(input.num_fields * sizeof(double));
    for (int i = 0; i < input.num_fields; i++)
    {   
        snprintf(msgbuf, sizeof(msgbuf),
        "normalizimg = %s\n",
        headers[i]);

        send(client_fd, msgbuf, strlen(msgbuf), 0);
        double value =0;
        if (normalization_table[i + 1].is_num)
        {
            sscanf(input.values[i], "%lf", &value);
            if (normalization_table[i + 1].x_max == normalization_table[i + 1].x_min)
            {
                test[i] = 0.0;
            }
            else
            {
                test[i] = (value - normalization_table[i + 1].x_min) / (normalization_table[i + 1].x_max - normalization_table[i + 1].x_min);
            }
        }
        else
        {
            bool flag = FALSE;
            for (int j = 0; j < sizeof(normalization_table[i + 1].catNames) / sizeof(char *); j++)
            {
                if (strcmp(input.values[i], "semi-furnished") == 0) // special rule
                {
                    test[i] = 0.5;

                    continue;
                }
                else if (strcmp(input.values[i], "no") == 0 || strcmp(input.values[i], "unfurnished") == 0)
                {
                    test[i] = 0;
                    continue;
                }
                else if (strcmp(input.values[i], "furnished") == 0 || strcmp(input.values[i], "yes") == 0)
                {
                    test[i] = 1;
                    continue;
                }
                if (strcmp(normalization_table[i + 1].catNames[j], input.values[i]) == 0) // if the value was already encountered
                {
                    test[i] = j;
                    flag=TRUE;
                
                }

            }
            if(1-flag){
                test[i] = sizeof(normalization_table[i + 1].catNames) / sizeof(char *);
            }
        }
    }

        for(int i=0;i<input.num_fields;i++){
            printf("max = %lf, min = %lf\n", normalization_table[i+1].x_max, normalization_table[i+1].x_min);

            snprintf(msgbuf, sizeof(msgbuf),
                     "%20s_norm = %lf\n",
                     headers[i], test [i]);

                     send(client_fd, msgbuf, strlen(msgbuf), 0);

        }

        

        double sum = coefficients[0][0];
        double coef_value=0;
        for (int i = 0; i < table->num_fields - 1; i++)
        {
            coef_value= test[i] * coefficients[i + 1][0];
            snprintf(msgbuf, sizeof(msgbuf),
            "%20s_norm = %lf\n",
            headers[i], coef_value);

            send(client_fd, msgbuf, strlen(msgbuf), 0);

            sum+=coef_value;
        }

        snprintf(msgbuf, sizeof(msgbuf),
            "\n\npredicted normalized %s = %lf\n",
            headers[table->num_fields-1], sum);

            send(client_fd, msgbuf, strlen(msgbuf), 0);


            message = "\n\nReverse-normalizing target...\n";
            send(client_fd, message, strlen(message), 0);   
            double y = (sum*(target_norm->x_max-target_norm->x_min))+target_norm->x_min;
            snprintf(msgbuf, sizeof(msgbuf),
            "\n\n %s = %lf\n",
            headers[table->num_fields-1], y);
            send(client_fd, msgbuf, strlen(msgbuf), 0);


            message = "\n\nPRICE PREDICTION RESULTS:\n";
            send(client_fd, message, strlen(message), 0);   

            snprintf(msgbuf, sizeof(msgbuf),
            "\n Normalized prediction : %lf\n",
             sum);
             send(client_fd, msgbuf, strlen(msgbuf), 0);

             snprintf(msgbuf, sizeof(msgbuf),
            "\n Real-scale prediction : %lf\n",
             y);
             send(client_fd, msgbuf, strlen(msgbuf), 0);

        // printf("\n \n  \n \n XT \n");
        // for (int i = 0; i < table->num_fields; i++)
        // {
        //     for (int j = 0; j < row_count; j++)
        //     {
        //         printf(" %lf\t", transpose_matrix[i][j]);
        //     }
        //     printf("\n");
        // }
        // printf("\n \n \n \n  XT*X \n");
        // for (int i = 0; i < table->num_fields; i++)
        // {
        //     for (int j = 0; j < table->num_fields; j++)
        //     {
        //         printf(" %lf\t", XTX[i][j]);
        //     }
        //     printf("\n");
        // }

        // printf("\n \n \n \n  (XT*X)^(-1) \n");
        // for (int i = 0; i < table->num_fields; i++)
        // {
        //     for (int j = 0; j < table->num_fields; j++)
        //     {
        //         printf(" %lf\t", inv_matrix[i][j]);
        //     }
        //     printf("\n");
        // }

        // printf("\n \n \n \n  (XTX)^(-1)*XT \n");

        // for (int i = 0; i < table->num_fields; i++)
        // { // rows = field_count
        //     for (int j = 0; j < row_count; j++)
        //     { // cols = row_count
        //         printf(" %lf\t", XTX_iXT[i][j]);
        //     }
        //     printf("\n");
        // }

       

        

        close(client_fd);
        close(socket_desc);

        freeNormTable(normalization_table, table->num_fields, row_count);
        freeHeaders(headers, table->num_fields);
        freeNorm(target_norm, row_count);
        freeMatrix(transpose_matrix, table->num_fields);
        freeMatrix(XTX, table->num_fields);
        freeMatrix(coefficients, table->num_fields);
        freeMatrix(XTX_iXT, table->num_fields);
        freeMatrix(inv_matrix, table->num_fields);
        freeTable(table, row_count);
    }

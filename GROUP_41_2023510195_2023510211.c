// Must compile with the given flags
// gcc GROUP_41_2023510195_2023510211.c -o output -lm
//
// -lm is required for math functions (only used for ceil function)

#include <arpa/inet.h> // for inet_addr
#include <ctype.h>     // might have to delete these
#include <ctype.h>
#include <math.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include <semaphore.h>

// We didnt want to always use 1/0 this is the same but better to read
#define bool int
#define TRUE 1
#define FALSE 0

// Configurations
#define MAX_SAMPLES 10000
#define MAX_FEATURES 100
#define STRING_BUFFER_LIMIT 100
#define PREPOC_THREAD_LIMIT 128
#define COEFF_THREAD_LIMIT 128
#define PORT 60000

// Global Variables
int socket_desc, client_fd;
sem_t mutex;
sem_t turnstile1;
sem_t turnstile2;
int barrier_count = 0;
int barrier_N = 0;

//---------------------------------------------------------------------------------

// exit--------------------------------------------------------------------------
//
// Exit with error message (also clear messages)
void exit_c(char *e) {
  char msgbuf[STRING_BUFFER_LIMIT];
  snprintf(msgbuf, sizeof(msgbuf), "\n\nERROR: %s \n", e);

  send(client_fd, msgbuf, strlen(msgbuf), 0);
  close(client_fd);
  close(socket_desc);
  perror(e);
  exit(1);
}
//---------------------------------------------------------------------------------

// READ CSV--------------------------------------------------------------------

typedef struct {
  char **values;  // array of strings for row values
  int num_fields; // number of fields in the row
} Row;

// Get field names from CSV header row
char **getFieldNames(char *line, int *num_fields) {
  int field_no = 0;
  int max_fields = MAX_FEATURES;
  char **field_names = malloc(max_fields * sizeof(char *));
  if (!field_names) {
    exit_c("malloc");
  }

  char *token = strtok(line, ",\n");
  while (token != NULL) {
    if (sizeof(token) > STRING_BUFFER_LIMIT * sizeof(char)) {

      exit_c("MAXIMUM STRING LENGTH HAS BEEN EXCEEDED");
    }
    field_names[field_no++] = strdup(token);

    if (field_no >= max_fields) {
      exit_c("MAXIMUM FIELDS HAVE BEEN EXCEEDED");
    }

    token = strtok(NULL, ",\n");
  }

  *num_fields = field_no;

  return field_names;
}

// Read a row from CSV file
Row readRow(char *line, int number_of_fields) {
  Row row;
  int field_no = 0;
  row.num_fields = number_of_fields;

  row.values = malloc(number_of_fields * sizeof(char *));
  if (!row.values) {
    exit_c("Malloc");
  }

  char *token = strtok(line, ",\n");
  while (token != NULL) {

    row.values[field_no] = strdup(token);
    field_no++;
    token = strtok(NULL, ",\n");
  }

  return row;
}

// Get table from CSV file
Row *getTable(FILE *stream, int *row_count, char ***headers) {
  char line[1024]; // is this supposed to be the string buffer limit or is that
                   // supposed to be for single values?
  int number_of_fields = 0;
  *row_count = 0;

  int capacity = MAX_SAMPLES;
  Row *table = malloc(capacity * sizeof(Row));
  if (!table) {
    exit_c("Malloc");
  }

  if (fgets(line, sizeof(line), stream)) {
    line[strcspn(line, "\r\n")] = 0;

    *headers = getFieldNames(
        line, &number_of_fields); // the first row is attribute names
    // we need to pass number of fields' addres so we can manipulate it
  }

  while (fgets(line, sizeof(line), stream)) {
    line[strcspn(line, "\r\n")] = 0;

    Row row = readRow(line, number_of_fields); // get each row from the table

    if (*row_count >= capacity) {

      exit_c("MAXIMUM NUMBER OF ROWS HAVE BEEN EXCEEDED");
    }

    table[(*row_count)++] = row;
  }

  return table;
}
//--------------------------------------------------------------------------------

// normalization-----------------------------------------------------------------------

typedef struct {
  char **catNames; // array of strings for the unnormalized values of
                   // categorical attributes
  int catAmount;
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

// Check if a string is numeric
int is_num(const char *str) {
  if (!str)
    return 0;

  // Skip leading whitespace
  while (isspace((unsigned char)*str))
    str++;

  if (*str == '\0')
    return 0; // empty or only spaces

  char *endptr;
  strtod(str, &endptr); // convert the string into a double. If the values are
                        // the same its not numeric.

  if (endptr == str)
    return 0; // no conversion happened

  // Skip trailing whitespace
  while (isspace((unsigned char)*endptr))
    endptr++;

  return *endptr == '\0'; // must end exactly
}

// Normalize numeric values in a column
void *normalize_num(void *arg_ptr) {
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
  if (!values_d) {
    exit_c("malloc");
  }

  if (!Norm->numValue) {
    exit_c("malloc");
  }

  for (int i = 0; i < row_count; i++) {
    sscanf(values[i], "%lf", &value);
    values_d[i] = value;
    if (value > Norm->x_max) // check min-max
    {
      Norm->x_max = value;
    }
    if (value < Norm->x_min) {
      Norm->x_min = value;
    }
  }

  char msgbuf[STRING_BUFFER_LIMIT];
  snprintf(msgbuf, sizeof(msgbuf),
           "[Thread N%d] Normalizing %s... xmin=%.2f xmax=%.2f\n",
           arg->thread_index, arg->col_name, Norm->x_min, Norm->x_max);

  send(arg->client_fd, msgbuf, strlen(msgbuf), 0);

  for (int i = 0; i < row_count; i++) {
    double denom = (Norm->x_max - Norm->x_min);
    if (denom == 0) // if x_min = x_max
    {
      Norm->numValue[i] = 0; // Just give 0 to all ofthem Idk what else to do
    } else {
      Norm->numValue[i] =
          (values_d[i] - Norm->x_min) / denom; // min-max normalization
    }
  }

  free(values_d);
  return Norm;
}

// Normalize categorical values to a numeric scale
void *normalize_cat(void *arg_ptr) {
  preproc_thread_arg *arg = arg_ptr; // arguments for mulitethreading
  char **values = arg->column_values;
  int row_count = arg->rowcount;
  norm *Norm = malloc(sizeof(norm));
  Norm->is_num = FALSE;
  Norm->catNames = calloc(row_count, sizeof(char *));
  Norm->numValue = malloc(row_count * sizeof(double));
  if (!Norm->catNames) {
    exit_c("malloc");
  }
  if (!Norm->numValue) {
    exit_c("malloc");
  }

  for (int i = 0; i < row_count; i++) {
    for (int j = 0; values[i][j]; j++) {
      values[i][j] = (char)tolower((unsigned char)values[i][j]);
    }

    if (strcmp(values[i], "semi-furnished") == 0) // special rule
    {
      Norm->catNames[1] = "semi-furnished";
      Norm->catAmount = 3;
      Norm->numValue[i] = 0.5;

      continue;
    } else if (strcmp(values[i], "no") == 0) {
      Norm->catNames[0] = "no";
      Norm->catAmount = 2;
      Norm->numValue[i] = 0;
      continue;
    } else if (strcmp(values[i], "yes") == 0) {
      Norm->catNames[1] = "yes";
      Norm->catAmount = 2;
      Norm->numValue[i] = 1;
      continue;
    } else if (strcmp(values[i], "unfurnished") == 0) {
      Norm->catNames[0] = "unfurnished";
      Norm->catAmount = 3;
      Norm->numValue[i] = 0;
      continue;
    } else if (strcmp(values[i], "furnished") == 0) {
      Norm->catNames[2] = "furnished";
      Norm->catAmount = 3;
      Norm->numValue[i] = 1;
      continue;
    }

    bool found = FALSE;
    for (int j = 0; j < Norm->catAmount; j++) {
      if (strcmp(Norm->catNames[j], values[i]) ==
          0) // if the value was already encountered
      {
        Norm->numValue[i] = j;
        found = TRUE;
        break;
      }
    }
    if (1 - found) // if there is no special rules for the values just number
                   // them according to the order they come
    {
      Norm->numValue[i] = Norm->catAmount;
      Norm->catNames[Norm->catAmount] = strdup(values[i]);
      Norm->catAmount++;
    }
  }

  char msgbuf[STRING_BUFFER_LIMIT];
  snprintf(msgbuf, sizeof(msgbuf), "[Thread N%d] Normalizing %s\n",
           arg->thread_index, arg->col_name);

  send(arg->client_fd, msgbuf, strlen(msgbuf), 0);

  Norm->x_min = 0.0;
  Norm->x_max = Norm->catAmount;
  if (Norm->x_max == 0) {
    return Norm;
  }
  for (int i = 0; i < row_count; i++) {
    Norm->numValue[i] /= Norm->x_max;
  }

  return Norm;
}

// Calculate the normalized table from the given table
norm *getNormTable(int number_of_fields, Row *table, int row_count,
                   norm *target_norm, int client_fd, char **headers) {
  norm *normalTable = malloc((number_of_fields) * sizeof(norm));
  if (!normalTable) {
    exit_c("malloc");
  }
  normalTable[0].is_num = TRUE;
  normalTable[0].numValue = malloc(row_count * sizeof(double));
  if (!normalTable[0].numValue) {
    exit_c("malloc");
  }

  for (int i = 0; i < row_count; i++) {
    normalTable[0].numValue[i] = 1.0;
  }

  pthread_t attribute_threads[PREPOC_THREAD_LIMIT];
  preproc_thread_arg *args[number_of_fields];
  for (int i = 0; i < number_of_fields; i++) // multithreading babyyyy
  {
    norm norm;
    int max = 0;
    int min = __INT_MAX__;
    char **column_values =
        malloc(row_count * sizeof(char *)); // gotta iterate over the attributes
                                            // so roll down the table
    // there is probably a more efficient way to do this
    for (int row = 0; row < row_count; row++) {
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

      pthread_create(&(attribute_threads[i]), NULL, normalize_num,
                     (void *)args[i]);
    } else {

      pthread_create(&(attribute_threads[i]), NULL, normalize_cat,
                     (void *)args[i]);
    }
  }
  for (int i = 0; i < number_of_fields; i++) {
    norm *result;
    if (pthread_join(attribute_threads[i], (void **)&result) !=
        0) // pthread_create returns wheter the create was actually created
    // we write the actual values into the result variable.
    {

      exit_c("pthread_join");
    }

    if (i == number_of_fields - 1) {
      *target_norm = *result; // assign the actual norm pointer
    } else {
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
// and others structs that have arrays in them, we can't just free them with the
// free() function.
void freeTable(Row *table, int major_index) {
  for (int r = 0; r < major_index; r++) {
    for (int c = 0; c < table[r].num_fields; c++) {
      free(table[r].values[c]);
    }
    free(table[r].values);
  }
  free(table);
}

void freeHeaders(char **headers, int index) {
  for (int i = 0; i < index; i++) {
    free(headers[i]);
  }
  free(headers);
}

void freeTargetNorm(norm target_norm, int row_count) {
  if (target_norm.is_num) {
    free(target_norm.numValue);
  } else {
    for (int i = 0; i < row_count; i++) {
      if (target_norm.catNames[i])
        free(target_norm.catNames[i]);
    }
    free(target_norm.catNames);
    free(target_norm.numValue);
  }
}

void freeNorm(norm n, int index) {

  if (n.is_num) {
    free(n.numValue);
  } else {
    for (int i = 0; i < index; i++) {
      if (n.catNames[i])
        free(n.catNames[i]);
    }
    free(n.catNames);
    free(n.numValue);
  }
}

void freeNormTable(norm *normalTable, int major_index, int index) {

  for (int i = 0; i < major_index; i++) {
    freeNorm(normalTable[i], index);
  }
  free(normalTable);
}

//---------------------------------------------------------------------------------

// matrix calculations--------------------------------------------------------------
typedef struct Matrix {
  double **data;
  int rows;
  int cols;
} Matrix;

typedef struct {
  Matrix *aug;
  int id;
  int n;
} ElimTask;

// Creates a matrix with 0 on all values
Matrix *createMatrix(int rows, int cols) {
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

// To free a matrix
void matrix_free(Matrix *m) {
  if (!m)
    return;
  for (int i = 0; i < m->rows; i++)
    free(m->data[i]);
  free(m->data);
  free(m);
}

// Switches between the turnstiles depending on the barrier count
void barrier_wait() {
  sem_wait(&mutex);
  barrier_count++;
  if (barrier_count == barrier_N) {
    sem_wait(&turnstile2);
    sem_post(&turnstile1);
  }
  sem_post(&mutex);

  sem_wait(&turnstile1);
  sem_post(&turnstile1);

  sem_wait(&mutex);
  barrier_count--;
  if (barrier_count == 0) {
    sem_wait(&turnstile1);
    sem_post(&turnstile2);
  }
  sem_post(&mutex);

  sem_wait(&turnstile2);
  sem_post(&turnstile2);
}

// This is used for solving the linear system, this is a thread function
void *eliminate_row(void *arg) {
  ElimTask *t = (ElimTask *)arg;

  int i = t->id;
  int n = t->n;
  Matrix *aug = t->aug;

  for (int k = 0; k < n; k++) {
    barrier_wait();

    if (i > k) {
      double beta = aug->data[i][k] / aug->data[k][k];
      for (int j = k; j <= n; j++)
        aug->data[i][j] -= beta * aug->data[k][j];
    }

    barrier_wait();
  }
  char msgbuf[STRING_BUFFER_LIMIT];

  snprintf(msgbuf, sizeof(msgbuf), "[Thread %d - Calculating beta-%d]\n", i, i);

  send(client_fd, msgbuf, strlen(msgbuf), 0);
  return NULL;
}

// For the given a matrices, solves Ax = B (x = beta coefficients)
Matrix *solveLinearSystem(Matrix *A, Matrix *B) {
  int n = A->rows;

  Matrix *aug = createMatrix(n, n + 1);
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++)
      aug->data[i][j] = A->data[i][j];
    aug->data[i][n] = B->data[i][0];
  }

  barrier_N = n + 1;
  barrier_count = 0;

  sem_init(&mutex, 0, 1);
  sem_init(&turnstile1, 0, 0);
  sem_init(&turnstile2, 0, 1);

  pthread_t threads[n];
  ElimTask tasks[n];

  for (int i = 0; i < n; i++) {
    tasks[i] = (ElimTask){aug, i, n};
    pthread_create(&threads[i], NULL, eliminate_row, &tasks[i]);
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

    if (max == 0.0)
      return NULL;

    if (pivot != k) {
      double *tmp = aug->data[k];
      aug->data[k] = aug->data[pivot];
      aug->data[pivot] = tmp;
    }

    barrier_wait();
    barrier_wait();
  }

  for (int i = 0; i < n; i++)
    pthread_join(threads[i], NULL);

  // Clear the semaphores
  sem_destroy(&mutex);
  sem_destroy(&turnstile1);
  sem_destroy(&turnstile2);

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

// This is used to turn norm array into a matrix (caused by different people doing different parts)
Matrix *getMatrix(norm *table, int rows, int cols) {
  Matrix *result = createMatrix(rows, cols);

  for (int c = 0; c < cols; c++) {
    for (int r = 0; r < rows; r++) {
      result->data[r][c] = table[c].numValue[r];
    }
  }

  return result;
}

// This is used to turn a norm into a vector (same problem as before)
Matrix *getVector(norm vector, int rows) {
  Matrix *result = createMatrix(rows, 1);

  for (int r = 0; r < rows; r++) {
    result->data[r][0] = vector.numValue[r];
  }

  return result;
}

//---------------------------------------------------------------------------------
// This part is for handling the X^T * X and X^T * Y

typedef struct {
  const Matrix *X;
  const Matrix *Y;
  int row_start;
  int row_end;
  Matrix *XTX_local;
  Matrix *XTY_local;
  int client_fd;
  int thread_no;
} NormalEqTask;

// Thread helper function
void *normal_eq_worker(void *arg) {
  NormalEqTask *t = (NormalEqTask *)arg;
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

// This is the main function for calculating the coefficients
Matrix *solveNormalEquation_parallel(

    const Matrix *X, const Matrix *Y, int num_threads, int client_fd,
    char **headers) {

  char msgbuf[STRING_BUFFER_LIMIT];

  int n = X->rows;
  int d = X->cols;

  Matrix *XTX = createMatrix(d, d);
  Matrix *XTY = createMatrix(d, 1);

  int rows_per_thread = (int)ceil((double)n / (double)num_threads);
  num_threads = (int)ceil((double)n / (double)rows_per_thread);

  pthread_t threads[num_threads];
  NormalEqTask tasks[num_threads];
  for (int t = 0; t < num_threads; t++) {

    tasks[t].X = X;
    tasks[t].Y = Y;
    tasks[t].row_start = t * rows_per_thread;
    tasks[t].row_end = (t == num_threads - 1) ? n : (t + 1) * rows_per_thread;

    tasks[t].XTX_local = createMatrix(d, d);
    tasks[t].XTY_local = createMatrix(d, 1);
    tasks[t].client_fd = client_fd;
    tasks[t].thread_no = t;

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

int main() {
  struct sockaddr_in server_addr, client_addr;
  socklen_t addr_len = sizeof(client_addr);

  socket_desc = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_desc < 0) {
    exit_c("socket");
  }

  server_addr.sin_family = AF_INET;
  server_addr.sin_addr.s_addr = INADDR_ANY;
  server_addr.sin_port = htons(PORT);

  if (bind(socket_desc, (struct sockaddr *)&server_addr, sizeof(server_addr)) <
      0) {
    exit_c("bind");
  }

  if (listen(socket_desc, 1) < 0) {
    exit_c("listen");
  }

  printf("Listening on port %d...\n", PORT);

  client_fd = accept(socket_desc, (struct sockaddr *)&client_addr, &addr_len);
  if (client_fd < 0) {
    exit_c("accept");
  }

  // This is the start of the whole deal
  char *message = "\n\nWELCOME TO TARGET PREDICTION SERVER\n"
                  "Enter CSV file name:";

  send(client_fd, message, strlen(message), 0);

  // Get and read the file name
  char buffer[STRING_BUFFER_LIMIT];
  int n = recv(client_fd, buffer, sizeof(buffer) - 1, 0);
  if (n > 0) {
    buffer[n] = '\0';
    buffer[strcspn(buffer, "\r\n")] = '\0';
    printf("Client sent: '%s'\n", buffer);
  }

  message = "\n\nChecking dataset...\n";
  send(client_fd, message, strlen(message), 0);

  FILE *stream = fopen(buffer, "r"); // read the file
  if (!stream) {
    exit_c("File couldn't be found");
  }

  char msgbuf[STRING_BUFFER_LIMIT];
  snprintf(msgbuf, sizeof(msgbuf), "\n\n[OK] File %s found. Reading file...\n",
           buffer);

  send(client_fd, msgbuf, strlen(msgbuf), 0);

  char **headers;
  int row_count;
  // we need to pass the adresses of row_count and headers so we can manipulate
  // them inside the functi0n since c does not support returning more than one
  // value this is necessary
  Row *table = getTable(stream, &row_count, &headers);
  snprintf(msgbuf, sizeof(msgbuf),
           "\n\n%d rows loaded.\n%d columns detected.\n\ncolumn analysis:\n",
           row_count, table->num_fields);

  send(client_fd, msgbuf, strlen(msgbuf), 0);

  // Calculate the norm vector parallelized
  norm target_norm;
  message = "\n[Building normalized feature matrix X_norm...\nBuilding "
            "normalized target vector y_norm...\n\n\n";
  send(client_fd, message, strlen(message), 0);
  norm *normalization_table = getNormTable(table->num_fields, table, row_count,
                                           &target_norm, client_fd, headers);
  message = "\n[OK] All normalization threads completed.\n";
  send(client_fd, message, strlen(message), 0);

  for (int i = 0; i < table->num_fields; i++) {
    char *type = "categorical";
    if (normalization_table[i].is_num) {
      type = "numerical";
    }

    snprintf(msgbuf, sizeof(msgbuf), "\n%20s: %s", headers[i], type);

    send(client_fd, msgbuf, strlen(msgbuf), 0);

    if (i == table->num_fields - 1) {
      message = " (target)";
      send(client_fd, message, strlen(message), 0);
    }
  }

  // Training part
  // Calculate the X and Y matrices then calculate the coefficients (B)
  Matrix *X = getMatrix(normalization_table, row_count, table->num_fields);
  Matrix *Y = getVector(target_norm, row_count);

  message = "\n\n\nSolving (XᵀX)β = Xᵀy ...\n\n";
  send(client_fd, message, strlen(message), 0);
  Matrix *B = solveNormalEquation_parallel(X, Y, COEFF_THREAD_LIMIT, client_fd,
                                           headers);
  message = "\nTraining completed.\n\nFINAL MODEL (Normalized Form)\n";
  send(client_fd, message, strlen(message), 0);

  snprintf(msgbuf, sizeof(msgbuf), "%s_norm =\n",
           headers[table->num_fields - 1]);
  send(client_fd, msgbuf, strlen(msgbuf), 0);
  snprintf(msgbuf, sizeof(msgbuf), "%lf\n", B->data[0][0]);
  send(client_fd, msgbuf, strlen(msgbuf), 0);

  // Printing the results
  for (int i = 0; i < table->num_fields - 1; i++) {
    char sign = '+';
    double val = B->data[i + 1][0];
    if (val < 0) {
      sign = '-';
      val = -val;
    }

    snprintf(msgbuf, sizeof(msgbuf), "%c %lf * %s\n", sign, val, headers[i]);

    send(client_fd, msgbuf, strlen(msgbuf), 0);
  }

  // The main question loop
  bool replay = TRUE;
  while (replay) {
    message = "\n\nEnter new instance for prediction:\n\n";
    send(client_fd, message, strlen(message), 0);
    Row input;
    input.num_fields = table->num_fields - 1;
    input.values = malloc(sizeof(char *) * input.num_fields);
    for (int i = 0; i < table->num_fields - 1; i++) {
      if (normalization_table[i + 1].is_num) {
        snprintf(
            msgbuf, sizeof(msgbuf), "%s (xmin=%lf, xmax=%lf): ", headers[i],
            normalization_table[i + 1].x_min, normalization_table[i + 1].x_max);
        send(client_fd, msgbuf, strlen(msgbuf), 0);
      } else {
        snprintf(msgbuf, sizeof(msgbuf), "%s (", headers[i]);
        send(client_fd, msgbuf, strlen(msgbuf), 0);

        for (int j = 0; j < normalization_table[i + 1].catAmount; j++) {
          snprintf(msgbuf, sizeof(msgbuf), "%s",
                   normalization_table[i + 1].catNames[j]);
          send(client_fd, msgbuf, strlen(msgbuf), 0);

          if (j < normalization_table[i + 1].catAmount - 1) {
            send(client_fd, ", ", 2, 0);
          }
        }
        send(client_fd, "): ", 3, 0);
      }

      // We must repeat the request if the given response is invalid
      bool repeat = FALSE;
      do {
        repeat = FALSE;
        int n = recv(client_fd, buffer, sizeof(buffer) - 1, 0);
        send(client_fd, "\n", 1, 0);
        if (n > 0) {
          buffer[n] = '\0';
          buffer[strcspn(buffer, "\r\n")] = '\0';
          printf("Client sent: '%s'\n", buffer);
        }
        input.values[i] = strdup(buffer);
        for (int k = 0; input.values[i][k]; k++) {
          input.values[i][k] = (char)tolower((unsigned char)input.values[i][k]);
        }

        if (normalization_table[i + 1].is_num && 1 - is_num(input.values[i])) {
          message = "PLEASE ENTER A NUMBER: ";
          send(client_fd, message, strlen(message), 0);
          repeat = TRUE;
        } else if (1 - normalization_table[i + 1].is_num) {
          int pivot = -1;
          for (int j = 0; j < normalization_table[i + 1].catAmount; j++) {

            if (strcmp(normalization_table[i + 1].catNames[j],
                       input.values[i]) ==
                0) // if the value was already encountered
            {
              pivot = j;
              break;
            }
          }

          if (pivot == -1) {
            message = "PLEASE ENTER A VALID CATEGORY: ";
            send(client_fd, message, strlen(message), 0);
            repeat = TRUE;
          }
        }
      } while (repeat);
    }

    // Normalize the input
    message = "\n\nNormalizing new input...\n";
    send(client_fd, message, strlen(message), 0);

    double *test = malloc(input.num_fields * sizeof(double));
    for (int i = 0; i < input.num_fields; i++) {
      snprintf(msgbuf, sizeof(msgbuf), "normalizing = %s\n", headers[i]);

      send(client_fd, msgbuf, strlen(msgbuf), 0);
      double value = 0;
      if (normalization_table[i + 1].is_num) {
        sscanf(input.values[i], "%lf", &value);
        if (normalization_table[i + 1].x_max ==
            normalization_table[i + 1].x_min) {
          test[i] = 0.0;
        } else {
          test[i] = (value - normalization_table[i + 1].x_min) /
                    (normalization_table[i + 1].x_max -
                     normalization_table[i + 1].x_min);
        }
      } else {
        bool flag = FALSE;
        if (strcmp(input.values[i], "semi-furnished") == 0) // special rule
        {
          test[i] = 0.5;

          continue;
        } else if (strcmp(input.values[i], "no") == 0 ||
                   strcmp(input.values[i], "unfurnished") == 0) {
          test[i] = 0;
          continue;
        } else if (strcmp(input.values[i], "furnished") == 0 ||
                   strcmp(input.values[i], "yes") == 0) {
          test[i] = 1;
          continue;
        }
        for (int j = 0; j < normalization_table[i + 1].catAmount; j++) {

          if (strcmp(normalization_table[i + 1].catNames[j], input.values[i]) ==
              0) // if the value was already encountered
          {
            test[i] = j;
            flag = TRUE;
          }
        }
        if (1 - flag) {
          exit_c("invalid input");
        }
      }
    }

    // Print out the result of user input
    double sum = B->data[0][0];
    double coef_value = 0;
    for (int i = 0; i < table->num_fields - 1; i++) {
      coef_value = test[i] * B->data[i + 1][0];
      snprintf(msgbuf, sizeof(msgbuf), "\n%20s_norm = %lf", headers[i],
               test[i]);

      send(client_fd, msgbuf, strlen(msgbuf), 0);

      sum += coef_value;
    }

    snprintf(msgbuf, sizeof(msgbuf), "\n\nPredicted normalized %s = %lf\n",
             headers[table->num_fields - 1], sum);

    send(client_fd, msgbuf, strlen(msgbuf), 0);

    message = "\n\nReverse-normalizing target...\n";
    send(client_fd, message, strlen(message), 0);
    double y =
        (sum * (target_norm.x_max - target_norm.x_min)) + target_norm.x_min;
    snprintf(msgbuf, sizeof(msgbuf), "%s = %lf * (%lf - %lf) + %lf\n",
             headers[table->num_fields - 1], sum, target_norm.x_max,
             target_norm.x_min, target_norm.x_min);
    send(client_fd, msgbuf, strlen(msgbuf), 0);
    snprintf(msgbuf, sizeof(msgbuf), "%s = %lf\n",
             headers[table->num_fields - 1], y);
    send(client_fd, msgbuf, strlen(msgbuf), 0);

    snprintf(msgbuf, sizeof(msgbuf), "\n%s PREDICTION RESULTS:\n",
             headers[table->num_fields - 1]);
    send(client_fd, msgbuf, strlen(msgbuf), 0);

    snprintf(msgbuf, sizeof(msgbuf), "\nNormalized prediction : %lf\n", sum);
    send(client_fd, msgbuf, strlen(msgbuf), 0);

    snprintf(msgbuf, sizeof(msgbuf), "\nReal-scale prediction : %lf\n", y);
    send(client_fd, msgbuf, strlen(msgbuf), 0);

    replay = FALSE;

    // Ask to repeat the loop
    message = "\n\nDo you want to continue? (y/n):\n";
    send(client_fd, message, strlen(message), 0);
    while (TRUE) {
      n = recv(client_fd, buffer, sizeof(buffer) - 1, 0);
      if (n > 0) {
        buffer[n] = '\0';
        buffer[strcspn(buffer, "\r\n")] = '\0';
        printf("Client sent: '%s'\n", buffer);
      }

      if (strcmp(buffer, "y") == 0) {
        replay = TRUE;
        break;
      } else if (strcmp(buffer, "n") == 0) {
        snprintf(msgbuf, sizeof(msgbuf),
                 "\n\nThank you for using %s PREDICTION SERVER! Good Bye!\n",
                 headers[table->num_fields - 1]);
        send(client_fd, msgbuf, strlen(msgbuf), 0);
        break;
      }
      message = "\n\nType either 'Y' or 'n' \n";
      send(client_fd, message, strlen(message), 0);
    }
  }

  // Clear the remaining resources
  close(client_fd);
  close(socket_desc);

  freeNormTable(normalization_table, table->num_fields, row_count);
  freeHeaders(headers, table->num_fields);
  freeNorm(target_norm, row_count);
  freeTable(table, row_count);

  matrix_free(B);
}

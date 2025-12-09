#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>// might have to delete these
#define bool int
#define TRUE 1
#define FALSE 0 
//READ CSV--------------------------------------------------------------------

typedef struct {
    char **values;       // array of strings for row values
    int num_fields;      // number of fields in the row
} Row;

typedef struct {
    char** catNames;       // array of strings for row values
    double* numValue;      // number of fields in the row
    bool is_num;
    int x_min;
    int x_max;
} norm;

char** getFieldNames(char* line, int* num_fields) {
    int field_no = 0;
    int max_fields = 100;
    char **field_names = malloc(max_fields * sizeof(char*));
    if (!field_names) { perror("malloc"); exit(1);}    

    char* token = strtok(line, ",\n");
    while (token != NULL) {
        
        field_names[field_no++] = strdup(token);

        if (field_no >= max_fields) {
            max_fields *= 2;
            field_names = realloc(field_names, max_fields * sizeof(char*));
            
        }

        token = strtok(NULL, ",\n");
    }

    *num_fields = field_no; // store number of fields
   
    return field_names;
}

Row readRow(char* line, int number_of_fields){
    
    Row row;
    int field_no =0;
    row.num_fields = number_of_fields;
    // Allocate initial space for up to 100 fields
    
    row.values = malloc(number_of_fields* sizeof(char*));
    if (!row.values) { perror("malloc"); exit(1);}    

    char* token = strtok(line, ",\n");
    while (token != NULL ) {
        
        row.values[field_no] = strdup(token);
        field_no++;
        token = strtok(NULL, ",\n");
    }

    if (field_no != number_of_fields) {
       
        // Fill missing fields with empty strings to avoid garbage
        for (int k = field_no; k < number_of_fields; ++k) {
            row.values[k] = strdup("");
            
        }
    }

    return row;

}


Row* getTable(FILE* stream, int* row_count, char*** headers) {
    char line[1024];
    int number_of_fields=0;
    *row_count = 0;

    int capacity = 100; // initial row capacity
    Row* table = malloc(capacity * sizeof(Row));
    if (!table) { perror("malloc"); exit(1);}    

    if (fgets(line, sizeof(line), stream)) {
        line[strcspn(line, "\r\n")] = 0;
       
        *headers = getFieldNames(line, &number_of_fields);
    }

    while (fgets(line, sizeof(line), stream)) {
        line[strcspn(line, "\r\n")] = 0;
      
        Row row = readRow(line, number_of_fields);

        if (*row_count >= capacity) {
            capacity *= 2;
            table = realloc(table, capacity * sizeof(Row));
            
        }

        table[(*row_count)++] = row;
        
    }

    
    return table;


}
//--------------------------------------------------------------------------------


//normalization-----------------------------------------------------------------------

#include <ctype.h>
#include <stdlib.h>

int is_num(const char *str) {
    if (!str) return 0;

    // Skip leading whitespace
    while (isspace((unsigned char)*str)) str++;

    if (*str == '\0') return 0; // empty or only spaces

    char *endptr;
    strtod(str, &endptr);

    if (endptr == str)
        return 0;  // no conversion happened

    // Skip trailing whitespace
    while (isspace((unsigned char)*endptr)) endptr++;

    return *endptr == '\0';  // must end exactly
}

norm normalize_num(char** values, int row_count){
    
norm Norm;
Norm.is_num = TRUE;
Norm.x_max=-__INT32_MAX__;
Norm.x_min=__INT32_MAX__;
double value;
double* values_d = malloc(row_count*sizeof(double));
if (!values_d) { perror("malloc"); exit(1);}    
Norm.numValue = malloc(row_count * sizeof(double));
if (!Norm.numValue) { perror("malloc"); exit(1);}    
for(int i=0; i<row_count; i++){
    sscanf(values[i], "%lf", &value);
    values_d[i] = value;
    if (value>Norm.x_max){
        Norm.x_max=value;
    }
    else if(value<Norm.x_min){
        Norm.x_min=value;
    }
}

for(int i=0; i<row_count; i++){
    double denom = (Norm.x_max - Norm.x_min);
    if (denom == 0) {
        Norm.numValue[i] = 0.0;
    } else {
        Norm.numValue[i] = (values_d[i] - Norm.x_min) / denom;
    }
}

free(values_d);

return Norm;
}

norm normalize_cat(char** values, int row_count, int num_of_fields){
    
norm Norm;
Norm.is_num=FALSE;
Norm.catNames = malloc(row_count*sizeof(char*));
if (!Norm.catNames) { perror("malloc"); exit(1);}    
Norm.numValue = malloc(row_count*sizeof(double));
if (!Norm.numValue) { perror("malloc"); exit(1);}    
int indexVal=0;
int number_of_cats=0;
for(int i=0; i<row_count; i++){

    if(strcmp(values[i], "yes")==0 || strcmp(values[i], "semi-furnished")==0 )//special rule
    {
        Norm.numValue[i] = 1;
        continue;
    }else if (strcmp(values[i], "no")==0 || strcmp(values[i], "unfurnished")==0)
    {
        Norm.numValue[i] = 0;
        continue;
    }
    else if (strcmp(values[i], "furnished")==0)
    {
        Norm.numValue[i]=2;
        continue;
    }
    

    bool found =FALSE;
    for(int j=0; j<number_of_cats; j++)
    {
        if(strcmp(Norm.catNames[j], values[i]) == 0){
            Norm.numValue[i] = j;
            found=TRUE;
            break;
        }   
    }
    if(1-found){
        Norm.numValue[i] = indexVal;
        Norm.catNames[indexVal] = strdup(values[i]);
        
        number_of_cats++;
        indexVal++;
    }
}
number_of_cats;

return Norm;
}

norm* getNormTable(int number_of_fields, Row* table, int row_count){
    norm* normalTable = malloc(number_of_fields * sizeof(norm));
    if (!normalTable) { perror("malloc"); exit(1);}    
    for (int i=0; i<number_of_fields; i++){
        norm norm;
        int max=0;
        int min=__INT_MAX__;
        char** column_values=malloc(row_count*sizeof(char*));
        if (!column_values) { perror("malloc"); exit(1);}        
        for(int row=0; row<row_count;row++){
            column_values[row] = table[row].values[i];
        }
        bool num_flag = is_num(table[0].values[i]);
       
    
            if(num_flag){
                norm = normalize_num(column_values, row_count);
            }
            else{
                norm = normalize_cat(column_values, row_count, number_of_fields);
            }
        
        normalTable[i] = norm;
        free(column_values);
    }
    return normalTable;

}
//-----------------------------------------------------------------------

//garbage collection------------------------------------------------------
void freeTable(Row* table, int row_count) {
    for (int r = 0; r < row_count; r++) {
        for (int c = 0; c < table[r].num_fields; c++) {
            free(table[r].values[c]);
        }
        free(table[r].values);
    }
    free(table);
}

void freeHeaders(char** headers, int num_fields) {
    for (int i = 0; i < num_fields; i++) {
        free(headers[i]);
    }
    free(headers);
}

void freeNorm(norm* n, int row_count) {
    if (n->is_num) {
        free(n->numValue);
    } else {
        for (int i = 0; i < row_count; i++) {
            if (n->catNames[i])
                free(n->catNames[i]);
        }
        free(n->catNames);
        free(n->numValue);
    }
}

void freeNormTable(norm* normalTable, int number_of_fields, int row_count) {
    for (int i = 0; i < number_of_fields; i++) {
        freeNorm(&normalTable[i], row_count);
    }
    free(normalTable);
}
//---------------------------------------------------------------------------------

int main(){
    FILE* stream = fopen("Housing.csv", "r");
    if (!stream) { perror("fopen"); return 1; }


    char** headers;
    int row_count;
    Row* table = getTable(stream, &row_count, &headers);


    norm* normalization_table = getNormTable(table->num_fields, table, row_count);


    for (int i = 0; i < row_count; i++) {
        for (int j = 0; j < table->num_fields; j++) {
            printf("%s: %s\t  %s: %lf\t", headers[j] ,table[i].values[j], headers[j], normalization_table[j].numValue[i] );

          }
          printf("\n");
      }
      

freeNormTable(normalization_table, table->num_fields, row_count);
freeTable(table, row_count);
freeHeaders(headers, table->num_fields);



}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//READ CSV 


typedef struct {
    char **field_names;  // array of strings for column names
    char **values;       // array of strings for row values
    int num_fields;      // number of fields in the row
} Row;



char** getFieldNames(char* line, int* num_fields) {
    int field_no = 0;
    int max_fields = 100;
    char **field_names = malloc(max_fields * sizeof(char*));

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

Row readRow(char* line, char** field_names,int number_of_fields){
    Row row;
    int field_no =0;
    row.num_fields = number_of_fields;
    // Allocate initial space for up to 100 fields
    
    row.values = malloc(number_of_fields* sizeof(char*));
    row.field_names = field_names;

    
       
        char* token = strtok(line, ",\n");
    while (token != NULL ) {
        row.values[field_no] = strdup(token);
        field_no++;
        token = strtok(NULL, ",\n");
    }

    return row;

}

Row* getTable(FILE* stream, int* row_count, char*** headers) {
    char line[1024];
    int number_of_fields=0;
    *row_count = 0;
    int capacity = 100; // initial row capacity
    Row* table = malloc(capacity * sizeof(Row));

    if (fgets(line, sizeof(line), stream)) {
        line[strcspn(line, "\r\n")] = 0;
        *headers = getFieldNames(line, &number_of_fields);
    }

    while (fgets(line, sizeof(line), stream)) {
        line[strcspn(line, "\r\n")] = 0;
        Row row = readRow(line, *headers, number_of_fields);

        if (*row_count >= capacity) {
            capacity *= 2;
            table = realloc(table, capacity * sizeof(Row));
        }

        table[(*row_count)++] = row;
    }

    return table;


}


int main(){
    FILE* stream = fopen("Housing.csv", "r");
    if (!stream) {
        perror("fopen");
        return 1;
    }

    char** headers;
    int num_fields;
    int row_count;
    Row* table = getTable(stream, &row_count, &headers);


    for (int i = 0; i < row_count; i++) {
        for (int j = 0; j < table->num_fields; j++) {
            printf("%s: %s\t", table[i].field_names[j],table[i].values[j] );
          }
      }


}


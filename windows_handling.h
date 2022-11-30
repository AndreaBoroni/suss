#ifndef __WINDOWS_HANDLING__
#define __WINDOWS_HANDLING__

#include <windows.h>
#include <math.h>
#include <stdio.h>

using namespace std;

typedef unsigned char  u8;
typedef unsigned int  u32;
typedef char           s8;
typedef int           s32;

char *load_file_memory(char *file_name, unsigned int *size) {
    char *buffer = NULL;
    DWORD number_of_bytes_read, high_file_size, error;
    HANDLE file_handle;

    file_handle = CreateFile(file_name, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if(file_handle == INVALID_HANDLE_VALUE) {
        printf("ERROR: unable to create file handle\n");
        return NULL;
    }

    *size = GetFileSize(file_handle, &high_file_size);

    buffer = (char *) malloc(*size);
    bool result = ReadFile(file_handle, buffer, *size, &number_of_bytes_read, NULL);
    CloseHandle(file_handle);

    if (!result) {
        error = GetLastError();
        printf("ERROR while reading the file %s. Error code: %d\n", file_name, error);
    }
    return buffer;
}

bool save_file_into_memory(char *file_name, char *data, int file_size) {
    
    HANDLE file_handle;
    file_handle = CreateFile(file_name, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    
    if(file_handle == INVALID_HANDLE_VALUE) {
        printf("ERROR: unable to create file handle\n");
        return false;
    }

    DWORD number_of_bytes_written, error;
    bool success = WriteFile(file_handle, data, file_size, &number_of_bytes_written, NULL);
    bool end_of_file_success = SetEndOfFile(file_handle);
    CloseHandle(file_handle);

    if(number_of_bytes_written != file_size) printf("Not everything was written!!\n");
    
    if (!success || !end_of_file_success) {
        error = GetLastError();
        printf("ERROR while reading the file %s. Error code: %d\n", file_name, error);
        return false;
    }
    return true;
}

#endif
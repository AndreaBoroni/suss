#ifndef __WINDOWS_HANDLING__
#define __WINDOWS_HANDLING__

#include <windows.h>
#include <string>
#include <math.h>

using namespace std;

#define milliseconds LARGE_INTEGER
#define get_time(t)  QueryPerformanceCounter((t));
#define time_elapsed(t0, t1, CPM) ((t1).QuadPart - (t0).QuadPart) / CPM;

typedef unsigned char  u8;
typedef unsigned int  u32;
typedef char           s8;
typedef int           s32;

struct keybord_inputs {
    u32 *presses;
    u32 *releases;
};

struct bitmap_buffer
{
    BITMAPINFO Info;
    void *Memory;
    
    int Width;
    int Height;

    int BytesPerPixel;
};

struct Color {
    int red, green, blue;
    int alpha;
};

keybord_inputs inputs = {NULL, NULL};

bool changed_size   = true;
bool regained_focus = true;

bitmap_buffer Buffer = {0};

void ResizeBuffer(bitmap_buffer *Buffer, int new_width, int new_height);
void DisplayBufferToWindow(HWND Window, bitmap_buffer *Buffer);
void InitializeBuffer(bitmap_buffer *Buffer, int width, int height);
LRESULT CALLBACK MainWindowCallback(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam);
void startWindow(HWND *Window, string window_name);
int handle_messages();

void ResizeBuffer(bitmap_buffer *Buffer, int new_width, int new_height)
{    
    if (Buffer->Memory) free(Buffer->Memory);
    
    Buffer->Info.bmiHeader.biWidth  =  new_width;
    Buffer->Info.bmiHeader.biHeight = -new_height;
    Buffer->Width  = new_width;
    Buffer->Height = new_height;

    Buffer->Memory = malloc((new_width * new_height)*Buffer->BytesPerPixel);
}

void DisplayBufferToWindow(HWND Window, bitmap_buffer *Buffer)
{
    HDC DeviceContext = GetDC(Window);

    StretchDIBits(DeviceContext,
                  0, 0, Buffer->Width, Buffer->Height,
                  0, 0, Buffer->Width, Buffer->Height,
                  Buffer->Memory, &Buffer->Info,
                  DIB_RGB_COLORS, SRCCOPY);

}

void InitializeBuffer(bitmap_buffer *Buffer, int width, int height)
{
    Buffer->Info.bmiHeader.biSize        = sizeof(Buffer->Info.bmiHeader);
    Buffer->Info.bmiHeader.biPlanes      = 1;
    Buffer->Info.bmiHeader.biBitCount    = 32;
    Buffer->Info.bmiHeader.biCompression = BI_RGB;

    Buffer->BytesPerPixel = 4;

    Buffer->Width  = width;
    Buffer->Height = height;
    Buffer->Memory = malloc((width*height) * Buffer->BytesPerPixel);
}

LRESULT CALLBACK MainWindowCallback(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam) {

    LRESULT Result = 0;

    switch(Message) {
        case WM_SIZE:
        {
            RECT ClientRect;
            GetClientRect(Window, &ClientRect);
            int width  = ClientRect.right  - ClientRect.left;
            int height = ClientRect.bottom - ClientRect.top;
            ResizeBuffer(&Buffer, width, height);
            DisplayBufferToWindow(Window, &Buffer);
            changed_size = true;

        } break;
        case WM_DESTROY:
        case WM_CLOSE: {
            PostQuitMessage(0);
            PostQuitMessage(0);
        } break;
        case WM_SYSKEYUP:
        case WM_SYSKEYDOWN:
        case WM_KEYDOWN:
        case WM_KEYUP: {

            uint32_t VKCode = WParam;
            bool WasDown = ((LParam & (1 << 30)) != 0); // Button released
            bool IsDown  = ((LParam & (1 << 31)) == 0); // Buttom pressed

            if (IsDown == WasDown) break;
            if (WasDown) ; //stb_arr_push(inputs.releases, get_binding_from_keyboard(VKCode));
            if (IsDown)  ; //stb_arr_push(inputs.presses,  get_binding_from_keyboard(VKCode));

        } break;
        case WM_KILLFOCUS: {
            // stb_arr_free(inputs.presses);
            // stb_arr_free(inputs.releases);
        } break;
        case WM_SETFOCUS: {
            regained_focus = true;
        } break;
        default: {
            Result = DefWindowProc(Window, Message, WParam, LParam);
        } break;
    }

    return Result;
}

void startWindow(HWND *Window, string window_name)
{
    WNDCLASS WindowClass = {};
    WindowClass.style         = CS_OWNDC | CS_HREDRAW | CS_VREDRAW;
    WindowClass.lpfnWndProc   = MainWindowCallback;
    WindowClass.hInstance     = GetModuleHandle(0);
    WindowClass.lpszClassName = "GeneralWindowClass";

    RegisterClass(&WindowClass);
    *Window = CreateWindowEx(
        0,
        WindowClass.lpszClassName,
        "Main",
        WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_VISIBLE | WS_THICKFRAME,
        CW_USEDEFAULT, CW_USEDEFAULT,
        800, 500, 0, 0,
        GetModuleHandle(0), 0);
}

int handle_messages() {
    MSG Message;
    while(PeekMessage(&Message, NULL, 0, 0, PM_REMOVE))
    {
        TranslateMessage(&Message);
        DispatchMessage(&Message);
        if (Message.message == WM_QUIT)
        {
            PostQuitMessage(0);
            return 0;
        }
    }
    return 1;
}


Color interpolate_colors(Color c1, Color c2, float a)
{
    Color interpolation;
    interpolation.red   = c1.red   * a + c2.red   * (1 - a);
    interpolation.green = c1.green * a + c2.green * (1 - a);
    interpolation.blue  = c1.blue  * a + c2.blue  * (1 - a);
    interpolation.alpha = 255;
    return interpolation;
}

Color hex_to_color(u32 hex_color)
{
    Color c;
    c.red   = ((hex_color >> 16) & 0xFF); 
    c.green = ((hex_color >> 8)  & 0xFF);
    c.blue  = ((hex_color)       & 0xFF); 
    return c;
}

void render_filled_rect(bitmap_buffer *Buffer, RECT *rect, Color color)
{
    u32 c = ((color.red << 16) | (color.green << 8) | color.blue);
    
    int starting_x = max((int) rect->left,   0);
    int starting_y = max((int) rect->top,    0);
    int ending_x   = min((int) rect->right,  Buffer->Width);
    int ending_y   = min((int) rect->bottom, Buffer->Height);
    
    if (starting_x > Buffer->Width)  return;
    if (starting_y > Buffer->Height) return;
    if (ending_x < 0) return;
    if (ending_y < 0) return;

    int Pitch = Buffer->Width * Buffer->BytesPerPixel;
    u8 *Row = (u8 *) Buffer->Memory;
    Row += starting_x*Buffer->BytesPerPixel + starting_y*Pitch;
    
    for (int Y = starting_y; Y < ending_y; Y++) {        
        u32 *Pixel = (u32 *)Row;   
        for (int X = starting_x; X < ending_x; X++) {
            *Pixel = c;
            Pixel++;
        }
        Row += Pitch;
    }
}

void render_transparent_rect(bitmap_buffer *Buffer, RECT *rect, Color color)
{
    if (color.alpha == 255) render_filled_rect(Buffer, rect, color);
    
    int starting_x = max((int) rect->left,   0);
    int starting_y = max((int) rect->top,    0);
    int ending_x   = min((int) rect->right,  Buffer->Width);
    int ending_y   = min((int) rect->bottom, Buffer->Height);
    
    if (starting_x > Buffer->Width)  return;
    if (starting_y > Buffer->Height) return;
    if (ending_x < 0) return;
    if (ending_y < 0) return;

    int Pitch = Buffer->Width*Buffer->BytesPerPixel;
    u8 *Row = (u8 *) Buffer->Memory;
    Row += starting_x*Buffer->BytesPerPixel + starting_y*Pitch;
    
    for (int Y = starting_y; Y < ending_y; Y++) {        
        u32 *Pixel = (u32 *)Row;   
        for (int X = starting_x; X < ending_x; X++) {
            Color bg_color  = hex_to_color(*Pixel);
            Color new_color = interpolate_colors(color, bg_color, color.alpha/255.0);
            *Pixel = ((new_color.red << 16) | (new_color.green << 8) | new_color.blue);
            Pixel++;
        }
        Row += Pitch;
    }
}

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

int get_CPM() {
    LARGE_INTEGER counter_per_second;
    QueryPerformanceFrequency(&counter_per_second);
    return counter_per_second.QuadPart / 1000;
}

void maximize_window(HWND *Window)
{ // @Incomplete: If the taskbar is on the side the rect is not correct
    RECT rect;
    GetWindowRect(*Window, &rect);

    WINDOWPLACEMENT *placement = (WINDOWPLACEMENT *) malloc(sizeof(WINDOWPLACEMENT));
    placement->length           = sizeof(WINDOWPLACEMENT);
    placement->flags            = 0;
    placement->showCmd          = SW_MAXIMIZE;
    placement->rcNormalPosition = rect;
    
    SetWindowPlacement(*Window, placement);
    
    free(placement);
}

void restore_window(HWND *Window)
{
    WINDOWPLACEMENT *placement = (WINDOWPLACEMENT *) malloc(sizeof(WINDOWPLACEMENT));
    GetWindowPlacement(*Window, placement);
    placement->showCmd = SW_RESTORE;
    SetWindowPlacement(*Window, placement);
    free(placement);
}

void resize_window(HWND *Window) {
    WINDOWPLACEMENT *placement = (WINDOWPLACEMENT *) malloc(sizeof(WINDOWPLACEMENT));
    GetWindowPlacement(*Window, placement);
    
    if (placement->showCmd == SW_MAXIMIZE) restore_window(Window);
    else                                   maximize_window(Window);

    free(placement);
}

POINT get_mouse_position(HWND *Window) {
    POINT p, v;
    GetCursorPos(&p);

    RECT rect;
    GetClientRect(*Window, (LPRECT)&rect);

    ClientToScreen(*Window, (LPPOINT)&rect.left);
    ClientToScreen(*Window, (LPPOINT)&rect.right);
    
    v.x = p.x - rect.left;
    v.y = p.y - rect.top;
    
    return v;
}

#endif
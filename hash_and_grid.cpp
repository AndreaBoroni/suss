
/*
HWND Window = {0};

const Color WHITE = {255, 255, 255, 255};
const Color DARK_WHITE = {160, 160 ,160, 255};
const Color LIGHT_GRAY = {80, 80, 80, 255};
const Color GRAY = {50, 50, 50, 255};
const Color LIGHT_BLACK = {30, 30, 30, 255};
const Color BLACK = {0, 0, 0, 255};
const Color GREEN = {70, 200, 80, 255};
const Color DARK_GREEN = {50, 125, 60, 255};
const Color BROWN = {125, 80, 70, 255};
const Color PURPLE = {80, 70, 125, 255};
const Color RED = {255, 0, 0, 255};
const Color BLUE = {0, 0, 255, 255};
const Color CYAN = {0, 160, 255, 255};
const Color MAGENTA = {255, 0, 255, 255};
const Color PUPRLE = {160, 0, 215, 255};
const Color GOLD = {255, 210, 0, 255};
const Color INVISIBLE = {0, 0, 0, 0};
*/

/*
struct Hash_Data {
    String key;
    void  *value;

    bool removed;
};

struct Hash_Table {
    Hash_Data *data = NULL;

    int size = 0;
    int used_slots = 0;

    void init(int size);
    void add(String key, void *value);
    void remove(String key);
    void resize_and_rehash();
    void *value_of(String key);

    u32 hash_function(String key) {
        u32 hash = 5381;
        u8  c = key.data[0];

        for (u32 i = 0; i < key.count; i++) hash = ((hash << 5) + hash) + key.data[i];

        return hash % size;
    }

    int find(String key) {
        int hash = hash_function(key);
        int index = hash;
        while (true) {
            if (are_strings_equal(key, data[index].key)) return index;
            if (!data[index].removed)                    return -1;

            index++;
            index = index % size;

            if (hash == index) return -1; // Hopefully we expanded before this happens
        }
        return index;
    }
};

void Hash_Table::init(int table_size) {
    size = table_size;
    Assert(size > 0);

    used_slots = 0;

    data = (Hash_Data *) malloc(sizeof(Hash_Data) * size);
    memset(data, 0, sizeof(Hash_Data) * size);
}

void Hash_Table::resize_and_rehash() {
    int old_size = size;
    Hash_Data *old_data = data;

    // Todo: Check if new size is acceptable
    init(size * 2);

    for (int i = 0; i < old_size; i++) {
        if (old_data[i].key.data) add(old_data[i].key, old_data[i].value);
    }

    free(old_data);
}

void Hash_Table::add(String key, void *value) {
    if (find(key) != -1) return;
    used_slots++;

    if (used_slots > size * 0.7) {
        resize_and_rehash();
    }

    int hash = hash_function(key);
    int index = hash;

    while (data[index].key.data) {
        index++;
        index = index % size;

        Assert(index != hash); // we did the entire loop and the table is full
    }

    data[index].removed = false;
    data[index].value   = value;

    data[index].key.count = key.count;
    data[index].key.data  = (char *) malloc(key.count);
    memcpy(data[index].key.data, key.data, key.count);
}

void Hash_Table::remove(String key) {
    int index = find(key);
    if (index == -1) return;
    
    if (data[index].key.data) free(data[index].key.data);
    data[index].key.data  = 0;
    data[index].key.count = 0;
    data[index].value     = 0;
    data[index].removed   = true;

    used_slots--;

    return;
}

void *Hash_Table::value_of(String key) {
    int index = find(key);
    Assert(index != -1);

    return data[index].value;
}
*/

/*
struct Grid {
    int *mem = 0;
    
    int cols = 0;
    int rows = 0;

    int *pos(int c, int r) { return &mem[c + cols * r]; }
    bool in(int c, int r) {
        if (c < 0)     return false;
        if (r < 0)     return false;
        if (c >= cols) return false;
        if (r >= rows) return false;
        return true;
    }

};

void update_grid(Grid *grid) {

    for (int r = 0; r < grid->rows; r++) {
        for (int c = 0; c < grid->cols; c++) {
            int count = 0;
            int value = *grid->pos(c, r);

            Assert(value == 0 || value == 1);

            if (grid->in(c-1, r-1)) count += *grid->pos(c-1, r-1);
            if (grid->in(c,   r-1)) count += *grid->pos(c,   r-1);
            if (grid->in(c+1, r-1)) count += *grid->pos(c+1, r-1);
            if (grid->in(c-1, r))   count += *grid->pos(c-1, r);
            if (grid->in(c+1, r))   count += *grid->pos(c+1, r);
            if (grid->in(c-1, r+1)) count += *grid->pos(c-1, r+1);
            if (grid->in(c,   r+1)) count += *grid->pos(c,   r+1);
            if (grid->in(c+1, r+1)) count += *grid->pos(c+1, r+1);

            if (value) {
                if      (count <= 1) *grid->pos(c, r) = 0;
                else if (count <= 3) *grid->pos(c, r) = 1;
                else                 *grid->pos(c, r) = 0;
            } else if (count == 3) {
                *grid->pos(c, r) = 1;
            }
        }
    }
}
*/



    /*
    int dim[2] = {50, 30};

    Grid grid;
    grid.mem = (int *) malloc(dim[0] * dim[1] * sizeof(int));
    grid.cols = dim[0];
    grid.rows = dim[1];

    for (int i = 0; i < grid.cols * grid.rows; i++) grid.mem[i] = (rand() % 10 == 0);
    int side = 20;
    for (;;) {
        RECT full_rect = {0, 0, Buffer.Width, Buffer.Height};
        render_filled_rect(&Buffer, &full_rect, BLACK);
        
        if (!handle_messages()) return 0;

        int offset_x = 100;
        int offset_y = 100;
        RECT background = {offset_x-1, offset_y-1, offset_y + grid.cols * side, offset_y + grid.rows * side};
        render_filled_rect(&Buffer, &background, GRAY);

        update_grid(&grid);
        for (int i = 0; i < grid.cols * grid.rows; i++) {
            int x = i % grid.cols;
            int y = i / grid.cols;
            RECT rect = {offset_x + x * side, offset_y + y * side, offset_x + (x+1) * side-1, offset_y + (y+1) * side-1};
            
            Color c;
            if (grid.mem[i]) c = WHITE;
            else             c = BLACK;
            render_filled_rect(&Buffer, &rect, c);
        }

        DisplayBufferToWindow(Window, &Buffer);
        Sleep(200);
    }
    */
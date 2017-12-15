#include <stdbool.h>
#include <glib.h>

int SCREEN_WIDTH = 640;
int SCREEN_HEIGHT = 480;

const Uint8 *keystate = NULL;

GSList* element_list;

//Starts up SDL and creates window
bool init();

//Loads media
bool loadMedia();

bool isPressed(int keyId);

//Frees media and shuts down SDL
void close();

struct tuple{
    int left;
    int right;
};

struct color{
    int r;
    int g;
    int b;
};

struct element{
    char * name;
    struct tuple size;
    struct tuple position;
    char* el_color;
    // int direction;
    // float speed;
};

struct world{
    struct tuple size;
    char* back_color;
};
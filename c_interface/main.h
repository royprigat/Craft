#include <stdbool.h>
#include <glib.h>

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

const Uint8 *keystate = NULL;

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
    struct tuple size;
    struct tuple position;
    struct color el_color;
    int direction;
    float speed;
};

struct world{
    struct color back_color;
    GSList* list
};
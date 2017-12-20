#include <stdbool.h>
#include <glib.h>

int SCREEN_WIDTH = 640;
int SCREEN_HEIGHT = 480;

const Uint8 *keystate = NULL;

GSList* element_list;

GSList* fn_list = NULL;

//Starts up SDL and creates window
bool init();

//Loads media
bool loadMedia();

int isPressed(char* key);

// bool isCollision(struct element *e1, char* e2, )

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
    // void (*event_fn)(struct element*);
    int direction;
    int speed;
};

struct world{
    struct tuple size;
    char* back_color;
};
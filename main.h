#include <stdbool.h>
#include <glib.h>

int SCREEN_WIDTH = 640;
int SCREEN_HEIGHT = 480;

const Uint8 *keystate = NULL;

// Element list
GSList* element_list;

// List of callback functions for events
GSList* fn_list = NULL;

//Starts up SDL and creates window
bool init();

//Loads media
bool loadMedia();

int isPressed(char* key);

//The window we'll be rendering to
SDL_Window* gWindow = NULL;
    
//The surface contained by the window
SDL_Surface *gScreenSurface = NULL;

// Flag to know when to refresh window
int refresh = 0;

// The world struct
struct world *w;

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
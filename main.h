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

// Changes direction for elastic collisions of the element passed depending on the axis of collision.
void reflection(struct element *e, int axis);
// Returns 0 if 2 elements do not collide. Returns 1 ro 2 depending on the axis that the elements collide on if they do.
int doElementsCollide(struct element *e1, struct element *e2);
// Refresh object depending on inherent speed and direction specified in the struct
void moveSpeed(struct element *e);

void moveUp(struct element *e);
void moveDown(struct element *e);
void moveLeft(struct element *e);
void moveRight(struct element *e);

// Wrapper function for all 4 move functions
void move(char *name, char *direction);

// Adding the pointer for the callback function for an event to the event pointer list
void addEventfn(void (*a));

// returns the state of the key associated with the string passed to it
int isPressed(char *key);

// draw an element on the the screen
void render_element(struct element *e);

// intialiaze the world
void init_world(struct world *temp);

// add element to main element list
void add_element(struct element *e);

// refresh the world
void refresh_world();

// delete and alament from the main list
struct element* delete_element(char *name);

// Setup and initialize the world
bool init();

// Placeholder function to insert game logos, graphics, startup screens etc
bool loadMedia();

// Fn to close window and close SDL
void close();

// Main world function
int world();
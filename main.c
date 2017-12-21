#include "SDL2/SDL.h"
#include "main.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include  <sys/types.h>
#include <math.h>

//REF: http://lazyfoo.net/tutorials/SDL/04_key_presses/index.php
//REF: https://wiki.libsdl.org
//REF: http://gamedevgeek.com/tutorials/moving-sprites-with-sdl/

int speed = 3;

// Changes direction for elastic collisions of the element passed depending on the axis of collision.
void reflection(struct element *e, int axis){
    if(axis == 2){
        e->direction = 180 - e->direction;
    }else{
        e->direction = 360 - e->direction;
    }
    refresh = 1;
}

// Returns 0 if 2 elements do not collide. Returns 1 ro 2 depending on the axis that the elements collide on if they do.
int doElementsCollide(struct element *e1, struct element *e2){

    // printf("%s %s \n", e1->name, e2->name);
    if(e1->position.left > e2->position.left + e2->size.left || 
       e1->position.left + e1->size.left < e2->position.left ||
       e1->position.right > e2->position.right + e2->size.right ||
       e1->position.right + e1->size.right < e2->position.right){
        return 0;
    }
    
    // Calls reflection for elastic collisions in case of collision
    if((e1->position.left < e2->position.left + e2->size.left + 3)&&
        (e1->position.left +3 > e2->position.left + e2->size.left)){
            reflection(e1, 2);
            return 2; 
    }
    if((e1->position.left + e1->size.left + 3 > e2->position.left)&&
        (e1->position.left + e1->size.left < e2->position.left+3)){
            reflection(e1, 2);
            return 2; 
    }
    if((e1->position.right < e2->position.right + e2->size.right + 3) &&
       (e1->position.right +3 > e2->position.right + e2->size.right)){
            reflection(e1, 1);
            return 1;
    }
    if((e1->position.right + e1->size.right + 3 > e2->position.right) &&
        (e1->position.right + e1->size.right < e2->position.right + 3)){
            reflection(e1, 1);
            return 1;
        }
    
}

// Refresh object depending on inherent speed and direction specified in the struct
void moveSpeed(struct element *e){

    double radians = M_PI * (e->direction/180.0);
    e->position.left = e->position.left + round(e->speed*cos(radians));
    e->position.right = e->position.right + round(e->speed*sin(radians));

    GSList* iterator = NULL;
    
    // Loop through the elements to check if the future space is already occupied by something 
    for (iterator = element_list; iterator; iterator = iterator->next){
        struct element *temp = (struct element*)iterator->data;
        if(strcmp(e->name, temp->name)==0){
            continue;
        }
        if(doElementsCollide(e, temp)){
            e->position.left = e->position.left - round(e->speed*cos(radians));
            e->position.right = e->position.right - round(e->speed*sin(radians));
        } 
    
        refresh = 1;
    }   
}

void moveUp(struct element *e){
    if(e->position.right + e->size.right + speed < SCREEN_HEIGHT){
        e->position.right = e->position.right + speed;

        GSList* iterator = NULL;
        for (iterator = element_list; iterator; iterator = iterator->next)
        {
            if(strcmp(e->name, ((struct element*)iterator->data)->name)==0){
                continue;
            }
            if(doElementsCollide(e, (struct element*)iterator->data)){
                e->position.right = e->position.right - speed;
                return;
            }
        }
    }
}

void moveDown(struct element *e){
    if(e->position.right - speed >=0){
         e->position.right = e->position.right - speed;

        GSList* iterator = NULL;
        for (iterator = element_list; iterator; iterator = iterator->next)
        {
            if(strcmp(e->name, ((struct element*)iterator->data)->name)==0){
                continue;
            }
            if(doElementsCollide(e, (struct element*)iterator->data)){
                e->position.right = e->position.right + speed;
                return;
            }
        }
    }
}

void moveLeft(struct element *e){
    if(e->position.left - speed >=0){
        e->position.left = e->position.left - speed;

        GSList* iterator = NULL;
        for (iterator = element_list; iterator; iterator = iterator->next)
        {
            if(strcmp(e->name, ((struct element*)iterator->data)->name)==0){
                continue;
            }
            if(doElementsCollide(e, (struct element*)iterator->data)){
                e->position.left = e->position.left + speed;
                return;
            }
        }
    }
}

void moveRight(struct element *e){
    if(e->position.left + e->size.left + speed < SCREEN_WIDTH){
        e->position.left = e->position.left + speed;

        GSList* iterator = NULL;
        for (iterator = element_list; iterator; iterator = iterator->next)
        {
            if(strcmp(e->name, ((struct element*)iterator->data)->name)==0){
                continue;
            }
            if(doElementsCollide(e, (struct element*)iterator->data)){
                e->position.left = e->position.left - speed;
                return;
            }
        }
    }
}

void move(char *name, char *direction){

    struct element *e = NULL;
    GSList* iterator = NULL;
        
    for (iterator = element_list; iterator; iterator = iterator->next)
    {
        e = (struct element*)iterator->data;

        if(strcmp(e->name, name)!=0){
            e = NULL;
        }else{
            break;
        }
    }
    if(e==NULL){
        return;
    }

    refresh = 1;
    if (strcmp(direction, "UP") == 0 || strcmp(direction, "SUP") == 0){
        moveUp(e);
    }else if(strcmp(direction, "DOWN") == 0 || strcmp(direction, "SDOWN") == 0){
        moveDown(e);
    }else if(strcmp(direction, "LEFT") == 0 || strcmp(direction, "SLEFT") == 0){
        moveLeft(e);
    }else if(strcmp(direction, "RIGHT") == 0 || strcmp(direction, "SRIGHT") == 0) {
        moveRight(e);
    }
}

void (*event_fn)();

// Adding the pointer for the callback function for an event to the event pointer list
void addEventfn(void (*a)){
    fn_list = g_slist_append(fn_list, a);
}

// returns the state of the key associated with the string passed to it
int isPressed(char *key){
    int keyId;

    if (strcmp(key, "DOWN") == 0){
        keyId = 82;
    }else if(strcmp(key, "UP") == 0){
         keyId = 81;
    }else if(strcmp(key, "LEFT") == 0){
         keyId = 80;
    }else if(strcmp(key, "RIGHT") == 0) {
         keyId = 79;
    }else if(strcmp(key, "SPACE") == 0){
        keyId = 44;
    }else if (strcmp(key, "SDOWN") == 0){
        keyId = 26;
    }else if(strcmp(key, "SUP") == 0){
         keyId = 22;
    }else if(strcmp(key, "SLEFT") == 0){
         keyId = 4;
    }else if(strcmp(key, "SRIGHT") == 0) {
         keyId = 7;
    }
    if(keystate == NULL){
        return 0;
    }

    return keystate[keyId];
}

// draw an element on the the screen
void render_element(struct element *e) {
    SDL_Rect rect;
    rect.x = e->position.left;
    rect.y = e->position.right;
    rect.w = e->size.left;
    rect.h = e->size.right;
    SDL_FillRect(gScreenSurface, &rect, (int)strtol(e->el_color, NULL, 16));
}
// intialiaze the world 
void init_world(struct world *temp){
    w=temp;
    SCREEN_WIDTH = w->size.left;
    SCREEN_HEIGHT = w->size.right;
}

// add element to main element list
void add_element(struct element *e){
    element_list = g_slist_append(element_list, e);
}

// refresh the world
void refresh_world(){
    SDL_FillRect(gScreenSurface, NULL, (int)strtol(w->back_color, NULL, 16));
}

// delete and alament from the main list
struct element* delete_element(char *name){
   
    struct element *e = NULL;
    GSList* iterator = NULL;
      
    for (iterator = element_list; iterator; iterator = iterator->next)
    {
        e = (struct element*)iterator->data;
        if(strcmp(e->name, name)!=0){
            e = NULL;
        }else{
            break;
        }
    }

    if(e !=NULL){
        element_list = g_slist_remove(element_list, iterator->data);
        refresh = 1;
        return e;
    }
    return NULL;
    
}

bool init()
{   
    //Initialization flag
    bool success = true;

    //Initialize SDL
    if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
        success = false;
    }
    else
    {
        //Create window
        gWindow = SDL_CreateWindow( "Craft", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN );
        // gWindow = SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, 0, 0);
        if( gWindow == NULL )
        {
            printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
            success = false;
        }
        else
        {
            //Get window surface
            gScreenSurface = SDL_GetWindowSurface( gWindow );
        }
    }

    return success;
}

// Placeholder function to insert game logos, graphics, startup screens etc
bool loadMedia()
{
    //Loading success flag
    bool success = true;

    //Load splash image
    // gHelloWorld = SDL_LoadBMP( "hello_world.bmp" );
    // if( gHelloWorld == NULL )
    // {
    //     printf( "Unable to load image %s! SDL Error: %s\n", "02_getting_an_image_on_the_screen/hello_world.bmp", SDL_GetError() );
    //     success = false;
    // }
    return success;
}

// Close and destroy the window
void close()
{
    //Destroy window
    printf("Exiting!!");
    SDL_DestroyWindow( gWindow );
    gWindow = NULL;

    //Quit SDL subsystems
    SDL_Quit();
}

int world( )
{
    //Start up SDL and create window
    if( !init() )
    {
        printf( "Failed to initialize!\n" );
    }
    else
    {
        //Load media
        if( !loadMedia() )
        {
            printf( "Failed to load media!\n" );
        }
        else
        {
            //Make the world of the size and backgorund
            SDL_FillRect(gScreenSurface, NULL, (int)strtol(w->back_color, NULL, 16));
            // Update the surface
            SDL_UpdateWindowSurface( gWindow );
            //Wait two seconds
            SDL_Delay( 2000 );
        }

        //Event handler
        SDL_Event e;

        bool quit = false;

        while(!quit){

            while( SDL_PollEvent (&e) != 0 ){

                //User requests quit
                if( e.type == SDL_QUIT ){
                    quit = true;
                }

                //User presses a key
                else if( e.type == SDL_KEYDOWN )
                {
                    //Select surfaces based on key press
                    switch( e.key.keysym.sym )
                    {
                        case SDLK_ESCAPE:
                        
                        case SDLK_q:
                            quit = true;
                            break;
                    }
                }
            }

            GSList* iterator = NULL;
            // Iterate over the elements, update the positions of elements with defined speeds
            for (iterator = element_list; iterator; iterator = iterator->next)
            {
                render_element((struct element*)iterator->data);
            }
            
            // Iterate over the elements, re-render them.
            iterator = NULL;
            for (iterator = element_list; iterator; iterator = iterator->next)
            {   
                moveSpeed((struct element*)iterator->data);
            }

            // Show changes on the window
            SDL_UpdateWindowSurface( gWindow );

            // Update the keymap
            keystate = SDL_GetKeyboardState(NULL);

            iterator = NULL;

            // Iterate over and call the callback functions
            for (iterator = fn_list; iterator; iterator = iterator->next)
            {
                void (*temp)() = (void *)iterator->data;
                temp();
            }

            // If the world needs to be refreshed, clean the slate
            if(refresh){
                refresh_world();
                refresh = 0;
            }

            // Delay to slow down the loop to a reasonable level
            SDL_Delay(5);
        }
    }

    return 0;
}
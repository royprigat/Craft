#include "SDL2/SDL.h"
#include "main.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include  <sys/types.h>
#include <math.h>

bool shouldStart = true;

//REF: http://lazyfoo.net/tutorials/SDL/04_key_presses/index.php
//REF: https://wiki.libsdl.org
//REF: http://gamedevgeek.com/tutorials/moving-sprites-with-sdl/
//Screen dimension constants

//The window we'll be rendering to
SDL_Window* gWindow = NULL;
    
//The surface contained by the window
SDL_Surface *gScreenSurface = NULL;

struct world *w;

int refresh = 0;
// SDL_Surface *player = NULL;

//The image we will load and show on the screen
// SDL_Surface* gHelloWorld = NULL;

// SDL_Renderer* renderer = NULL;

int speed = 2;

void reflection(struct element *e, int axis){
    // printf("reflection axis %d", axis);
    if(axis == 2){
        // printf("%d %d \n", e->direction, 180 - e->direction);
        e->direction = 180 - e->direction;
    }else{
        // printf("%d %d \n", e->direction, 360 - e->direction);
        e->direction = 360 - e->direction;
    }
    refresh = 1;
}

int doElementsCollide(struct element *e1, struct element *e2){

    // printf("%s %s \n", e1->name, e2->name);
    if(e1->position.left > e2->position.left + e2->size.left || 
       e1->position.left + e1->size.left < e2->position.left ||
       e1->position.right > e2->position.right + e2->size.right ||
       e1->position.right + e1->size.right < e2->position.right){
        return 0;
    }
    if((e1->position.left < e2->position.left + e2->size.left + 3)&&
        (e1->position.left +3 > e2->position.left + e2->size.left)){
        printf("trip1\n");
        reflection(e1, 2);
        return 2; 
    }
    if((e1->position.left + e1->size.left + 3 > e2->position.left)&&
        (e1->position.left + e1->size.left < e2->position.left+3)){
        printf("trip2\n");
        reflection(e1, 2);
        return 2; 
    }
    if((e1->position.right < e2->position.right + e2->size.right + 3) &&
       (e1->position.right +3 > e2->position.right + e2->size.right)){
        printf("trip3\n");
        reflection(e1, 1);
        return 1;
    }
    if((e1->position.right + e1->size.right + 3 > e2->position.right) &&
        (e1->position.right + e1->size.right < e2->position.right + 3)){
            printf("trip4\n");
            reflection(e1, 1);
            return 1;
        }
    
}

void moveSpeed(struct element *e){

    double radians = M_PI * (e->direction/180.0);
    e->position.left = e->position.left + round(e->speed*cos(radians));
    e->position.right = e->position.right + round(e->speed*sin(radians));

    GSList* iterator = NULL;
    
    for (iterator = element_list; iterator; iterator = iterator->next)
    {
        struct element *temp = (struct element*)iterator->data;
        if(strcmp(e->name, temp->name)==0){
            continue;
        }
        // printf("%d  %f\n", e->direction, radians);
        if(doElementsCollide(e, temp)){
        // printf("COLLISION\n");
            e->position.left = e->position.left - round(e->speed*cos(radians));
            e->position.right = e->position.right - round(e->speed*sin(radians));
        } 
    
        if(doElementsCollide(e, temp)){
        // printf("COLLISION\n");
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
            // printf("COLLISION\n");
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
            // printf("COLLISION\n");
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
            // printf("COLLISION\n");
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
            // printf("COLLISION\n");
                e->position.left = e->position.left - speed;
                return;
            }
        }
    }
}



void move(char *name, char *direction){

    struct element *e = NULL;
    GSList* iterator = NULL;
        // render_element(&ele);
    for (iterator = element_list; iterator; iterator = iterator->next)
    {
        e = (struct element*)iterator->data;
        // printf("\nELEMENT FOUND%s %s \n", e->name, e->el_color);
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

void addEventfn(void (*a)){
    // printf("Adding event fn");
    fn_list = g_slist_append(fn_list, a);
}

void test_print(){
    printf("%s", "Roses are red\n Violets are blue\n The learning curve is steep\n Screw you");
}

void startRender(){
    shouldStart = true;
}
int isPressed(char *key){
    int keyId;
    // printf("%s\n",key );

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
    // printf("Returning value of keypress:%s==%d", key, keystate[keyId]);
    return keystate[keyId];
}

void render_element(struct element *e) {
    SDL_Rect rect;
    rect.x = e->position.left;
    rect.y = e->position.right;
    rect.w = e->size.left;
    rect.h = e->size.right;
    SDL_FillRect(gScreenSurface, &rect, (int)strtol(e->el_color, NULL, 16));
}
void init_world(struct world *temp){
    // w = malloc (sizeof (struct world));
    w=temp;
    // w->list = NULL;
    SCREEN_WIDTH = w->size.left;
    SCREEN_HEIGHT = w->size.right;
}
void add_element(struct element *e){
    // printf( "add_element called in C\n" );
    element_list = g_slist_append(element_list, e);
}

// void delete_element(struct element *e){
//     element_list = g_list_remove(element_list. e);
//     free(e->el_color);
//     free(e);
// }
void refresh_world(){
    SDL_FillRect(gScreenSurface, NULL, (int)strtol(w->back_color, NULL, 16));
}

struct element* delete_element(char *name){
    printf("Before %d", g_slist_length(element_list));
    struct element *e = NULL;
    GSList* iterator = NULL;
        // render_element(&ele);
    for (iterator = element_list; iterator; iterator = iterator->next)
    {
        e = (struct element*)iterator->data;
        printf("\nELEMENT FOUND%s %s \n", e->name, e->el_color);
        if(strcmp(e->name, name)!=0){
            printf("Before test1\n");
            e = NULL;
        }else{
            printf("Before test2\n");
            break;
        }
        printf("Before test3\n");
    }
    printf("Before test4\n");
    if(e !=NULL){
        printf("Before test5\n");
        element_list = g_slist_remove(element_list, iterator->data);
        printf("After%d", g_slist_length(element_list));
        printf("%s", e->el_color);
        refresh = 1;
        printf("Before return e in del. elem.\n");
        return e;
        // free(e->el_color);
        // free(e);
    }
    return NULL;
    
}

bool init()
{   
    // struct tuple s = {400,500};
    // init_world(s, "FFFFFF");
    // GHashTable* hash = g_hash_table_new(g_str_hash, g_str_equal);
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
            // renderer =  SDL_CreateRenderer( gWindow, -1, 0);
            // SDL_RenderSetLogicalSize( renderer, SCREEN_WIDTH, SCREEN_HEIGHT );
        }
    }

    return success;
}

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

void close()
{
    // //Deallocate surface
    // SDL_FreeSurface( gHelloWorld );
    // gHelloWorld = NULL;

    //Destroy window
    SDL_DestroyWindow( gWindow );
    gWindow = NULL;

    //Quit SDL subsystems
    SDL_Quit();
}

int world( )
{
    // testfn();
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
            //Apply the image
            // SDL_BlitSurface( gHelloWorld, NULL, gScreenSurface, NULL );
            SDL_FillRect(gScreenSurface, NULL, (int)strtol(w->back_color, NULL, 16));
            // Update the surface
            SDL_UpdateWindowSurface( gWindow );

            //Wait two seconds
            SDL_Delay( 2000 );
        }

        // struct element ele = {20, 20, 10, 10, "aabbcc", 1, 1};
        // struct element ele1 = {20, 20, 70, 70, "bbbbbb", 1, 1};
        // element_list = g_slist_append(element_list, &ele);
        // element_list = g_slist_append(element_list, &ele1);
       
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
            // render_element(&ele);
            for (iterator = element_list; iterator; iterator = iterator->next)
            {
                moveSpeed((struct element*)iterator->data);
                render_element((struct element*)iterator->data);
            }
        
            SDL_UpdateWindowSurface( gWindow );

            keystate = SDL_GetKeyboardState(NULL);

            iterator = NULL;

            for (iterator = fn_list; iterator; iterator = iterator->next)
            {
                void (*temp)() = (void *)iterator->data;
                // printf("%s", "Entering ITERATOR");
                temp();
            }

            if(refresh){
                refresh_world();
                refresh = 0;
            }
            SDL_Delay(5);

            // printf("%d ", SDLK_LEFT);
        }
    }

    //Free resources and close SDL
    // close();

    return 0;
}

// int main(int argc, char ** argv){
//         struct element ele = {"one", {20, 20}, {10, 10}, "aabbcc", 1, 1};
//         struct element ele1 = {"two", 20, 20, 70, 70, "bbbbbb", 1, 1};
//         struct element ele2 = {"three", 20, 20, 70, 70, "333333", 1, 1};
        
//         struct world t = {400,400, "aaaaaa"};
//         init_world(&t);
//         add_element(&ele);
//         add_element(&ele1);
//         // world();
//         // if(fork()==0){
//         //     world();
//         // }
//         // printf("BACK to main\n");
//         // SDL_Delay(10000);
//         delete_element("one");
//         add_element(&ele2);
//         world();
        

//     return 0;
// }
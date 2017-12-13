#include "SDL2/SDL.h"
#include "main.h"
#include <stdio.h>

//REF: http://lazyfoo.net/tutorials/SDL/04_key_presses/index.php
//REF: https://wiki.libsdl.org
//REF: http://gamedevgeek.com/tutorials/moving-sprites-with-sdl/
//Screen dimension constants

//The window we'll be rendering to
SDL_Window* gWindow = NULL;
    
//The surface contained by the window
SDL_Surface *gScreenSurface = NULL;

struct world *w;
// SDL_Surface *player = NULL;

//The image we will load and show on the screen
// SDL_Surface* gHelloWorld = NULL;

// SDL_Renderer* renderer = NULL;

bool isPressed(int keyId){
    return keystate[keyId];
}

void render_element(struct element *e) {
    SDL_Rect rect;
    rect.x = e->position.left;
    rect.y = e->position.right;
    rect.w = e->size.left;
    rect.h = e->size.right;

    SDL_FillRect(gScreenSurface, &rect, SDL_MapRGB(gScreenSurface->format, e->el_color.r, e->el_color.g, e->el_color.b));
}
void init_world(struct tuple *size, struct color *c){
    w = malloc (sizeof (struct world));
    SCREEN_WIDTH = size.left;
    SCREEN_HEIGHT = size.right;
}
void add_element(struct element *e){
    w ->list = g_slist_append(w ->list, e);
}


bool init()
{   
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
        gWindow = SDL_CreateWindow( "Sandbox", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN );
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

int main( int argc, char* argv[] )
{
    
    // list = NULL;

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
            SDL_FillRect(gScreenSurface, NULL, 0xFFFFFF);
            // Update the surface
            SDL_UpdateWindowSurface( gWindow );

            //Wait two seconds
            SDL_Delay( 2000 );
        }
        struct color c = {255, 0, 255};
        struct element ele = {70, 70, 10, 10, c, 1, 1};
        // list = g_slist_append(list, &ele);
        render_element(&ele);
        SDL_UpdateWindowSurface( gWindow );
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

            keystate = SDL_GetKeyboardState(NULL);

            // printf("%d ", SDLK_LEFT);
        }
    }

    //Free resources and close SDL
    // close();

    return 0;
}
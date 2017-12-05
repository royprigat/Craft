#include "SDL2/SDL.h"
#include "main.h"
#include <stdio.h>
#include <glib.h>

//REF: http://lazyfoo.net/tutorials/SDL/04_key_presses/index.php
//REF: https://wiki.libsdl.org
//REF: http://gamedevgeek.com/tutorials/moving-sprites-with-sdl/
//Screen dimension constants


//The window we'll be rendering to
SDL_Window* gWindow = NULL;
    
//The surface contained by the window
SDL_Surface *gScreenSurface = NULL;
// SDL_Surface *player = NULL;

//The image we will load and show on the screen
// SDL_Surface* gHelloWorld = NULL;

// SDL_Renderer* renderer = NULL;

void render_element(){

    printf( "Reached render" );
    // SDL_FreeSurface(player);


    // SDL_SetRenderDrawColor( renderer, 255, 255, 255, 255 );

    // // Clear winow
    // SDL_RenderClear( renderer );
    // SDL_SetRenderDrawColor( renderer, 255, 0, 0, 255 );
    SDL_Rect r;
    r.x = 50;
    r.y = 50;
    r.w = 50;
    r.h = 50;

    // Render rect
    // SDL_RenderFillRect( renderer, &r );

    // // Render the rect to the screen
    // SDL_RenderPresent(renderer);
    // SDL_Delay(3000);
    SDL_FillRect(gScreenSurface, &r, SDL_MapRGB(gScreenSurface->format, 255, 0, 0));
};


bool init()
{   
    GHashTable* hash = g_hash_table_new(g_str_hash, g_str_equal);
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
            render_element();
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
            
            //Update the surface
            // SDL_UpdateWindowSurface( gWindow );

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
                        case SDLK_UP:
                        printf("%s\n","UP" );
                        break;

                        case SDLK_DOWN:
                        printf("%s\n", "DOWN");
                        break;

                        case SDLK_LEFT:
                        printf("%s\n", "LEFT");
                        break;

                        case SDLK_RIGHT:
                        printf("%s\n", "RIGHT" );
                        break;

                        default:
                        printf("%s\n", "DEFAULT" );
                        break;
                    }
                }
            }
        }
    }

    //Free resources and close SDL
    // close();

    return 0;
}
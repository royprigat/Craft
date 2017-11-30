//Let's make a hello world pop up

#include "SDL2/SDL.h"
#include <stdio.h>
#include <stdbool.h>

//REF: http://lazyfoo.net/tutorials/SDL/04_key_presses/index.php
//Screen dimension constants
const int SCREEN_WIDTH = 100;
const int SCREEN_HEIGHT = 100;

//Starts up SDL and creates window
bool init();

//Loads media
bool loadMedia();

//Frees media and shuts down SDL
void close();

//The window we'll be rendering to
SDL_Window* gWindow = NULL;
    
//The surface contained by the window
SDL_Surface* gScreenSurface = NULL;

//The image we will load and show on the screen
SDL_Surface* gHelloWorld = NULL;

// struct screen_size {
//     int height;
//     int width;
// };


struct pair{
    int left;
    int right;
};

struct color{
    int r;
    int g;
    int b;
};

struct element{
    struct pair size;
    struct pair position;
    struct color el_color;
    int direction;
    float speed;
};

struct world{
    struct color back_color;
    struct pair screen_size;
    
};




bool init(struct world* world_ptr)
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
        // gWindow = SDL_CreateWindow( "Craft", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN );
        gWindow = SDL_CreateWindow( "Craft", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, world_ptr->screen_size.left, world_ptr->screen_size.right, SDL_WINDOW_SHOWN );
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

bool loadMedia()
{
    //Loading success flag
    bool success = true;

    //Load splash image
    gHelloWorld = SDL_LoadBMP( "hello_world.bmp" );
    if( gHelloWorld == NULL )
    {
        printf( "Unable to load image %s! SDL Error: %s\n", "02_getting_an_image_on_the_screen/hello_world.bmp", SDL_GetError() );
        success = false;
    }

    return success;
}

bool fillWindow(struct world* world_ptr)
{
    //Loading success flag
    bool success = true;

    /* Creating the surface. */
    // gHelloWorld = SDL_CreateRGBSurface(0, SCREEN_WIDTH, SCREEN_HEIGHT, 32, 0, 0, 0, 0);
    gHelloWorld = SDL_CreateRGBSurface(0, world_ptr->screen_size.left, world_ptr->screen_size.right, 32, 0, 0, 0, 0);

    /* Filling the surface with red color. */
    SDL_FillRect(gHelloWorld, NULL, SDL_MapRGB(gHelloWorld->format, 255, 0, 0));

    return success;
}

void close()
{
    //Deallocate surface
    SDL_FreeSurface( gHelloWorld );
    gHelloWorld = NULL;

    //Destroy window
    SDL_DestroyWindow( gWindow );
    gWindow = NULL;

    //Quit SDL subsystems
    SDL_Quit();
}

void world(struct world* world_ptr)
{

    // printf("%d\n", test_int );
    printf("%d\n", world_ptr->screen_size.left); //height is right? y-value...
    printf("%d\n", world_ptr->screen_size.right);

    //Start up SDL and create window
    if( !init(world_ptr) )
    {
        printf( "Failed to initialize!\n" );
    }
    else
    {
        //Load media
        //if( !loadMedia() )
        if ( !fillWindow(world_ptr) )
        {
            //printf( "Failed to load media!\n" );
            printf( "fillWindow failed!\n" );
        }
        else
        {
            //Apply the image
            SDL_BlitSurface( gHelloWorld, NULL, gScreenSurface, NULL );
            
            //Update the surface
            SDL_UpdateWindowSurface( gWindow );

            //Wait two seconds
            //SDL_Delay( 2000 );
        
        }
        //Event handler
        SDL_Event event;
        bool done = false;

        while(!done) {
            while(SDL_PollEvent(&event)){

                // if(ptr->height == 500) {
                //     printf("test 212"); //test
                // }

                //User requests quit
                if( event.type == SDL_QUIT ){
                    close();
                    done = true;
                }

                // //User presses a key
                // else if( e.type == SDL_KEYDOWN )
                // {
                //     //Select surfaces based on key press
                //     switch( e.key.keysym.sym )
                //     {
                //         case SDLK_UP:
                //         printf("%s\n","UP" );
                //         break;

                //         case SDLK_DOWN:
                //         printf("%s\n", "DOWN");
                //         break;

                //         case SDLK_LEFT:
                //         printf("%s\n", "LEFT");
                //         break;

                //         case SDLK_RIGHT:
                //         printf("%s\n", "RIGHT" );
                //         break;

                //         default:
                //         printf("%s\n", "DEFAULT" );
                //         break;
                //     }
                // }

            }
        }

    }

    //Free resources and close SDL
    // close();

    
}
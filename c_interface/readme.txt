May have to make with this for now: gcc main.c -L/usr/local/lib -lSDL2

gcc main.c -Wall -o test `pkg-config --cflags --libs glib-2.0`

gcc main.c -Wall -o test -I /usr/local/include  -L/usr/local/lib -lSDL2 `pkg-config --cflags --libs glib-2.0`

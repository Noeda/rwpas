#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

void set_window_size( const int w, const int h )
{
    struct winsize ws;
    memset( &ws, 0, sizeof(struct winsize) );

    ws.ws_row = h;
    ws.ws_col = w;

    ioctl( STDIN_FILENO, TIOCSWINSZ, &ws );
}

void get_window_size( int* w, int* h)
{
    struct winsize ws;
    memset( &ws, 0, sizeof(struct winsize) );

    ioctl( STDIN_FILENO, TIOCGWINSZ, &ws );

    (*w) = ws.ws_col;
    (*h) = ws.ws_row;
}


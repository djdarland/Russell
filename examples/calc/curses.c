#include	<curses.h>
#define		MAXWINDOWS 10

WINDOW *get_newwin(w,x,y,z)
int w,x,y,z;
{
    return(newwin(w,x,y,z));
}

putwin(w,s)
WINDOW *w;
char *s;
{
    wprintw(w,"%s",s);
}

int gety(w)
WINDOW *w;
{
    int y,x;
    getyx(w,y,x);
    return (y);
}

int getx(w)
WINDOW *w;
{
    int y,x;
    getyx(w,y,x);
    return (x);
}

char win_getch(w)
WINDOW *w;
{
    char c;
    int x,y;

    getyx(w,y,x);
    c = mvwgetch(w,y,x);
    return(c);
}

nonlM() { nonl(); }
nlM() { nl(); }
nocbreakM() { nocbreak(); }
cbreakM() { cbreak(); }
noechoM() { noecho(); }
echoM() { echo(); }

clearM() { clear(); }
refreshM() { refresh(); }

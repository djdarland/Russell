# define VERBOSE
# undef VERBOSE

/* Some incredibly grubby routines to determine screen size */
/* This should be easy, but ...                             */
#include <stdio.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/ioctl.h>

short ospeed;     /* Used only for padding.  Should be conservative. */

char termbuf[1024];  /* entire termcap entry */

char cap_buf[1024];  /* string space for individual capabilities */

static char * free = cap_buf;  /* Pointer to next available space */

char * term_name;

extern int LINES;

extern int COLS;

static int is_sun;  /* We're talking to a suntools window */

static int is_x;    /* We're talking to an X window       */

static int init_flag = 0;

static int Forced_resize = 0;

int stdout_is_terminal;

extern char * getenv();
extern char * tgetstr();
extern int tgetnum();


/* Set terminal type to that specified in the environment */
static void tsetup()
{
    int retcode;
    int (*Osigfunc)();
    unsigned long Omask;

    if (!init_flag) {
      term_name = getenv("TERM");
      if (term_name == (char *) 0) {
        fprintf(stderr, "No TERM variable in environment\n");
        exit(1);
      }
      retcode = tgetent(termbuf, term_name);
      if (retcode == -1) {
	fprintf(stderr, "No termcap file!!\r\n");
        exit(1);
      }
      if (retcode == 0) {
	fprintf(stderr, "Unknown terminal type\r\n");
        exit(1);
      }
      init_flag = 1;
      is_sun = (strcmp(term_name,"sun") == 0);
      is_x = (strcmp(term_name, "xterm") == 0 ||
	      strcmp(term_name, "xterms") == 0);
    }
    LINES = 0;
    COLS = 0;
#   ifdef TIOCGWINSZ
      /* Try the IOCTL first ... */
      {
	struct winsize winsz;

	if (ioctl(1, TIOCGWINSZ, &winsz) >= 0) {
	    LINES = winsz.ws_row;
	    COLS = winsz.ws_col;
	}
      }
#   endif
    if (LINES == 0) {
	LINES = tgetnum("li");
    }
    if (COLS == 0) {
	COLS = tgetnum("co");
    }
    /* Compensate for a curses bug.  >= 128 columns appears as 0 */
    /* Also expand absurdly small windows.                       */
    /* This is done using suntools window escape sequences.  It  */
    /* should probably be done in a more portable way using      */
    /* TIOCSWINSZ.  However, that may not work for old versions  */
    /* of SunOS, so we leave it for now.                         */
    /* The signal resulting from the size change must be         */
    /* handled by the caller.                                    */
	if (is_sun && COLS == 0) {
	    /* Set width to 127 */
		COLS = 127;
		printf("\033[8;%d;%dt", LINES, COLS);
		fflush(stdout);
		Forced_resize = 1;
	}
	if (is_sun && COLS < 18) {
	    COLS = 18;
	    printf("\033[8;%d;%dt", LINES, COLS);
	    fflush(stdout);
	    Forced_resize = 1;
	}
	if (is_sun && LINES < 8) {
	    LINES = 8;
	    printf("\033[8;%d;%dt", LINES, COLS);
	    fflush(stdout);
	    Forced_resize = 1;
	}
	if (LINES < 8 || COLS < 18) {
	    /* Maybe should try TIOCSWINSZ here? */
	    fprintf(stderr, "Window too small");
	    sleep(5);
	    tsetup();
	}
#   ifdef VERBOSE
	fprintf(stderr, "Setting to %d, %d\r\n", LINES, COLS);
	sleep(3);
#   endif
}

/* Return the number of lines on the screen */
long get_lines()
{
    if (!init_flag || is_sun || is_x) {
      tsetup();
    }
    return(LINES);
}


/* Return the number of columns on the screen */
long get_columns()
{
    if (!init_flag || is_sun || is_x) {
      tsetup();
    }
    return(COLS);
}

/* Return true if we're running in an X window. */
int is_x_win()
{
    if (!init_flag) {
	tsetup();
    }
    return(is_x);
}

/* Return true if a window resize was forced.  Clear Forced_resize */
long forced_resize()
{
    int result = Forced_resize;
    
    Forced_resize = 0;
    return(result);
}

/* Block SIGWINCH. Save old mask in saved_mask */
static unsigned long saved_mask;

void block_sigwinch()
{
    saved_mask = sigblock(sigmask(SIGWINCH));
}

/* Unblock SIGWINCH by restoring old signal mask */
void unblock_sigwinch()
{
    sigsetmask(saved_mask);
}

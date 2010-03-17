/* settings */
static const int   DEFAULT_WIDTH  = 800;
static const int   DEFAULT_HEIGHT = 600;
static const float ZOOM_STEP      = 10;
static const float ZOOM_MIN       = 10;
static const float ZOOM_MAX       = 400;
static const float SCROLL_STEP    = 40;
static const float TRANSPARENCY   = 0.4;

/* completion */
static const char FORMAT_COMMAND[]     = "<b>%s</b>";
static const char FORMAT_DESCRIPTION[] = "<i>%s</i>";

/* directories and files */
static const char ZATHURA_DIR[]   = ".config/zathura";
static const char BOOKMARK_FILE[] = "bookmarks";

/* bookmarks */
static const char BM_PAGE_ENTRY[] = "page";

/* look */
static const char font[]                   = "fixed medium 6";
static const char default_bgcolor[]        = "#090909";
static const char default_fgcolor[]        = "#DDDDDD";
static const char inputbar_bgcolor[]       = "#141414";
static const char inputbar_fgcolor[]       = "#67d797";
static const char statusbar_bgcolor[]      = "#090909";
static const char statusbar_fgcolor[]      = "#a488d9";
static const char completion_fgcolor[]     = "#DDDDDD";
static const char completion_bgcolor[]     = "#232323";
static const char completion_g_fgcolor[]   = "#DEDEDE";
static const char completion_g_bgcolor[]   = "#625873";
static const char completion_hl_fgcolor[]  = "#232323";
static const char completion_hl_bgcolor[]  = "#67d797";
static const char notification_e_bgcolor[] = "#b8ddea";
static const char notification_e_fgcolor[] = "#090909";
static const char notification_w_bgcolor[] = "#625873";
static const char notification_w_fgcolor[] = "#090909";

static const char search_highlight[]       = "#b8ddea";

/* statusbar */
static const char DEFAULT_TEXT[] = "[No Name]";

/* printing */
#define LIST_PRINTER_COMMAND "lpstat -v | sed -n '/^.*device for \\(.*\\): .*$/s//\\1/p'"
#define PRINT_COMMAND "lp -d '%s' -p %s '%s'" /* printer / pages / file */

/* additional settings */
#define SHOW_SCROLLBARS 0
#define ADJUST_OPEN ADJUST_BESTFIT

/* shortcuts */
Shortcut shortcuts[] = {
  /* mask,             key,               function,             mode,     argument */
  {GDK_CONTROL_MASK,   GDK_t,             sc_toggle_statusbar,  NORMAL,   {0} },
  {GDK_CONTROL_MASK,   GDK_m,             sc_toggle_inputbar,   NORMAL,   {0} },
  {GDK_CONTROL_MASK,   GDK_c,             sc_abort,             -1,       {0} },
  {GDK_CONTROL_MASK,   GDK_i,             sc_revert_video,      NORMAL,   {0} },
  {0,                  GDK_slash,         sc_focus_inputbar,    NORMAL,   { .data = "/" } },
  {GDK_SHIFT_MASK,     GDK_slash,         sc_focus_inputbar,    NORMAL,   { .data = "/" } },
  {GDK_SHIFT_MASK,     GDK_question,      sc_focus_inputbar,    NORMAL,   { .data = "?" } },
  {0,                  GDK_Tab,           sc_toggle_index,      NORMAL,   {0} },
  {0,                  GDK_J,             sc_navigate,          NORMAL,   { NEXT } },
  {0,                  GDK_K,             sc_navigate,          NORMAL,   { PREVIOUS } },
  {0,                  GDK_Escape,        sc_abort,             -1,       {0} },
  {0,                  GDK_i,             sc_change_mode,       NORMAL,   { INSERT } },
  {0,                  GDK_v,             sc_change_mode,       NORMAL,   { VISUAL } },
  {0,                  GDK_m,             sc_change_mode,       NORMAL,   { ADD_MARKER } },
  {0,                  GDK_apostrophe,    sc_change_mode,       NORMAL,   { EVAL_MARKER } },
  {0,                  GDK_colon,         sc_focus_inputbar,    NORMAL,   { .data = ":" } },
  {0,                  GDK_o,             sc_focus_inputbar,    NORMAL,   { .data = ":open " } },
  {GDK_CONTROL_MASK,   GDK_p,             sc_rotate,            NORMAL,   {0} },
  {0,                  GDK_h,             sc_scroll,            NORMAL,   { LEFT } },
  {0,                  GDK_j,             sc_scroll,            NORMAL,   { UP } },
  {0,                  GDK_k,             sc_scroll,            NORMAL,   { DOWN } },
  {0,                  GDK_l,             sc_scroll,            NORMAL,   { RIGHT } },
  {0,                  GDK_period,        sc_search,            NORMAL,   { FORWARD } },
  {0,                  GDK_comma,         sc_search,            NORMAL,   { BACKWARD } },
  {0,                  GDK_a,             sc_adjust_window,     NORMAL,   { ADJUST_BESTFIT } },
  {0,                  GDK_w,             sc_adjust_window,     NORMAL,   { ADJUST_WIDTH } },
  {0,                  GDK_BackSpace,     sc_change_buffer,     -1,       { DELETE_LAST } },
};

/* inputbar shortcuts */
InputbarShortcut inputbar_shortcuts[] = {
  /* mask,             key,               function,                  argument */
  {0,                  GDK_Escape,        isc_abort,                 {0} },
  {GDK_CONTROL_MASK,   GDK_r,             isc_command_history,       {0} },
  {0,                  GDK_Tab,           isc_completion,            { NEXT } },
  {GDK_CONTROL_MASK,   GDK_Tab,           isc_completion,            { NEXT_GROUP } },
  {0,                  GDK_ISO_Left_Tab,  isc_completion,            { PREVIOUS } },
  {GDK_CONTROL_MASK,   GDK_ISO_Left_Tab,  isc_completion,            { PREVIOUS_GROUP } },
  {GDK_CONTROL_MASK,   GDK_e,             isc_string_manipulation,   { DELETE_LAST_WORD } },
};

/* commands */
Command commands[] = {
  /* command,   abbreviation,   function,            completion,   description  */
  {"bmark",     "b",            cmd_bookmark,        0,            "Bookmark current page" },
  {"blist",     0,              cmd_open_bookmark,   cc_bookmark,  "List and open bookmark" },
  {"close",     "c",            cmd_close,           0,            "Close current file" },
  {"delbmark",  0,              cmd_delete_bookmark, cc_bookmark,  "Bookmark current page" },
  {"export",    "e",            cmd_export,          cc_export,    "Export images or attached files" },
  {"info",      "i",            cmd_info,            0,            "Show information about the document" },
  {"open",      "o",            cmd_open,            cc_open,      "Open a file" },
  {"print",     "p",            cmd_print,           cc_print,     "Print the document" },
  {"rotate",    "r",            cmd_rotate,          0,            "Rotate the page" },
  {"set",       "s",            cmd_set,             cc_set,       "Set an option" },
  {"quit",      "q",            cmd_quit,            0,            "Quit zathura" },
  {"write",     "w",            cmd_save,            0,            "Save the document" },
};

/* buffer commands */
BufferCommand buffer_commands[] = {
  /* regex,        function,       argument */
  {"^gg$",         bcmd_goto,       { TOP } },
  {"^G$",          bcmd_goto,       { BOTTOM } },
  {"^[0-9]+G$",    bcmd_goto,       {0} },
  {"^zI$",         bcmd_zoom,       { ZOOM_IN } },
  {"^zO$",         bcmd_zoom,       { ZOOM_OUT } },
  {"^z0$",         bcmd_zoom,       { ZOOM_ORIGINAL } },
  {"^[0-9]+Z$",    bcmd_zoom,       { ZOOM_SPECIFIC } },
  {"^[0-9]+%$",    bcmd_scroll,     {0} },
};

/* special commands */
SpecialCommand special_commands[] = {
  /* identifier,   function,      a,   argument */
  {'/',            scmd_search,   0,   { DOWN } },
  {'?',            scmd_search,   0,   { UP } },
};

/* settings */
Setting settings[] = {
  /* name,         variable,                        type,  render,  description */
  {"revertvideo",  &(Zathura.Global.reverse_video), 'b',   TRUE,    "Invert the image"},
  {"password",     &(Zathura.PDF.password),         's',   FALSE,   "The password of the document"},
};

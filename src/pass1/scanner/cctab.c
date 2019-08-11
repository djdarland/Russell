#
/*
 * Russell
 *
 * character class table for scanner
 */

#include "scan.h"

int cctab[128] = {
    BADCC,      /*  NUL     */
    BADCC,      /*  SOH     */
    SEPCC,      /*  STX     */     /* used as user program marker in prologue */
    BADCC,      /*  ETX     */
    BADCC,      /*  EOT     */
    BADCC,      /*  ENQ     */
    BADCC,      /*  ACK     */
    BADCC,      /*  BEL     */
    OPRCC,      /*  BS      */
    WHTCC,      /*  HT      */
    WHTCC,      /*  LF      */
    BADCC,      /*  VT      */
    WHTCC,      /*  FF      */
    WHTCC,      /*  CR      */
    BADCC,      /*  SO      */
    BADCC,      /*  SI      */
    BADCC,      /*  DLE     */
    BADCC,      /*  DC1     */
    BADCC,      /*  DC2     */
    BADCC,      /*  DC3     */
    BADCC,      /*  DC4     */
    BADCC,      /*  NAK     */
    BADCC,      /*  SYN     */
    BADCC,      /*  ETB     */
    BADCC,      /*  CAN     */
    BADCC,      /*  EM      */
    BADCC,      /*  SUB     */
    BADCC,      /*  ESC     */
    BADCC,      /*  FS      */
    BADCC,      /*  GS      */
    BADCC,      /*  RS      */
    BADCC,      /*  US      */

    WHTCC,      /*  SP      */
    OPRCC,      /*  !       */
    DQUCC,      /*  "       */
    SEPCC,      /*  #       */
    SEPCC,      /*  $       */
    OPRCC,      /*  %       */
    OPRCC,      /*  &       */
    SQUCC,      /*  '       */
    SEPCC,      /*  (       */
    SEPCC,      /*  )       */
    OPRCC,      /*  *       */
    OPRCC,      /*  +       */
    SEPCC,      /*  ,       */
    OPRCC,      /*  -       */
    OPRCC,      /*  .       */
    OPRCC,      /*  /       */
    DIGCC,      /*  0       */
    DIGCC,      /*  1       */
    DIGCC,      /*  2       */
    DIGCC,      /*  3       */
    DIGCC,      /*  4       */
    DIGCC,      /*  5       */
    DIGCC,      /*  6       */
    DIGCC,      /*  7       */
    DIGCC,      /*  8       */
    DIGCC,      /*  9       */
    OPRCC,      /*  :       */
    SEPCC,      /*  ;       */
    OPRCC,      /*  <       */
    OPRCC,      /*  =       */
    OPRCC,      /*  >       */
    OPRCC,      /*  ?       */

    OPRCC,      /*  @       */
    LETCC,      /*  A       */
    LETCC,      /*  B       */
    LETCC,      /*  C       */
    LETCC,      /*  D       */
    LETCC,      /*  E       */
    LETCC,      /*  F       */
    LETCC,      /*  G       */
    LETCC,      /*  H       */
    LETCC,      /*  I       */
    LETCC,      /*  J       */
    LETCC,      /*  K       */
    LETCC,      /*  L       */
    LETCC,      /*  M       */
    LETCC,      /*  N       */
    LETCC,      /*  O       */
    LETCC,      /*  P       */
    LETCC,      /*  Q       */
    LETCC,      /*  R       */
    LETCC,      /*  S       */
    LETCC,      /*  T       */
    LETCC,      /*  U       */
    LETCC,      /*  V       */
    LETCC,      /*  W       */
    LETCC,      /*  X       */
    LETCC,      /*  Y       */
    LETCC,      /*  Z       */
    SEPCC,      /*  [       */
    OPRCC,      /*  \       */
    SEPCC,      /*  ]       */
    OPRCC,      /*  ^       */
    LETCC,      /*  _       */

    OPRCC,      /*  `       */
    LETCC,      /*  a       */
    LETCC,      /*  b       */
    LETCC,      /*  c       */
    LETCC,      /*  d       */
    LETCC,      /*  e       */
    LETCC,      /*  f       */
    LETCC,      /*  g       */
    LETCC,      /*  h       */
    LETCC,      /*  i       */
    LETCC,      /*  j       */
    LETCC,      /*  k       */
    LETCC,      /*  l       */
    LETCC,      /*  m       */
    LETCC,      /*  n       */
    LETCC,      /*  o       */
    LETCC,      /*  p       */
    LETCC,      /*  q       */
    LETCC,      /*  r       */
    LETCC,      /*  s       */
    LETCC,      /*  t       */
    LETCC,      /*  u       */
    LETCC,      /*  v       */
    LETCC,      /*  w       */
    LETCC,      /*  x       */
    LETCC,      /*  y       */
    LETCC,      /*  z       */
    SEPCC,      /*  {       */
    OPRCC,      /*  |       */
    SEPCC,      /*  }       */
    OPRCC,      /*  ~       */
    BADCC       /*  DEL     */
};

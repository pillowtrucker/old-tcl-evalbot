#include <tcl.h>
#include "tclstubs.h"

const char * Tcl_InitStubs_wrap(Tcl_Interp * interp, char * wanted_version, int wantexact) {
  return Tcl_InitStubs(interp,wanted_version,wantexact);
  }

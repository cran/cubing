#include "header.h"
#include <stdlib.h>
#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[] = {
    {"kociemba1",  (DL_FUNC) &kociemba1,  18},
    {"twistflip1", (DL_FUNC) &twistflip1, 17},
    {"zemtwist1",  (DL_FUNC) &zemtwist1,  17},
    {NULL, NULL, 0}
};

void R_init_cubing(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

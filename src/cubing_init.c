#include "header.h"
#include <Rinternals.h> 
#include <R_ext/Rdynload.h>

static R_NativePrimitiveArgType kociemba1_t[] = {
    INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
	INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
	INTSXP, INTSXP, LGLSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType twistflip1_t[] = {
    INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
	INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
	INTSXP, LGLSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType zemtwist1_t[] = {
    INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
	INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
	INTSXP, LGLSXP, INTSXP, INTSXP, INTSXP
};

static const R_CMethodDef CEntries[] = {
    {"kociemba1",  (DL_FUNC) &kociemba1,  18, kociemba1_t},
    {"twistflip1", (DL_FUNC) &twistflip1, 17, twistflip1_t},
    {"zemtwist1",  (DL_FUNC) &zemtwist1,  17, zemtwist1_t},
    {NULL, NULL, 0, NULL}
};

void R_init_cubing(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
	R_forceSymbols(dll, TRUE);
}







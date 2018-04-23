
void kociemba1(int *mt_co, int *mt_eo, int *mt_e4b, 
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_eoXeEb, int *pt_coXeEb, 
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map,
              int *maxDepth, int *verbose,
              int *search, int *depthtotal, int *depthA);

int kociemba2(int *search, int depth1, int maxDepth,
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map);

void zemtwist1(int *mt_co, int *mt_eo, int *mt_e4b, 
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_eoXeEb, 
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map,
              int *maxDepth, int *verbose,
              int *search, int *depthtotal, int *depthA);

void twistflip1(int *mt_co, int *mt_eo, int *mt_e4b, 
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_eEb, 
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map,
              int *maxDepth, int *verbose,
              int *search, int *depthtotal, int *depthA);

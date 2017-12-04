#include <R.h>
#include <Rmath.h>

/* declarations */ 
  
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

/* kociemba phase one function */ 

void kociemba1(int *mt_co, int *mt_eo, int *mt_e4b, 
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_eoXeEb, int *pt_coXeEb, 
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map,
              int *maxDepth, int *verbose,
              int *search, int *depthtotal, int *depthA)
{
  
  int n_eEb = 495;
  int nc = 31;
  
  int axis = 0;
  int power = 1;
  int eEb = 5;
  int co = 6;
  int eo = 7;
  int minDist1 = 11;
  
  int mv, s;
  int n = 0;
  int busy = 0;
  int depth1 = 1;
  
  do {
    do {
      if ((depth1 - n > search[minDist1*nc + (n + 1)]) && !busy) {
        
        if (search[axis*nc + n] == 1 || search[axis*nc + n] == 4) {
          n = n + 1;
          search[axis*nc + n] = 2;
          search[power*nc + n] = 1;
        }
        else {
          n = n + 1;
          search[axis*nc + n] = 1;
          search[power*nc + n] = 1;
        }
        
      } else if (++search[power*nc + n] > 3) {
        do {
          if (++search[axis*nc + n] > 6) {
            
            if (n == 0) {
              if (depth1 >= *maxDepth)
                error("Phase 1 depth exceeds maximum");
              else {
                depth1 = depth1 + 1;
                search[axis*nc + n] = 1;
                search[power*nc + n] = 1;
                busy = 0;
                break;
              }
            } else {
              n = n - 1;
              busy = 1;
              break;
            }
            
          } else {
            search[power*nc + n] = 1;
            busy = 0;
          }
        } while (n != 0 && (search[axis*nc + (n - 1)] == search[axis*nc + n] || search[axis*nc + (n - 1)] == search[axis*nc + n] + 3));
      } else
        busy = 0;
    } while (busy);
    
    /* compute new coordinates for move and new minDist1 */
    /* if minDist1 = 0 then the subgroup is reached */
    
    mv = 3 * (search[axis*nc + n] - 1) + search[power*nc + n] - 1;
    search[eEb*nc + (n + 1)] = mt_e4b[(search[eEb*nc + n] - 1)*18 + mv];
    search[co*nc + (n + 1)] = mt_co[(search[co*nc + n] - 1)*18 + mv];
    search[eo*nc + (n + 1)] = mt_eo[(search[eo*nc + n] - 1)*18 + mv];
    
    search[minDist1*nc + (n + 1)] = imax2(
      pt_eoXeEb[n_eEb * (search[eo*nc + (n + 1)] - 1) + search[eEb*nc + (n + 1)] - 1], 
        pt_coXeEb[n_eEb * (search[co*nc + (n + 1)] - 1) + search[eEb*nc + (n + 1)] - 1]);
  
  
    if (search[minDist1*nc + (n + 1)] == 0) 
    {
      search[minDist1*nc + (n + 1)] = 100;
      
      if (n == depth1 - 1) {
        if(*verbose) Rprintf("Entering Phase Two: ");
        
        s = kociemba2(search, depth1, *maxDepth, mt_cp, mt_e4t, mt_eMSp, mt_eEp,
                      pt_cpXeEp, pt_eMSpXeEp, tt_eMSp, tt_eMSp_Map);
        
        if(*verbose) {
          if(s == -2) Rprintf("Immediate Return To Phase One\n");
          if(s == -1) Rprintf("Return To Phase One\n");
          if(s >= 0) Rprintf("%d Move Solution Found\n", s);
        }
        if(s >= 0) {
          if (s == depth1 || (search[axis*nc + (depth1 - 1)] != search[axis*nc + depth1])) {
            *depthtotal = s;
            *depthA = depth1;
            return;
          } else {
            if(*verbose) Rprintf("%d Move Solution Rejected Due To Phase Break\n", s);
          }
        }
      }
    }
  } while (1);

}

/* kociemba phase two function */ 

int kociemba2(int *search, int depth1, int maxDepth,
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map)
{
  
  int n_eEp = 24;
  int nc = 31;
  
  int axis = 0;
  int power = 1;
  int cp = 2;
  int eMSp = 3;
  int eEp = 4;
  int eMt = 8;
  int eEt = 9;
  int eSt = 10;
  int minDist2 = 12;
  
  int i;
  int mv, dd;
  int eMbval, eMbval2, eMpval, eSpval;
  int maxDepth2;
    
  for (i = 0; i < depth1; i++) {
    mv = 3 * (search[axis*nc + i] - 1) + search[power*nc + i] - 1;
    search[cp*nc + (i + 1)] = mt_cp[(search[cp*nc + i] - 1)*18 + mv];
    search[eMt*nc + (i + 1)] = mt_e4t[(search[eMt*nc + i] - 1)*18 + mv];
    search[eEt*nc + (i + 1)] = mt_e4t[(search[eEt*nc + i] - 1)*18 + mv];
    search[eSt*nc + (i + 1)] = mt_e4t[(search[eSt*nc + i] - 1)*18 + mv];
  }
  
  eMbval2 = (search[eMt*nc + depth1] - 1) / 24 + 1;
  eMbval = 0;
  while (tt_eMSp_Map[eMbval] != eMbval2) {
    eMbval = eMbval + 1;
  }
  if(eMbval >= 70) {
    Rprintf("%d %d %d", eMbval, eMbval2, search[eMt*nc + depth1]);
    Rprintf("\n\n");
    for (i = 0; i < 12; i++) {
      for (int j = 0; j < 31; j++) {
        Rprintf("%d ", search[i*nc + j]);
      }
      Rprintf("\n");
    }
    error("code error: eMbval value not found");
  }
  eMpval = (search[eMt*nc + depth1] - 1) % 24;
  eSpval = (search[eSt*nc + depth1] - 1) % 24; 
  
  search[eEp*nc + depth1] = (search[eEt*nc + depth1] - 1) % 24 + 1;
  search[eMSp*nc + depth1] = tt_eMSp[eMbval*24*24 + eMpval*24 + eSpval];
  
  maxDepth2 = maxDepth - depth1;
  
  if (maxDepth2 < (dd = imax2(
    pt_cpXeEp[n_eEp * (search[cp*nc + depth1] - 1) + search[eEp*nc + depth1] - 1],
      pt_eMSpXeEp[n_eEp * (search[eMSp*nc + depth1] - 1) + search[eEp*nc + depth1] - 1]))) {
    return -2;
  }
                     
  if ((search[minDist2*nc + depth1] = dd) == 0)  
    return depth1;
  
  /* initialize phase two */
  int n = depth1;
  int busy = 0;
  int depth2 = 1;
  
  search[axis*nc + depth1] = 1;
  search[power*nc + depth1] = 0;
  search[minDist2*nc + (n + 1)] = 1;
  
  do {
    do {
      if ((depth1 + depth2 - n > search[minDist2*nc + (n + 1)]) && !busy) {
        
        if (search[axis*nc + n] == 1 || search[axis*nc + n] == 4)
        {
          n = n + 1;
          search[axis*nc + n] = 2;
          search[power*nc + n] = 2;
        } else {
          n = n + 1;
          search[axis*nc + n] = 1;
          search[power*nc + n] = 1;
        }
      } else if ((search[axis*nc + n] == 1 || search[axis*nc + n] == 4) ? (++search[power*nc + n] > 3) : ((search[power*nc + n] = search[power*nc + n] + 2) > 3)) {
        do {
          if (++search[axis*nc + n] > 6) {
            if (n == depth1) {
              if (depth2 >= maxDepth2)
                return -1;
              else {
                depth2 = depth2 + 1;
                search[axis*nc + n] = 1;
                search[power*nc + n] = 1;
                busy = 0;
                break;
              }
            } else {
              n = n - 1;
              busy = 1;
              break;
            }
            
          } else {
            if (search[axis*nc + n] == 1 || search[axis*nc + n] == 4)
              search[power*nc + n] = 1;
            else
              search[power*nc + n] = 2;
            busy = 0;
          }
        } while (n != depth1 && (search[axis*nc + (n - 1)] == search[axis*nc + n] || search[axis*nc + (n - 1)] == search[axis*nc + n] + 3));
      } else
        busy = 0;
    } while (busy);
    
    /* compute new coordinates for move and new minDist2 */
    /* if minDist2 = 0 then the cube is solved */
    
    mv = 3 * (search[axis*nc + n] - 1) + search[power*nc + n] - 1;
    search[cp*nc + (n + 1)] = mt_cp[(search[cp*nc + n] - 1)*18 + mv];
    search[eMSp*nc + (n + 1)] = mt_eMSp[(search[eMSp*nc + n] - 1)*18 + mv];
    search[eEp*nc + (n + 1)] = mt_eEp[(search[eEp*nc + n] - 1)*18 + mv];
    
    search[minDist2*nc + (n + 1)] = imax2(
      pt_cpXeEp[n_eEp * (search[cp*nc + (n + 1)] - 1) + search[eEp*nc + (n + 1)] - 1],
        pt_eMSpXeEp[n_eEp * (search[eMSp*nc + (n + 1)] - 1) + search[eEp*nc + (n + 1)] - 1]);
                                              
  } while (search[minDist2*nc + (n + 1)] != 0);
  
  return depth1 + depth2;
}

/* zemtwist phase one function */

void zemtwist1(int *mt_co, int *mt_eo, int *mt_e4b, 
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_eoXeEb,
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map,
              int *maxDepth, int *verbose,
              int *search, int *depthtotal, int *depthA)
{
  
  int n_eEb = 495;
  int nc = 31;
  
  int axis = 0;
  int power = 1;
  int eEb = 5;
  int co = 6;
  int eo = 7;
  int minDist1 = 11;
  
  int mv, s, i;
  int n = 0;
  int busy = 0;
  int depth1 = 1;
  
  do {
    do {
      if ((depth1 - n > search[minDist1*nc + (n + 1)]) && !busy) {
        
        if (search[axis*nc + n] == 1 || search[axis*nc + n] == 4) {
          n = n + 1;
          search[axis*nc + n] = 2;
          search[power*nc + n] = 1;
        }
        else {
          n = n + 1;
          search[axis*nc + n] = 1;
          search[power*nc + n] = 1;
        }
        
      } else if (++search[power*nc + n] > 3) {
        do {
          if (++search[axis*nc + n] > 6) {
            
            if (n == 0) {
              if (depth1 >= *maxDepth)
                error("Phase 1 depth exceeds maximum");
              else {
                depth1 = depth1 + 1;
                search[axis*nc + n] = 1;
                search[power*nc + n] = 1;
                busy = 0;
                break;
              }
            } else {
              n = n - 1;
              busy = 1;
              break;
            }
            
          } else {
            search[power*nc + n] = 1;
            busy = 0;
          }
        } while (n != 0 && (search[axis*nc + (n - 1)] == search[axis*nc + n] || search[axis*nc + (n - 1)] == search[axis*nc + n] + 3));
      } else
        busy = 0;
    } while (busy);
    
    /* compute new coordinates for move and new minDist1 */
    /* if minDist1 = 0 then the subgroup is reached */
    
    mv = 3 * (search[axis*nc + n] - 1) + search[power*nc + n] - 1;
    search[eEb*nc + (n + 1)] = mt_e4b[(search[eEb*nc + n] - 1)*18 + mv];
    search[eo*nc + (n + 1)] = mt_eo[(search[eo*nc + n] - 1)*18 + mv];
    
    search[minDist1*nc + (n + 1)] = 
      pt_eoXeEb[n_eEb * (search[eo*nc + (n + 1)] - 1) + search[eEb*nc + (n + 1)] - 1];
    
    
    if (search[minDist1*nc + (n + 1)] == 0) 
    {
      search[minDist1*nc + (n + 1)] = 100;
      
      if (n == depth1 - 1) {
        if(*verbose) Rprintf("Entering Phase Two: ");
        
        s = kociemba2(search, depth1, *maxDepth, mt_cp, mt_e4t, mt_eMSp, mt_eEp,
                      pt_cpXeEp, pt_eMSpXeEp, tt_eMSp, tt_eMSp_Map);
        
        if(*verbose) {
          if(s == -2) Rprintf("Immediate Return To Phase One\n");
          if(s == -1) Rprintf("Return To Phase One\n");
          if(s >= 0) Rprintf("%d Move Solution Found\n", s);
        }
        if(s >= 0) {
          if (s == depth1 || (search[axis*nc + (depth1 - 1)] != search[axis*nc + depth1])) {
            for (i = 0; i < s; i++) {
              mv = 3 * (search[axis*nc + i] - 1) + search[power*nc + i] - 1;
              search[co*nc + (i + 1)] = mt_co[(search[co*nc + i] - 1)*18 + mv];
            }
            *depthtotal = s;
            *depthA = depth1;
            return;
          } else {
            if(*verbose) Rprintf("%d Move Solution Rejected Due To Phase Break\n", s);
          }
        }
      }
    }
  } while (1);
  
}

/* twistflip phase one function */

void twistflip1(int *mt_co, int *mt_eo, int *mt_e4b, 
              int *mt_cp, int *mt_e4t, int *mt_eMSp, int *mt_eEp,
              int *pt_eEb,
              int *pt_cpXeEp, int *pt_eMSpXeEp,
              int *tt_eMSp, int *tt_eMSp_Map,
              int *maxDepth, int *verbose,
              int *search, int *depthtotal, int *depthA)
{
  int nc = 31;
  
  int axis = 0;
  int power = 1;
  int eEb = 5;
  int co = 6;
  int eo = 7;
  int minDist1 = 11;
  
  int mv, s, i;
  int n = 0;
  int busy = 0;
  int depth1 = 1;
  
  do {
    do {
      if ((depth1 - n > search[minDist1*nc + (n + 1)]) && !busy) {
        
        if (search[axis*nc + n] == 1 || search[axis*nc + n] == 4) {
          n = n + 1;
          search[axis*nc + n] = 2;
          search[power*nc + n] = 1;
        }
        else {
          n = n + 1;
          search[axis*nc + n] = 1;
          search[power*nc + n] = 1;
        }
        
      } else if (++search[power*nc + n] > 3) {
        do {
          if (++search[axis*nc + n] > 6) {
            
            if (n == 0) {
              if (depth1 >= *maxDepth)
                error("Phase 1 depth exceeds maximum");
              else {
                depth1 = depth1 + 1;
                search[axis*nc + n] = 1;
                search[power*nc + n] = 1;
                busy = 0;
                break;
              }
            } else {
              n = n - 1;
              busy = 1;
              break;
            }
            
          } else {
            search[power*nc + n] = 1;
            busy = 0;
          }
        } while (n != 0 && (search[axis*nc + (n - 1)] == search[axis*nc + n] || search[axis*nc + (n - 1)] == search[axis*nc + n] + 3));
      } else
        busy = 0;
    } while (busy);
    
    /* compute new coordinates for move and new minDist1 */
    /* if minDist1 = 0 then the subgroup is reached */
    
    mv = 3 * (search[axis*nc + n] - 1) + search[power*nc + n] - 1;
    search[eEb*nc + (n + 1)] = mt_e4b[(search[eEb*nc + n] - 1)*18 + mv];
    
    search[minDist1*nc + (n + 1)] = 
      pt_eEb[search[eEb*nc + (n + 1)] - 1];
    
    
    if (search[minDist1*nc + (n + 1)] == 0) 
    {
      search[minDist1*nc + (n + 1)] = 100;
      
      if (n == depth1 - 1) {
        if(*verbose) Rprintf("Entering Phase Two: ");
        
        s = kociemba2(search, depth1, *maxDepth, mt_cp, mt_e4t, mt_eMSp, mt_eEp,
                      pt_cpXeEp, pt_eMSpXeEp, tt_eMSp, tt_eMSp_Map);
        
        if(*verbose) {
          if(s == -2) Rprintf("Immediate Return To Phase One\n");
          if(s == -1) Rprintf("Return To Phase One\n");
          if(s >= 0) Rprintf("%d Move Solution Found\n", s);
        }
        if(s >= 0) {
          if (s == depth1 || (search[axis*nc + (depth1 - 1)] != search[axis*nc + depth1])) {
            for (i = 0; i < s; i++) {
              mv = 3 * (search[axis*nc + i] - 1) + search[power*nc + i] - 1;
              search[co*nc + (i + 1)] = mt_co[(search[co*nc + i] - 1)*18 + mv];
              search[eo*nc + (i + 1)] = mt_eo[(search[eo*nc + i] - 1)*18 + mv];
            }
            *depthtotal = s;
            *depthA = depth1;
            return;
          } else {
            if(*verbose) Rprintf("%d Move Solution Rejected Due To Phase Break\n", s);
          }
        }
      }
    }
  } while (1);
  
}



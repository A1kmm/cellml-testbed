#include <stdlib.h>
#include <unistd.h>
#include <inttypes.h>
#include <ida/ida.h>
#include <sundials/sundials_nvector.h>
#include <nvector/nvector_serial.h>
#include <math.h>
#include <kinsol/kinsol.h>
#include <kinsol/kinsol_spgmr.h>

struct ArrayBundle {
  double* abConstants, * abStates, * abRates;
};

struct KSData {
  struct ArrayBundle* ksBundle;
  int (*ksf)(void*,double*,double*);
};

#define UNBUNDLE_ARRAYS \
  double * CONSTANTS; \
  double * STATES; \
  double * RATES; \
  CONSTANTS = ((struct ArrayBundle*)allState)->abConstants; \
  STATES = ((struct ArrayBundle*)allState)->abStates; \
  RATES = ((struct ArrayBundle*)allState)->abRates;

int solver_call(N_Vector params, N_Vector resids, void* udata)
{
  return ((struct KSData*)udata)->ksf(((struct KSData*)udata)->ksBundle, NV_DATA_S(params),
                                      NV_DATA_S(resids));
}

void read_fully(size_t count, void* buf)
{
  size_t r;
  while (count > 0)
  {
    r = read(0, buf, count);
    if (r <= 0)
      exit(1);
    count -= r;
    buf += r;
  }
}

void write_fully(size_t count, void* buf)
{
  size_t w;
  while (count > 0)
  {
    w = write(1, buf, count);
    if (w <= 0)
      exit(1);
    count -= w;
    buf += w;
  }
}

#include <stdarg.h>

void print_error(const char* aMsg, ...)
{
  va_list val;
  va_start(val, aMsg);
  vprint_error(aMsg, val);
  va_end(val);
}

void
vprint_error(const char* aMsg, va_list aVal)
{
  char buf[256];
  vsnprintf(buf, 255, aMsg, aVal);
  buf[255] = 0;
  write_error(buf);
}

void write_error(const char* aMsg)
{
  uint8_t byte = 0;
  write_fully(1, &byte);
  byte = strlen(aMsg);
  write_fully(1, &byte);
  write_fully(byte, aMsg);
}

struct Override {
  uint32_t overVar;
  double overVal;
};

#define NWORK (1 + NCONSTS + NVARS*2)

void err_report(int code, const char* mod, const char* func, const char* msg, void* udata)
{
  print_error("IDA failure in module %s, function %s: %s", mod, func, msg);
}

double* DoSolve(struct ArrayBundle* b, size_t n, int (*f)(void*,double*,double*))
{
  N_Vector params, ones;
  void* solver;
  int flag;
  double* paramArray;
  struct KSData s;

  paramArray = malloc(sizeof(double)*n);
  params = N_VMake_Serial(n, paramArray);
  memset(NV_DATA_S(params), 0, sizeof(double) * n);

  s.ksBundle = b;
  s.ksf = f;

  solver = KINCreate();
  KINInit(solver, solver_call, params);
  KINSetErrHandlerFn(solver, err_report, NULL);
  KINSetUserData(solver, &s);
  KINSpgmr(solver, 0);
  
  ones = N_VNew_Serial(n);
  N_VConst(1.0, ones);
  flag = KINSol(solver, params, KIN_LINESEARCH, ones, ones);
  N_VDestroy(ones);

  KINFree(solver);
  N_VDestroy(params);

  if (flag != KIN_SUCCESS)
  {
    free(paramArray);
    return NULL;
  }

  return paramArray;
}

int compute_residuals(double t, N_Vector yy, N_Vector yp, N_Vector r, void* user_data)
{
  int ret, i;
  double* work = user_data, * states = NV_DATA_S(yy), * rates = NV_DATA_S(yp),
        * residuals = NV_DATA_S(r);

#ifdef DEBUG_RESIDUALS
  for (i = 0; i < NCONSTS; i++)
    fprintf(stderr, "CONSTANTS[i] = %g\n", i, work[1 + i]);
  for (i = 0; i < NVARS; i++)
    fprintf(stderr, "VARS[%d] = %g, RATES[%d] = %g\n", i, states[i], i, rates[i]);
#endif
  ret = solveResiduals(t, work + 1, states, rates, residuals);
#ifdef DEBUG_RESIDUALS
  for (i = 0; i < NVARS; i++)
    fprintf(stderr, "residuals[%d] = %g\n", i, residuals[i]);
#endif
  return ret;
}

int compute_jacobian(double t, N_Vector yy, N_Vector yp, N_Vector r, N_Vector vv, N_Vector Jvv, double alpha, void* user_data, N_Vector tmp1, N_Vector tmp2)
{
  double * work = user_data, * states = NV_DATA_S(yy), * rates = NV_DATA_S(yp),
    * residuals = NV_DATA_S(r), * v = NV_DATA_S(vv), * Jv = NV_DATA_S(Jvv);

  solveJacobianxVec(t, alpha, work + 1, states, rates, v, Jv);

  return 0;
}

int consistent_solve_step(N_Vector params, N_Vector resids, void* udata)
{
  int ret;
  double* work = udata;
  paramsToState(NV_DATA_S(params), work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS);
  return solveResiduals(work[0], work + 1, work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS, NV_DATA_S(resids));
}

int ensure_consistent_at(double t, double* work)
{
  N_Vector params, ones;
  void* solver;
  int flag;

  if (NRESIDINITVARS == 0)
    return 0;

  params = N_VNew_Serial(NRESIDINITVARS);
  memset(NV_DATA_S(params), 0, sizeof(double) * NRESIDINITVARS);

  solver = KINCreate();
  KINInit(solver, consistent_solve_step, params);
  KINSetUserData(solver, work);
  KINSetErrHandlerFn(solver, err_report, NULL);
  KINSpgmr(solver, 0);
  
  work[0] = t;

  ones = N_VNew_Serial(NRESIDINITVARS);
  N_VConst(1.0, ones);
  flag = KINSol(solver, params, KIN_LINESEARCH, ones, ones);
  paramsToState(NV_DATA_S(params), work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS);

  {
    double norm;
    int i;
    flag = KINGetFuncNorm(solver, &norm);
#ifdef DEBUG_RESIDUALS
    fprintf(stderr, "Final residual: %g, flag = %d\n", norm, flag);
    for (i = 0; i < NRESIDINITVARS; i++)
      fprintf(stderr, " param[%d] = %g\n", i, NV_DATA_S(params)[i]);
#endif
  }

  KINFree(&solver);
  N_VDestroy(ones);
  N_VDestroy(params);

  return flag;
}

int main(int argc, char** argv)
{
  double* work = malloc(sizeof(double) * NWORK);
  memset(work, 0, sizeof(double) * NWORK);

  while (1)
  {
    uint64_t nOverrides, i;
    struct Override* overrides;
    double tStart, tEnd, tActual, relTol, absTol;
    int solret;
    uint8_t byte;
    void* idaProblem;
    N_Vector yvec, ypvec;
    int wasSuccess = 0;

    read_fully(1, &byte);
    if (byte == 1)
      return 0;
    read_fully(8, &nOverrides);
    overrides = malloc(sizeof(struct Override) * nOverrides);
    read_fully(sizeof(struct Override) * nOverrides, overrides);
    read_fully(sizeof(double), &tStart);
    read_fully(sizeof(double), &tEnd);
    read_fully(sizeof(double), &relTol);
    read_fully(sizeof(double), &absTol);

    solret = solveForIVs(tStart, work + 1, work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS);
    for (i = 0; i < nOverrides; i++)
      work[overrides[i].overVar] = overrides[i].overVal;

    free(overrides);
    /* Error will have already been produced if this fails. */
    if (solret)
      continue;
    
    idaProblem = IDACreate();
    IDASetErrHandlerFn(idaProblem, err_report, NULL);

    ensure_consistent_at(tStart, work);

    yvec = N_VMake_Serial(NVARS, work + 1 + NCONSTS);
    ypvec = N_VMake_Serial(NVARS, work + 1 + NCONSTS + NVARS);

    IDASetUserData(idaProblem, work);
    IDAInit(idaProblem, compute_residuals, tStart, yvec, ypvec);
    IDASStolerances(idaProblem, relTol, absTol);
    IDASpgmr(idaProblem, 0);
    IDASetStopTime(idaProblem, tEnd);
    IDASpilsSetJacTimesVecFn(idaProblem, compute_jacobian);

    // TODO: IDARootInit for piecewise changes...

    solret = IDA_SUCCESS;
    tActual = tStart;
    while (1)
    {
      if (solret != IDA_SUCCESS && solret != IDA_TSTOP_RETURN)
        break;

      /* Send NumericalData */
      byte = 2;
      write_fully(1, &byte);
      work[0] = tActual;
      write_fully(NWORK * sizeof(double), work);

      if (tEnd - tActual <= fabs(1E-6 * (tEnd - tStart)))
      {
        wasSuccess = 1;
        break;
      }

      solret = IDASolve(idaProblem, tEnd, &tActual, yvec, ypvec, IDA_ONE_STEP);
    }

    if (wasSuccess)
    {
      /* Send NumericalSuccess */
      byte = 1;
      write_fully(1, &byte);
    }

    N_VDestroy(yvec);
    N_VDestroy(ypvec);
    IDAFree(&idaProblem);
  }

  free(work);
}

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
    fprintf(stderr, "Trying to read %d bytes\n", count);
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
    w = write(0, buf, count);
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
  vsnprintf(aMsg, 255, buf, aVal);
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
  double* work = user_data, * states = NV_DATA_S(yy), * rates = NV_DATA_S(yp),
        * residuals = NV_DATA_S(r);
  return solveResiduals(t, work + 1, states, rates, residuals);
}

int consistent_solve_step(N_Vector params, N_Vector resids, void* udata)
{
  int ret;
  double* work = udata;
  paramsToState(NV_DATA_S(params), work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS);
  return solveResiduals(work[0], work + 1, work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS);
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
  
  ones = N_VNew_Serial(NRESIDINITVARS);
  N_VConst(1.0, ones);
  flag = KINSol(solver, params, KIN_LINESEARCH, ones, ones);
  N_VDestroy(ones);

  KINFree(solver);
  N_VDestroy(params);

  return flag;
}

int main(int argc, char** argv)
{
  double* work = malloc(sizeof(double) * NWORK);

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
    if (!solret)
    {
      print_error("Failed to solve for initial values");
      continue;
    }
    
    idaProblem = IDACreate();
    IDASetErrHandlerFn(idaProblem, err_report, NULL);

    yvec = N_VMake_Serial(NVARS, work + 1 + NCONSTS);
    ypvec = N_VMake_Serial(NVARS, work + 1 + NCONSTS + NVARS);

    IDAInit(idaProblem, compute_residuals, tStart, yvec, ypvec);
    IDASStolerances(idaProblem, relTol, absTol);
    IDASpgmr(idaProblem, 0);
    
    ensure_consistent_at(tStart, work);

    // TODO: IDARootInit for piecewise changes...

    while (IDASolve(idaProblem, tEnd, &tActual, yvec, ypvec, IDA_ONE_STEP) == IDA_SUCCESS)
    {
      /* Send NumericalData */
      byte = 2;
      write_fully(1, &byte);
      work[0] = tActual;
      write_fully(NWORK * sizeof(double), work);

      if (tEnd - tActual >= fabs(1E-6 * (tEnd - tStart)))
      {
        wasSuccess = 1;
        break;
      }
    }

    if (wasSuccess)
    {
      /* Send NumericalSuccess */
      byte = 1;
      write_fully(1, &byte);
    }

    N_VDestroy(yvec);
    N_VDestroy(ypvec);
    IDAFree(idaProblem);
  }

  free(work);
}

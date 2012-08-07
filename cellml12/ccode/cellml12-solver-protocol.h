#include <stdlib.h>
#include <unistd.h>
#include <inttypes.h>
#include <ida/ida.h>
#include <sundials/sundials_nvector.h>
#include <nvector/nvector_serial.h>
#include <math.h>
#include <cvode/cvode.h>
#include <kinsol/kinsol.h>
#include <kinsol/kinsol_spgmr.h>

struct ArrayBundle {
  double* abConstants, * abStates, * abRates, * abBvars, * abOldStates, * abOldRates;
};

struct KSData {
  struct ArrayBundle* ksBundle;
  int (*ksf)(void*,double*,double*);
};

#define UNBUNDLE_ARRAYS \
  double * CONSTANTS; \
  double * STATES; \
  double * RATES; \
  double * BVARS; \
  double * OLDSTATES; \
  double * OLDRATES; \
  CONSTANTS = ((struct ArrayBundle*)allState)->abConstants; \
  STATES = ((struct ArrayBundle*)allState)->abStates; \
  RATES = ((struct ArrayBundle*)allState)->abRates; \
  BVARS = ((struct ArrayBundle*)allState)->abBvars; \
  OLDSTATES = ((struct ArrayBundle*)allState)->abOldStates; \
  OLDRATES = ((struct ArrayBundle*)allState)->abOldRates;

struct PDFData {
  struct ArrayBundle* pdfBundle;
  double (*pdf)(void* aBundle);
  double cdfTarget;
  int pdfBvar;
  double cdfUplimit;
};

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

#define NWORK (1 + NCONSTS + NVARS*4)
#define NSEND (1 + NCONSTS + NVARS*2)

void err_report(int code, const char* mod, const char* func, const char* msg, void* udata)
{
  print_error("IDA failure in module %s, function %s: %s", mod, func, msg);
}

void err_ignore(int code, const char* mod, const char* func, const char* msg, void* udata)
{
}

double* DoSolve(struct ArrayBundle* b, size_t n, int (*f)(void*,double*,double*))
{
  N_Vector params, ones;
  void* solver;
  int flag;
  double* paramArray;
  struct KSData s;
  int i;

  paramArray = malloc(sizeof(double)*n);
  for (i = 0; i < n; i++)
    paramArray[i] = 1.0;
  params = N_VMake_Serial(n, paramArray);

  s.ksBundle = b;
  s.ksf = f;

  solver = KINCreate();
  KINInit(solver, solver_call, params);
  KINSetErrHandlerFn(solver, err_report, NULL);
  KINSetUserData(solver, &s);
  KINSpgmr(solver, 0);
  
  ones = N_VNew_Serial(n);
  N_VConst(1.0, ones);
  KINSetMaxNewtonStep(solver, 1E20);
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

int
pdfIntegrand(double t, N_Vector varsV, N_Vector ratesV, void* data)
{
  struct PDFData* aPDF = (struct PDFData*)data;
  double tp1 = 1.0 + t;

  aPDF->pdfBundle->abBvars[aPDF->pdfBvar] = aPDF->cdfUplimit + t / tp1;

  NV_DATA_S(ratesV)[0] = aPDF->pdf(aPDF->pdfBundle) * (1.0 / (tp1 * tp1));

  return 0;
}

int
computeCDF(N_Vector params, N_Vector resids, void* udata)
{
  void* cv;
  struct PDFData* aPDF = (struct PDFData*)udata;
  int ret;
  double tret;

  aPDF->cdfUplimit = NV_DATA_S(params)[0];

  cv = CVodeCreate(CV_BDF, CV_NEWTON);
  NV_DATA_S(resids)[0] = 0;
  CVodeSetErrHandlerFn(cv, err_ignore, NULL);
  CVodeInit(cv, pdfIntegrand, -1.0 + 1E-6, resids);
  CVodeSetMaxStep(cv, 1E-3);
  CVodeSetMaxNumSteps(cv, 2000);
  CVodeSStolerances(cv, 1E-6, 1E-6);
  CVodeSetUserData(cv, udata);
  CVSpgmr(cv, PREC_NONE, 0);

  ret = CVode(cv, 0, resids, &tret, CV_NORMAL);
  CVodeFree(&cv);

  NV_DATA_S(resids)[0] = aPDF->cdfTarget - NV_DATA_S(resids)[0];

  return ret;
}

double SampleFromPDF(int aIdx,
                     double (*aFunc)(void*), struct ArrayBundle* aBundle)
{
  struct PDFData dPDF;
  double x, y;
  N_Vector params, ones;
  int attempt, flag;
  void * solver;
  
  dPDF.pdfBundle = aBundle;
  dPDF.pdf = aFunc;
  dPDF.pdfBvar = aIdx;

  x = (rand() + 0.0) / RAND_MAX;

  dPDF.cdfTarget = x;

  params = N_VMake_Serial(1, &y);
  ones = N_VNew_Serial(1);
  N_VConst(1.0, ones);
  
  for (attempt = 0; attempt < 26; attempt++)
  {
    y = (attempt % 2 ? -1.0 : 1.0) * pow(10.0, attempt / 2.0 - 3.0);


    solver = KINCreate();
    KINInit(solver, computeCDF, params);
    KINSetErrHandlerFn(solver, err_ignore, NULL);
    KINSetUserData(solver, &dPDF);
    KINSpgmr(solver, 0);


    KINSetMaxNewtonStep(solver, 1E20);
    flag = KINSol(solver, params, KIN_LINESEARCH, ones, ones);

    KINFree(&solver);

    if (flag == KIN_SUCCESS && isfinite(y))
      break;
  }

  N_VDestroy(params);
  N_VDestroy(ones);
  return y;
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
  ret = solveResiduals(t, work + 1, states, rates,
                       work + 1 + NCONSTS + NVARS * 4,
                       work + 1 + NCONSTS + NVARS * 2,
                       work + 1 + NCONSTS + NVARS * 3,
                       residuals);
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

  solveJacobianxVec(t, alpha, work + 1, states, rates,
                    work + 1 + NCONSTS + NVARS * 4,
                    work + 1 + NCONSTS + NVARS * 2,
                    work + 1 + NCONSTS + NVARS * 3, v, Jv);

  return 0;
}

#ifdef ONLY_RESID_VARS
int consistent_solve_step(N_Vector params, N_Vector resids, void* udata)
{
  int ret;
  double* work = udata;
  paramsToState(NV_DATA_S(params), work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS);
  return solveResiduals(work[0], work + 1,
                        work + 1 + NCONSTS,
                        work + 1 + NCONSTS + NVARS,
                        work + 1 + NCONSTS + NVARS * 4,
                        work + 1 + NCONSTS + NVARS * 2,
                        work + 1 + NCONSTS + NVARS * 3,
                        NV_DATA_S(resids));
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
  KINSetMaxNewtonStep(solver, 1E20);
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
#endif

struct SolveRatesState {
  int isRateSensitive[NVARS];
  double* work;
};

int consistent_solve_step(N_Vector params, N_Vector resids, void* udata)
{
  int ret, i;
  struct SolveRatesState* srs = udata;

  for (i = 0; i < NVARS; i++)
    if (srs->isRateSensitive[i])
        srs->work[1 + NCONSTS + NVARS + i] = NV_DATA_S(params)[i];
    else
      srs->work[1 + NCONSTS + i] = NV_DATA_S(params)[i];

  return solveResiduals(srs->work[0], srs->work + 1, srs->work + 1 + NCONSTS,
                        srs->work + 1 + NCONSTS + NVARS,
                        srs->work + 1 + NCONSTS + NVARS * 4,
                        srs->work + 1 + NCONSTS + NVARS * 2,
                        srs->work + 1 + NCONSTS + NVARS * 3,
                        NV_DATA_S(resids));
}

int ensure_consistent_at(double t, double* work)
{
  struct SolveRatesState srs;
  double resids1[NVARS], resids2[NVARS];
  N_Vector params, ones;
  void* solver;
  int flag, i, j;

  if (NVARS == 0)
    return 0;

  solveResiduals(work[0], work + 1, work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS,
                 work + 1 + NCONSTS + NVARS * 4,
                 work + 1 + NCONSTS + NVARS * 2,
                 work + 1 + NCONSTS + NVARS * 3,
                 resids1);
  for (i = 1 + NCONSTS + NVARS; i < 1 + NCONSTS + NVARS * 2; i++)
  {
    double backwork = work[i];
    if (fabs(work[i]) < 1E-6)
      work[i] = 1.0;
    else
      work[i] *= 1.01;
    solveResiduals(work[0], work + 1, work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS,
                   work + 1 + NCONSTS + NVARS * 4,
                   work + 1 + NCONSTS + NVARS * 2,
                   work + 1 + NCONSTS + NVARS * 3,
                   resids2);
    srs.isRateSensitive[i - (NCONSTS + NVARS + 1)] = 0;
    for (j = 0; j < NVARS; j++)
      if (resids1[j] != resids2[j])
      {
        srs.isRateSensitive[i - (NCONSTS + NVARS + 1)] = 1;
        break;
      }
    work[i] = backwork;
  }

  params = N_VNew_Serial(NVARS);
  memset(NV_DATA_S(params), 0, sizeof(double) * NVARS);

  srs.work = work;
  solver = KINCreate();
  KINInit(solver, consistent_solve_step, params);
  KINSetUserData(solver, &srs);
  KINSetErrHandlerFn(solver, err_report, NULL);
  KINSpgmr(solver, 0);
  
  work[0] = t;

  ones = N_VNew_Serial(NRESIDINITVARS);
  N_VConst(1.0, ones);
  KINSetMaxNewtonStep(solver, 1E20);
  flag = KINSol(solver, params, KIN_LINESEARCH, ones, ones);

  for (i = 0; i < NVARS; i++)
    if (srs.isRateSensitive[i])
      work[1 + NCONSTS + NVARS + i] = NV_DATA_S(params)[i];
    else
      work[1 + NCONSTS + i] = NV_DATA_S(params)[i];

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

int
ida_root_func(double t, N_Vector y, N_Vector yp, double* gout, void* user_data)
{
  double * work = user_data, * states = NV_DATA_S(y), * rates = NV_DATA_S(yp);
  
  getEventRoots(t, work + 1, states, rates, work + 1 + NCONSTS + NVARS * 4, gout);
  return 0;
}

int main(int argc, char** argv)
{
  double* work = malloc(sizeof(double) * NWORK);
  memset(work, 0, sizeof(double) * NWORK);

  srand(time(0));

  while (1)
  {
    uint64_t nOverrides, i;
    struct Override* overrides;
    double tStart, tEnd, tActual, relTol, absTol;
    int solret;
    uint8_t byte;
    void* idaProblem;
    N_Vector yvec, ypvec;
    int wasSuccess = 0, shouldRestart;

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

    solret = solveForIVs(tStart, work + 1,
                         work + 1 + NCONSTS, work + 1 + NCONSTS + NVARS,
                         work + 1 + NCONSTS + NVARS * 4,
                         work + 1 + NCONSTS + NVARS * 2,
                         work + 1 + NCONSTS + NVARS * 3);
    for (i = 0; i < nOverrides; i++)
      work[overrides[i].overVar] = overrides[i].overVal;

    free(overrides);
    /* Error will have already been produced if this fails. */
    if (solret)
      continue;

    yvec = N_VMake_Serial(NVARS, work + 1 + NCONSTS);
    ypvec = N_VMake_Serial(NVARS, work + 1 + NCONSTS + NVARS);
    
    tActual = tStart;

    do
    {
      shouldRestart = 0;
      idaProblem = IDACreate();

      IDASetErrHandlerFn(idaProblem, err_report, NULL);

      ensure_consistent_at(tActual, work);

      IDASetUserData(idaProblem, work);
      IDAInit(idaProblem, compute_residuals, tActual, yvec, ypvec);
      IDASStolerances(idaProblem, relTol, absTol);
      IDASpgmr(idaProblem, 0);
      IDASetStopTime(idaProblem, tEnd);
      IDASpilsSetJacTimesVecFn(idaProblem, compute_jacobian);
      
      if (NROOTS > 0)
        IDARootInit(idaProblem, NROOTS, ida_root_func);

      solret = IDA_SUCCESS;
      while (1)
      {
        if (solret != IDA_SUCCESS && solret != IDA_TSTOP_RETURN && solret != IDA_ROOT_RETURN)
          break;

        if (solret == IDA_ROOT_RETURN)
        {
          shouldRestart = 1;
          break;
        }

        /* Send NumericalData */
        byte = 2;
        write_fully(1, &byte);
        work[0] = tActual;
        write_fully(NSEND * sizeof(double), work);
        
        if (tEnd - tActual <= fabs(1E-6 * (tEnd - tStart)))
        {
          wasSuccess = 1;
          break;
        }

        solret = IDASolve(idaProblem, tEnd, &tActual, yvec, ypvec, IDA_ONE_STEP);
        /* Update the OLDSTATES and OLDRATES data... */
        memcpy(work + 1 + NCONSTS + NVARS * 2, work + 1 + NCONSTS, NVARS * 2 * sizeof(double));
      }

      if (wasSuccess)
      {
        /* Send NumericalSuccess */
        byte = 1;
        write_fully(1, &byte);
      }

      IDAFree(&idaProblem);
    } while (shouldRestart);
    N_VDestroy(yvec);
    N_VDestroy(ypvec);
  }

  free(work);
}

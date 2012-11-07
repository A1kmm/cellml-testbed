#include <cstdlib>
#include "cellml-testbed.h"
#include "HsFFI.h"
#include "CellMLMarshal_stub.h"

CellMLAPI::CellMLAPI()
{
  int argc = 1;
  char name[] = "CellMLAPI";
  char *argv[] = {name, NULL};
  char ** argvp = argv;
  hs_init(&argc, &argvp);
}

CellMLAPI::~CellMLAPI()
{
  hs_exit();
}

static HsStablePtr pathToVariableFromModel(PathToVariableFromModel* inp)
{
  if (inp->whatType == PathToVariableFromModel::GoToImport)
    return camarshal_makeModelPathImport(const_cast<char*>(inp->name.c_str()),
                                         pathToVariableFromModel(inp->toImport.get()));
  else
    return camarshal_makeModelPathComponent(const_cast<char*>(inp->name.c_str()),
                                            const_cast<char*>(inp->toComponent->
                                                              variableName.c_str()));
}


std::tr1::shared_ptr<PathToVariableFromModel>
mpathPtrToPath(HsStablePtr sp)
{
  std::tr1::shared_ptr<PathToVariableFromModel> result;

  if (camarshal_isModelPathImport(sp))
  {
    result.reset(new PathToVariableFromModel);
    result->whatType = PathToVariableFromModel::GoToImport;
    char* tmp = reinterpret_cast<char*>(camarshal_modelPathImportGetName(sp));
    result->name = tmp;
    free(tmp);
    result->toImport = mpathPtrToPath(camarshal_modelPathImportPath(sp));
  }
  else
  {
    result.reset(new PathToVariableFromModel);
    result->whatType = PathToVariableFromModel::GoToComponent;
    char* tmp = reinterpret_cast<char*>(camarshal_modelPathComponentGetName(sp));
    result->name = tmp;
    free(tmp);
    result->toComponent.reset(new PathToVariableFromComponent);
    tmp = reinterpret_cast<char*>(camarshal_modelPathComponentGetVariable(sp));
    result->toComponent->variableName = tmp;
    free(tmp);
  }

  hs_free_stable_ptr(sp);

  return result;
}

PossiblyError<std::tr1::shared_ptr<SolveCode> >
CellMLAPI::generateSolveCode(const std::string& aModelURL,
                             PathToVariableFromModel* aBvar)
{
  HsStablePtr sp =
    camarshal_generateSolveCode(const_cast<char*>(aModelURL.c_str()),
                                pathToVariableFromModel(aBvar));

  PossiblyError<std::tr1::shared_ptr<SolveCode> > pe;

  if (camarshal_isErrorFail(sp))
  {
    char* ptr = reinterpret_cast<char*>(camarshal_eitherUnsafeGetError(sp));
    pe.whatType = PossiblyError<std::tr1::shared_ptr<SolveCode> >::WasError;
    pe.errMsg = ptr;
    free(ptr);
    return pe;
  }
  pe.whatType = PossiblyError<std::tr1::shared_ptr<SolveCode> >::NotError;
  sp = camarshal_eitherUnsafeGetStablePtr(sp);

  char* ccode = reinterpret_cast<char*>(camarshal_getCCode(sp));
  pe.value.reset(new SolveCode);
  pe.value->code = ccode;
  free(ccode);

  HsStablePtr vpaths;
  for (vpaths = camarshal_startIteratingVariablePaths(sp);
       !camarshal_isIteratorFinished(vpaths);
       vpaths = camarshal_advanceIterator(vpaths))
  {
    VariableID varid = camarshal_getVarPathVarID(vpaths);
    std::list<std::tr1::shared_ptr<PathToVariableFromModel> > pathList;

    HsStablePtr actualMpaths;
    for (actualMpaths = camarshal_getVarPathModelPaths(vpaths);
         !camarshal_isIteratorFinished(actualMpaths);
         actualMpaths = camarshal_advanceIterator(actualMpaths))
      pathList.push_back(mpathPtrToPath(camarshal_getNextModelPath(actualMpaths)));
    hs_free_stable_ptr(actualMpaths);

    pe.value->variablePaths.insert(std::pair<VariableID, std::list<std::tr1::shared_ptr<PathToVariableFromModel> > >
                                   (varid, pathList));
  }
  hs_free_stable_ptr(vpaths);

  hs_free_stable_ptr(sp);

  return pe;
}

PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> >
CellMLAPI::solveModelWithParameters
(
 const std::string& aModelURL,
 PathToVariableFromModel* aBVar,
 // a list of initial condition overrides, as
 // pairs of parameters and new value, where a
 // parameter is specified as a pair of variable
 // path and derivative degree.
 const std::list<std::pair<std::pair<std::tr1::shared_ptr<PathToVariableFromModel>, DegreeOfDifferentiation>,
                           double> >&
   aParameterOverrides,
 double aLowBvar,
 double aHighBvar,
 double aRelTol,
 double aAbsTol
)
{
  HsStablePtr paramOverrides = camarshal_makeEmptyList();
  for (std::list<std::pair<std::pair<std::tr1::shared_ptr<PathToVariableFromModel>, DegreeOfDifferentiation>,
                           double> >::const_iterator i = aParameterOverrides.begin();
       i != aParameterOverrides.end();
       i++)
    paramOverrides = camarshal_addOverride(pathToVariableFromModel(i->first.first.get()), i->first.second, i->second, paramOverrides);

  HsStablePtr sp =
    camarshal_solveModelWithParameters
      (
       const_cast<char*>(aModelURL.c_str()),
       pathToVariableFromModel(aBVar),
       paramOverrides,
       aLowBvar,
       aHighBvar,
       aRelTol,
       aAbsTol
      );

  if (camarshal_isErrorFail(sp))
  {
    PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> > pe;
    char* ptr = reinterpret_cast<char*>(camarshal_eitherUnsafeGetError(sp));
    pe.whatType = PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> >::WasError;
    pe.errMsg = ptr;
    free(ptr);
    return pe;
  }
  sp = camarshal_eitherUnsafeGetStablePtr(sp);

  std::tr1::shared_ptr<DAEIntegrationResult> dir(new DAEIntegrationResult);

  HsStablePtr vpaths;
  for (vpaths = camarshal_startIteratingVariablePaths(sp);
       !camarshal_isIteratorFinished(vpaths);
       vpaths = camarshal_advanceIterator(vpaths))
  {
    VariableID varid = camarshal_getVarPathVarID(vpaths);
    std::list<std::tr1::shared_ptr<PathToVariableFromModel> > pathList;

    HsStablePtr actualMpaths;
    for (actualMpaths = camarshal_getVarPathModelPaths(vpaths);
         !camarshal_isIteratorFinished(actualMpaths);
         actualMpaths = camarshal_advanceIterator(actualMpaths))
      pathList.push_back(mpathPtrToPath(camarshal_getNextModelPath(actualMpaths)));
    hs_free_stable_ptr(actualMpaths);

    dir->variablePaths.insert(std::pair<VariableID, std::list<std::tr1::shared_ptr<PathToVariableFromModel> > >
                                   (varid, pathList));
  }
  hs_free_stable_ptr(vpaths);

  HsStablePtr residx;
  for (residx = camarshal_startIteratingVariableResultIndex(sp);
       !camarshal_isIteratorFinished(residx);
       residx = camarshal_advanceIterator(residx))
  {
    VariableID varid = camarshal_getNextResultIndexVariableID(residx);
    DegreeOfDifferentiation deg = camarshal_getNextResultIndexDeriv(residx);
    int idx = camarshal_getNextResultIndex(residx);
    dir->variableIndexInResult.insert
      (std::pair<std::pair<VariableID, DegreeOfDifferentiation>, int>
       (std::pair<VariableID, DegreeOfDifferentiation>(varid, deg),
        idx));
  }
  hs_free_stable_ptr(residx);

  HsStablePtr result;
  for (result = camarshal_beginResults(sp);
       !camarshal_isIteratorFinished(result);
       result = camarshal_advanceIterator(result))
  {
    int l = camarshal_getResultLength(result);
    double* rawdat = reinterpret_cast<double*>(camarshal_getResultRow(result));
    std::tr1::shared_ptr<RawResult> rawres(new RawResult);
    rawres->bvarValue = camarshal_getBvarValue(result);
    rawres->rawData.insert(rawres->rawData.begin(), rawdat, rawdat + l - 1);
    dir->rawResults.push_back(rawres);
  }
  hs_free_stable_ptr(result);

  hs_free_stable_ptr(sp);

  PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> > pe;
  pe.whatType = PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> >::NotError;
  pe.value = dir;
  return pe;
}

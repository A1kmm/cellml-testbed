#include <cstdlib>
#include "cellml-testbed.h"
#include "HsFFI.h"
#include "CellMLMarshal_stub.h"

CellMLAPI::CellMLAPI()
{
  int argc = 1;
  char name[] = "CellMLAPI";
  char *argv[2] = {name, NULL};
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
}

/*
PossiblyError<std::tr1::shared_ptr<SolveCode> >
CellMLAPI::solveModelWithParameters
(
 const std::string& aModelURL,
 PathToVariableFromModel* aBVar,
 // a list of initial condition overrides, as
 // pairs of parameters and new value, where a
 // parameter is specified as a pair of variable
 // path and derivative degree.
 const std::list<std::pair<std::pair<PathToVariableFromModel*, DegreeOfDifferentiation>,
                           double> >&
   aParameterOverrides,
 double aLowBvar,
 double aHighBvar,
 double aRelTol,
 double aAbsTol
)
{
  camarshal_solveModelWithParameters
    (
     aModelURL.c_str(),
     pathToVariableFromModel(aBvar),
     paramOverrides,
     
    );
  HsStablePtr sp =
    camarshal_generateSolveCode(const_cast<char*>(aModelURL.c_str()),
                                pathToVariableFromModel(aBvar));
  
}
*/

#include <memory>
#include <list>
#include <vector>
#include <string>
#include <map>
#include <tr1/memory>

struct PathToVariableFromComponent {
  std::string variableName;
};
struct PathToVariableFromModel {
  enum { GoToImport, GoToComponent } whatType;
  std::string name; // The name of the import or component to go to.
  // Must be non-null if whatType == GoToComponent
  std::tr1::shared_ptr<PathToVariableFromComponent> toComponent;
  // Must be non-null if whatType == GoToImport
  std::tr1::shared_ptr<PathToVariableFromModel> toImport;
};

template<typename T> class PossiblyError {
public:
  enum { WasError, NotError } whatType;
  std::string errMsg; // valid if whatType == WasError
  T value; // valid if whatType == NotError
};

typedef int VariableID;
typedef int DegreeOfDifferentiation;

class RawResult {
public:
  double bvarValue;
  std::vector<double> rawData;
};

class DAEIntegrationResult {
public:
  std::map<VariableID, std::list<std::tr1::shared_ptr<PathToVariableFromModel> > > variablePaths;
  std::map<std::pair<VariableID, DegreeOfDifferentiation>, int> variableIndexInResult;
  std::list<std::tr1::shared_ptr<RawResult> > rawResults;
};

class SolveCode {
public:
  std::map<VariableID, std::list<std::tr1::shared_ptr<PathToVariableFromModel> > > variablePaths;
  // The second pair gives the indices in the constant and variable arrays,
  // respectively. A value of -1 means no entry in the respective array.
  std::map<std::pair<VariableID, DegreeOfDifferentiation>,
           std::pair<int, int> > variableStorage;
  std::string code;
};

/* Note: You must create exactly one of these, and it must stay in scope for
         the duration of your use of the CellML testbed. */
class CellMLAPI
{
public:
  CellMLAPI();
  ~CellMLAPI();

  PossiblyError<std::tr1::shared_ptr<SolveCode> >
  generateSolveCode(
                    const std::string& aModelURL,
                    PathToVariableFromModel* aBVar
                   );

  PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> >
  solveModelWithParameters(const std::string& aModelURL,
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
                          );
};

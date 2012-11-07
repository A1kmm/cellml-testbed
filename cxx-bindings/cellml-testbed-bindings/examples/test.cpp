#include <cstdlib>
#include "cellml-testbed.h"
#include <iostream>

int
main(int argc, char** argv)
{
  if (argc < 4)
  {
    std::cout << "Usage: test modelURL timeComp timeVar" << std::endl;
    return 1;
  }

  CellMLAPI ca;

  PathToVariableFromModel time;
  time.whatType = PathToVariableFromModel::GoToComponent;
  time.name = argv[2];
  time.toComponent.reset(new PathToVariableFromComponent);
  time.toComponent->variableName = argv[3];
  PossiblyError<std::tr1::shared_ptr<SolveCode> >
    code(ca.generateSolveCode(argv[1], &time));

  if (code.whatType == PossiblyError<std::tr1::shared_ptr<SolveCode> >::WasError)
  {
    std::cout << code.errMsg << std::endl;
    return 2;
  }

  std::cout << code.value->code << std::endl;

  std::list<std::pair<std::pair<std::tr1::shared_ptr<PathToVariableFromModel>,
                                DegreeOfDifferentiation>,
                      double> > l;

  // Now run a simulation...
  PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> > simr =
    ca.solveModelWithParameters(argv[1], &time, l, 0, 10, 1E-6, 1E-6);
  if (simr.whatType == PossiblyError<std::tr1::shared_ptr<DAEIntegrationResult> >::WasError)
  {
    std::cout << simr.errMsg << std::endl;
    return 3;
  }
  
  for (
       std::list<std::tr1::shared_ptr<RawResult> >::iterator i = simr.value->rawResults.begin();
       i != simr.value->rawResults.end();
       i++
      )
  {
    std::cout << (*i)->bvarValue << ": ";
    for (std::vector<double>::iterator j = (*i)->rawData.begin(); j != (*i)->rawData.end(); j++)
      std::cout << *j << " ";
    std::cout << std::endl;
  }

  return 0;
}

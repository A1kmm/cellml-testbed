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

  return 0;
}

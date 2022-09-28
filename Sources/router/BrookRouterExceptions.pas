unit BrookRouterExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BrookException;

type
  { Handles exceptions for @link(TBrookRoutes). }
  EBrookRoutes = class(EBrook);

  { Handles exceptions for @link(TBrookRouter). }
  EBrookRouter = class(EBrook);


implementation

end.


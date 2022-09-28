(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ Router handler class. }

unit BrookRouterHandler;

{$I BrookDefines.inc}

interface

uses
  BrookRouter;

type
  { Is a metaclass for @link(BrookRouterHandler) class. }
  TBrookRouterHandlerClass = class of TBrookRouterHandler;

  { Handles the router events. }
  TBrookRouterHandler = class(TBrookRouter)
  published
    property AfterExecuteAction;
    property AfterMatchPattern;
    property AfterRoute;
    property BeforeExecuteAction;
    property BeforeMatchPattern;
    property BeforeRoute;
  end;

implementation

end.

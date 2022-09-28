(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ FCL CGI broker. }

unit BrookFCLCGIBroker;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  BrookClasses, BrookApplication, BrookLog, BrookMessages, BrookConsts,
  BrookHTTPConstants, BrookRouter, BrookUtils, BrookHttpDefsBroker, HttpDefs,
  CustWeb, CustCGI, Classes, SysUtils;

implementation

uses
  BrookGlobal;

end.

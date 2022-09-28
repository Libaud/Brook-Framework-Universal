(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ FCL EventLog handler. }

unit BrookFCLEventLogHandler;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  BrookFCLEventLogBroker;

type
  { Is a metaclass for @link(TBrookFCLEventLogHandler) class. }
  TBrookFCLEventLogHandlerClass = class of TBrookFCLEventLogHandler;

  { Handles the logger features. }
  TBrookFCLEventLogHandler = class(TBrookFCLEventLog)
  published
    property Active;
    property FileName;
    property Output;
    property Types;
    property AfterLog;
    property BeforeLog;
  end;

implementation

end.


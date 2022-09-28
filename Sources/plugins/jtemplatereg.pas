(*
  J-Template plugin.
  Copyright (C) 2012-2014 Silvio Clecio.

  Please see the LICENSE, README and AUTHORS files.
*)

unit JTemplateReg;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  LResources, Classes, JTemplate;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JTemplate', [TJTemplate]);
end;

initialization
  {$i jtemplatereg.lrs}

end.

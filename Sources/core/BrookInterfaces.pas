unit BrookInterfaces;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

const
  BROOK_GUID = '{D2BDD8EF-78C0-47CD-95C3-664CDFFDAA9E}';
  BROOK_APP_GUID = '{669B03B7-AA2D-4B64-AAFC-4FBD4A41267E}';

type
  { Is the main interface for Brook. }
  IBrookInterface = interface(IInterface)
    [BROOK_GUID]
  end;


implementation

end.


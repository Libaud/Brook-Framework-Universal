unit BrookDatas;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

type
  { Is the main data module for Brook. }
  TBrookDataModule = class(TDataModule)
      public
        constructor Create(AOwner: TComponent); override;
    end;

implementation

uses
  RTLConsts;

{ TBrookDataModule }

constructor TBrookDataModule.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  if (ClassType <> TBrookDataModule) and not (csDesigning in ComponentState) then
  begin
    if not InitInheritedComponent(Self, TBrookDataModule) then
      raise EStreamError.CreateFmt(SErrNoStreaming, [ClassName]);
    if OldCreateOrder then
      DoCreate;
  end;
end;

end.


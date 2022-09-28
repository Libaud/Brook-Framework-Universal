(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ Choose project type unit. }

unit frmBrookChooseProjectType;

{$I BrookDefines.inc}

interface

uses
  Forms, ExtCtrls, Buttons, Controls;

type
  TfrBrookChooseProjectType = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    pnCliente: TPanel;
    pnBottom: TPanel;
    rgType: TRadioGroup;
    sbClient: TScrollBox;
  public
    class function Execute: Integer;
  end;

implementation

{$ifdef LAZARUS}
{$R *.lfm}
{$else}
{$R *.frm}
{$endif}

class function TfrBrookChooseProjectType.Execute: Integer;
begin
  with Self.Create(nil) do
    try
      Result := -1;
      ShowModal;
      if ModalResult <> mrOK then
        Exit;
      Result := rgType.ItemIndex;
    finally
      Free;
    end;
end;

end.


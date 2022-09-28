(*  _                     _
 * | |__  _ __ ___   ___ | | __
 * | '_ \| '__/ _ \ / _ \| |/ /
 * | |_) | | | (_) | (_) |   <
 * |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2021 Silvio Clecio <silvioprog@gmail.com>
 *
 * Brook framework is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Brook framework; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

{ Integrates Brook to Delphi or Lazarus IDE. }

unit BrookIDEIntegration;

{$I BrookDefines.inc}

{$IFDEF FPC}
 {$WARN 5024 OFF}
{$ENDIF}

interface

uses
  SysUtils,
  Classes,
  TypInfo,
  Dialogs,
{$IFDEF LCL}
  PropEdits,
  ComponentEditors,
{$ELSE}
  DesignIntf,
  DesignEditors,
  ColnEdit,
{$ENDIF}
  LibSAGuiBasedTypes,
  BrookUtility;

resourcestring
  SBrookSelectLibraryTitle = 'Select library ...';
  SBrookSharedLibraryFilter = 'Shared libraries (%s)|%s|All files (*.*)|*.*';
  SBrookURLEntryPointsEditor = 'URL entry-points editor ...';
  SBrookURLRoutesEditor = 'URL routes editor ...';

type

  { TBrookLibraryNamePropertyEditor }

  TBrookLibraryNamePropertyEditor = class(
{$IFDEF LCL}TFileNamePropertyEditor{$ELSE}TStringProperty{$ENDIF})
  public
{$IFDEF LCL}
    function GetVerbCount: Integer; override;
    function GetVerb(AIndex: Integer): string; override;
    procedure ExecuteVerb(AIndex: Integer); override;
{$ENDIF}
    function GetFilter: string;{$IFDEF LCL}override{$ELSE}virtual{$ENDIF};
    function GetDialogTitle: string;{$IFDEF LCL}override{$ELSE}virtual{$ENDIF};
{$IFNDEF LCL}
    function CreateFileDialog: TOpenDialog; virtual;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetDialogOptions: TOpenOptions; virtual;
    function GetInitialDirectory: string; virtual;
    procedure SetFileName(const AFileName: string); virtual;
{$ENDIF}
  end;

  { TBrookHTTPRequestMethodsPropertyEditor }

  TBrookHTTPRequestMethodsPropertyEditor = class(TSetProperty)
  public
    procedure GetProperties(AProc:
{$IFDEF LCL}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF}); override;
  end;

  { TBrookLibraryNameComponentEditor }

  TBrookLibraryNameComponentEditor = class(TDefaultEditor)
  public
    procedure Edit; override;
    function GetVerb(AIndex: Integer): string; override;
{$IFNDEF LCL}
    function GetVerbCount: Integer; override;
{$ENDIF}
    procedure ExecuteVerb(AIndex: Integer); override;
  end;

  { TBrookOnMathExtensionComponentEditor }

  TBrookOnMathExtensionComponentEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const AProperty:
{$IFDEF LCL}TPropertyEditor{$ELSE}IProperty{$ENDIF};
      var AContinue: Boolean); override;
  end;

  { TBrookOnHTTPRequestComponentEditor }

  TBrookOnHTTPRequestComponentEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const AProperty:
{$IFDEF LCL}TPropertyEditor{$ELSE}IProperty{$ENDIF};
      var AContinue: Boolean); override;
  end;

  { TBrookURLRouterComponentEditor }

  TBrookURLRouterComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TBrookURLEntryPointsComponentEditor }

  TBrookURLEntryPointsComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(AIndex: Integer); override;
    function GetVerb(AIndex: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$R BrookTardigradeIcons.res}

implementation

uses
  BrookLibraryLoader,
  BrookLogger,
  BrookMathExpression,
  BrookMediaTypes,
  BrookURLEntryPoints,
  BrookURLRouter,
  BrookHTTPServer;

{$IFNDEF LCL}

type
  TLocalSetElementProperty = class(TSetElementProperty)
  public
    constructor Create(AParent: TPropertyEditor; AElement: Integer); reintroduce;
  end;

constructor TLocalSetElementProperty.Create(AParent: TPropertyEditor;
  AElement: Integer);
begin
  inherited Create(AParent, AElement);
end;

function BrookHTTPRequestMethodsPropertyMapper(AObj: TPersistent;
  APropInfo: PPropInfo): TPropertyEditorClass;
begin
  if Assigned(AObj) and (AObj is TBrookURLRoute) and
    SameText(APropInfo.NameFld.ToString, 'Methods') then
    Exit(TBrookHTTPRequestMethodsPropertyEditor);
  Result := nil;
end;

{$ENDIF}

{$IFDEF LCL}

{ TBrookLibraryNamePropertyEditor }

function TBrookLibraryNamePropertyEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TBrookLibraryNamePropertyEditor.GetVerb(AIndex: Integer): string;
begin
  Result := SBrookSelectLibraryTitle;
end;

procedure TBrookLibraryNamePropertyEditor.ExecuteVerb(AIndex: Integer);
begin
  Edit;
end;

{$ENDIF}

function TBrookLibraryNamePropertyEditor.GetFilter: string;
var
  VSharedSuffix: string;
begin
  VSharedSuffix := Concat('*.', SharedSuffix);
  Result := Format(SBrookSharedLibraryFilter, [VSharedSuffix,
{$IFDEF LINUX}Concat({$ENDIF}VSharedSuffix
{$IFDEF LINUX}, ';', VSharedSuffix, '.*'){$ENDIF}]);
end;

function TBrookLibraryNamePropertyEditor.GetDialogTitle: string;
begin
  Result := SBrookSelectLibraryTitle;
end;

{$IFNDEF LCL}

function TBrookLibraryNamePropertyEditor.CreateFileDialog: TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
end;

procedure TBrookLibraryNamePropertyEditor.Edit;
begin
  with CreateFileDialog do
  try
    Filter := GetFilter;
    Options := GetDialogOptions;
    FileName := GetStrValue;
    InitialDir := GetInitialDirectory;
    Title := GetDialogTitle;
    if Execute then
      SetFileName(FileName);
  finally
    Free;
  end;
end;

function TBrookLibraryNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

function TBrookLibraryNamePropertyEditor.GetDialogOptions: TOpenOptions;
begin
  Result := [ofEnableSizing];
end;

function TBrookLibraryNamePropertyEditor.GetInitialDirectory: string;
begin
  Result := '';
end;

procedure TBrookLibraryNamePropertyEditor.SetFileName(
  const AFileName: string);
begin
  SetStrValue(AFileName);
end;

{$ENDIF}

{ TBrookHTTPRequestMethodsPropertyEditor }

procedure TBrookHTTPRequestMethodsPropertyEditor.GetProperties(
  AProc:{$IFDEF LCL}TGetPropEditProc{$ELSE}TGetPropProc{$ENDIF});
var
  M: TBrookHTTPRequestMethod;
{$IFNDEF LCL}
  P: IProperty;
{$ENDIF}
begin
  for M := Succ(Low(TBrookHTTPRequestMethod)) to High(TBrookHTTPRequestMethod) do
{$IFDEF LCL}
    AProc(TSetElementProperty.Create(Self, Ord(M)));
{$ELSE}
  begin
    P := TLocalSetElementProperty.Create(Self, Ord(M));
    AProc(P);
    P := nil;
  end;
{$ENDIF}
end;

{ TBrookLibraryNameComponentEditor }

procedure TBrookLibraryNameComponentEditor.Edit;
var
  VDialog: TOpenDialog;
  VLibraryLoader: TBrookLibraryLoader;
  VPropertyEditor: TBrookLibraryNamePropertyEditor;
begin
  VLibraryLoader := Component as TBrookLibraryLoader;
  if not Assigned(VLibraryLoader) then
    Exit;
  VPropertyEditor := TBrookLibraryNamePropertyEditor.Create(nil, 0);
  VDialog := VPropertyEditor.CreateFileDialog;
  try
    VDialog.Filter := VPropertyEditor.GetFilter;
    VDialog.Options := VPropertyEditor.GetDialogOptions;
    VDialog.InitialDir := VPropertyEditor.GetInitialDirectory;
    VDialog.Title := VPropertyEditor.GetDialogTitle;
    VDialog.FileName := VLibraryLoader.LibraryName;
    if VDialog.Execute then
    begin
      VLibraryLoader.LibraryName := VDialog.FileName;
      Designer.Modified;
    end;
  finally
    VPropertyEditor.Free;
  end;
end;

function TBrookLibraryNameComponentEditor.GetVerb(
  AIndex: Integer): string;
begin
  Result := SBrookSelectLibraryTitle;
end;

{$IFNDEF LCL}

function TBrookLibraryNameComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$ENDIF}

procedure TBrookLibraryNameComponentEditor.ExecuteVerb(AIndex: Integer);
begin
  Edit;
end;

{ TBrookOnMathExtensionComponentEditor }

procedure TBrookOnMathExtensionComponentEditor.EditProperty(const AProperty:
{$IFDEF LCL}TPropertyEditor{$ELSE}IProperty{$ENDIF}; var AContinue: Boolean);
begin
  if SameText(AProperty.GetName, 'OnExtension') then
    inherited EditProperty(AProperty, AContinue);
end;

{ TBrookOnHTTPRequestComponentEditor }

procedure TBrookOnHTTPRequestComponentEditor.EditProperty(const AProperty:
{$IFDEF LCL}TPropertyEditor{$ELSE}IProperty{$ENDIF}; var AContinue: Boolean);
begin
  if SameText(AProperty.GetName, 'OnRequest') then
    inherited EditProperty(AProperty, AContinue);
end;

{ TBrookURLRouterComponentEditor }

procedure TBrookURLRouterComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VRouter: TBrookURLRouter;
begin
  VRouter := GetComponent as TBrookURLRouter;
{$IFDEF LCL}
  EditCollection(
{$ELSE}
  ShowCollectionEditor(Designer,
{$ENDIF}
    VRouter, VRouter.Routes, 'Routes');
end;

function TBrookURLRouterComponentEditor.GetVerb(AIndex: Integer): string;
begin
  Result := SBrookURLRoutesEditor;
end;

function TBrookURLRouterComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBrookURLEntryPointsComponentEditor }

procedure TBrookURLEntryPointsComponentEditor.ExecuteVerb(AIndex: Integer);
var
  VEntryPoints: TBrookURLEntryPoints;
begin
  VEntryPoints := GetComponent as TBrookURLEntryPoints;
{$IFDEF LCL}
  EditCollection(
{$ELSE}
  ShowCollectionEditor(Designer,
{$ENDIF}
    VEntryPoints, VEntryPoints.List, 'List');
end;

function TBrookURLEntryPointsComponentEditor.GetVerb(AIndex: Integer): string;
begin
  Result := SBrookURLEntryPointsEditor;
end;

function TBrookURLEntryPointsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.


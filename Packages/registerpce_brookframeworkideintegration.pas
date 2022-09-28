unit RegisterPCE_BrookFrameworkIDEIntegration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

uses
  ComponentEditors, PropEdits, BrookIDEIntegration, BrookLibraryLoader, BrookMediaTypes, BrookHTTPServer,
  BrookURLRouter, BrookUtility, BrookMathExpression, BrookURLEntryPoints;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TFileName), TBrookLibraryLoader,
    'LibraryName', TBrookLibraryNamePropertyEditor);
{$IFDEF LCL}
  RegisterPropertyEditor(TypeInfo(TFileName), TBrookMIME,
    'FileName', TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TBrookHTTPServerSecurity,
    'PrivatePassword', TPasswordStringPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBrookHTTPRequestMethods), nil, '',
    TBrookHTTPRequestMethodsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TBrookHTTPServer, 'UploadsDir',
    TDirectoryPropertyEditor);
{$ELSE}
  RegisterPropertyMapper(BrookHTTPRequestMethodsPropertyMapper);
{$ENDIF}
  RegisterComponentEditor(TBrookLibraryLoader, TBrookLibraryNameComponentEditor);
  RegisterComponentEditor(TBrookMathExpression, TBrookOnMathExtensionComponentEditor);
  RegisterComponentEditor(TBrookURLEntryPoints, TBrookURLEntryPointsComponentEditor);
  RegisterComponentEditor(TBrookURLRouter, TBrookURLRouterComponentEditor);
  RegisterComponentEditor(TBrookHTTPServer, TBrookOnHTTPRequestComponentEditor);
end;

end.


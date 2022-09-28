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

{ Contains classes for media types parsing. }

unit BrookMediaTypes;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
  SysUtils,
  Classes,
  BrookExtra,
  BrookReader,
  BrookHandledClasses,
  BrookStringMap, LibSAGuiAPITypes;

const
  { Default MIME types file name. }
  BROOK_MIME_FILE = 'mime.types';
  { Register prefix for MIME types class. }
  BROOK_MIME_TAG = 'BrookMIME_';
  { Default MIME provider. }
  BROOK_MIME_PROVIDER =
{$IF DEFINED(MSWINDOWS)}
    'Windows'
{$ELSEIF DEFINED(UNIX) OR DEFINED(POSIX)}
    'Unix'
{$ELSE}
    'Path'
{$ENDIF};

resourcestring
  { Error message @code('Invalid media type: <media-type>.'). }
  SBrookInvalidMediaType = 'Invalid media type: %s.';
  { Error message @code('Invalid media extension: <ext>.'). }
  SBrookInvalidMediaExt = 'Invalid media extension: %s.';
  { Error message @code('Empty media type'). }
  SBrookEmptyMediaType = 'Empty media type.';
  { Error message @code('Empty media extension'). }
  SBrookEmptyMediaExt = 'Empty media extension.';
  { Error message @code('Active MIME types'). }
  SBrookActiveMIMETypes = 'Active MIME types.';
  { Error message @code('Inactive MIME types'). }
  SBrookInactiveMIMETypes = 'Inactive MIME types.';
  { Error message @code('Empty MIME provider'). }
  SBrookEmptyMIMEProvider = 'Empty MIME provider.';
  { Error message @code('Invalid MIME provider class: <class-name>.'). }
  SBrookInvalidMIMEProviderClass = 'Invalid MIME provider class: %s.';
  { Error message @code('Unknown MIME provider: <unknown-mime>.'). }
  SBrookUnknownMIMEProvider = 'Unknown MIME provider: %s.';

type
  { Handles exceptions related to media type classes. }
  EBrookMediaTypes = class(Exception);

  { Cached abstract class to register, add, remove, find a media type. }
  TBrookMediaTypes = class abstract(TBrookHandledPersistent)
  private
    FCache: TBrookStringMap;
    FDefaultType: string;
    FHandle: Psg_strmap;
    procedure SetDefaultType(const AValue: string);
  protected
    function CreateCache: TBrookStringMap; virtual;
    function IsPrepared: Boolean; virtual; abstract;
    function GetHandle: Pointer; override;
    procedure CheckExt(const AExt: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckType(const AType: string); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckPrepared; {$IFNDEF DEBUG}inline;{$ENDIF}
    property Cache: TBrookStringMap read FCache;
  public
    { Creates an instance of @code(TBrookMediaTypes). }
    constructor Create; virtual;
    { Destroys an instance of @code(TBrookMediaTypes). }
    destructor Destroy; override;
    { Returns the alias name for media type source.
      @returns(Media type source alias.) }
    class function GetRegisterAlias: string; virtual;
    { Returns the description of the media types source.
      @returns(Description of the media types source.) }
    class function GetDescription: string; virtual; abstract;
    { Returns @True if a string represents a media type,
      e.g @code('text/plain').
      @param(AType[in] Media type identifier.)
      @returns(@True if a string represents a media type.) }
    class function IsValid(const AType: string): Boolean; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Returns @True if a string represents a text media type,
      e.g. @code('text/plain').
      @param(AType[in] Media type identifier.)
      @returns(@True if a string represents a text media type.) }
    class function IsText(const AType: string): Boolean; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Returns @True if a string represents a file extension,
      e.g. @code('text/plain').
      @param(AExt[in] File extension.)
      @returns(@True if a string represents a file extension.) }
    class function IsExt(const AExt: string): Boolean; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Normalizes file extension by adding a dot, e.g. a @code('pas') will be
      normalized to @code('.pas').
      @param(AExt[in] File extension.)
      @returns(Normalized file extension.) }
    class function NormalizeExt(const AExt: string): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Prepares the media types source. }
    procedure Prepare; virtual; abstract;
    { Adds a new media type to the cache.
      @param(AExt[in] File extension.)
      @param(AType[in] Media type identifier.) }
    procedure Add(const AExt, AType: string); virtual;
    { Removes a media type from the cache.
      @param(AExt[in] File extension.) }
    procedure Remove(const AExt: string); virtual;
    { If the cache is not prepared yet, this method prepares it automatically
      and tries to find a media type identifier by file extension.
      @param(AExt[in] File extension.)
      @param(AType[in] Media type identifier.)
      @returns(@True if the media type identifier is found.) }
    function TryType(const AExt: string; out AType: string): Boolean; virtual;
    { Finds a media type identifier by file extension. If the cache is not
      prepared yet, this method prepares it automatically. If a media type
      identifier is not found, the @code(ADefType) is returned instead.
      @param(AExt[in] File extension.)
      @param(ADefType[in] Default media type identifier.)
      @returns(Media type identifier.) }
    function Find(const AExt, ADefType: string): string; overload; virtual;
    { Finds a media type identifier by file extension. If the cache is not
      prepared yet, this method prepares it automatically. If a media type
      identifier is not found, the @code(DefaultType) is returned instead.
      @param(AExt[in] File extension.)
      @returns(Media type identifier.) }
    function Find(const AExt: string): string; overload; virtual;
    { Counts all media type identifiers present in the cache.
      @return(All media type identifiers present in the cache.) }
    function Count: Integer; virtual;
    { Clears all media type identifiers present in the cache. }
    procedure Clear; virtual;
    { Default media type identifier returned by @code(Find). }
    property DefaultType: string read FDefaultType write SetDefaultType;
    { @True if the media types cache is prepared. }
    property Prepared: Boolean read IsPrepared; //FI:C110
  end;

  { Class-reference for @code(TBrookMediaTypes). }
  TBrookMediaTypesClass = class of TBrookMediaTypes;

  { Base class containing a basic media types parser. }
  TBrookMediaTypesParser = class(TPersistent)
  private
    FReader: TBrookTextReader;
    FTypes: TBrookMediaTypes;
  public
    { Creates an instance of @code(TBrookMediaTypesParser). }
    constructor Create(AReader: TBrookTextReader;
      ATypes: TBrookMediaTypes); virtual;
    { Parses a media types source passed by @code(Reader). }
    procedure Parse; virtual;
    { Line reader containing a media types source. }
    property Reader: TBrookTextReader read FReader;
    { Cached list containing all parsed media types. }
    property Types: TBrookMediaTypes read FTypes;
  end;

  { Media types parser for
    @html(<a href="https://github.com/nginx/nginx/blob/master/conf/mime.types">Nginx mime.types</a>). }
  TBrookMediaTypesParserNginx = class(TBrookMediaTypesParser)
  public
    { Parses a Nginx media types source. }
    procedure Parse; override;
  end;

  { Media types provider from the @code(mime.types) file. }
  TBrookMediaTypesPath = class(TBrookMediaTypes)
  private
    FParser: TBrookMediaTypesParser;
    FReader: TBrookTextReader;
    FFileName: string;
    FPrepared: Boolean;
  protected
    function CreateReader: TBrookTextReader; virtual;
    function CreateParser: TBrookMediaTypesParser; virtual;
    function IsPrepared: Boolean; override;
  public
    { Creates an instance of @code(TBrookMediaTypesPath).
      @param(AFileName[in] Media types file.) }
    constructor Create(const AFileName: string); reintroduce; overload; virtual;
    { Creates an instance of @code(TBrookMediaTypesPath). }
    constructor Create; overload; override;
    { Destroys an instance of @code(TBrookMediaTypesPath). }
    destructor Destroy; override;
    { Returns the description of the media types source.
      @returns(Description of the media types source.) }
    class function GetDescription: string; override;
    { Returns the file name of the media types source.
      @returns(File name of the media types source.) }
    class function GetFileName: TFileName; virtual;
    { Prepares the media types source. }
    procedure Prepare; override;
    { Clears the media types source. }
    procedure Clear; override;
    { Line reader containing a media types source. }
    property Reader: TBrookTextReader read FReader;
    { Media types parser containing a media types source. }
    property Parser: TBrookMediaTypesParser read FParser;
    { File name of the media types source. }
    property FileName: string read FFileName;
  end;

  { Class-reference for @code(TBrookMediaTypesPath). }
  TBrookMediaTypesPathClass = class of TBrookMediaTypesPath;

  { Media types provider from the
    @html(<a href="https://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types">Apache mime.types</a>). }
  TBrookMediaTypesApache = class(TBrookMediaTypesPath)
  public
    { Returns the description of the media types source.
      @returns(Description of the media types source.) }
    class function GetDescription: string; override;
  end;

  { Media types provider from the
    @html(<a href="https://github.com/nginx/nginx/blob/master/conf/mime.types">Nginx mime.types</a>). }
  TBrookMediaTypesNginx = class(TBrookMediaTypesPath)
  protected
    function CreateParser: TBrookMediaTypesParser; override;
  public
    { Returns the description of the media types source.
      @returns(Description of the media types source.) }
    class function GetDescription: string; override;
    { Returns the file name of the media types source.
      @returns(File name of the media types source.) }
    class function GetFileName: TFileName; override;
  end;

  { Media types provider from the Windows registry. }
  TBrookMediaTypesWindows = class(TBrookMediaTypesPath)
  public
    { Returns the description of the media types source.
      @returns(Description of the media types source.) }
    class function GetDescription: string; override;
  end;

  { Media types provider from the @code(/etc/mime.types). }
  TBrookMediaTypesUnix = class(TBrookMediaTypesPath)
  public
    { Returns the description of the media types source.
      @returns(Description of the media types source.) }
    class function GetDescription: string; override;
    { Returns the file name of the media types source.
      @returns(File name of the media types source.) }
    class function GetFileName: TFileName; override;
  end;

  { Provides all registered media types in any supported platform. }
  TBrookMIME = class(TBrookHandledComponent)
  private
    FDefaultType: string;
    FFileName: TFileName;
    FTypes: TBrookMediaTypes;
    FActive: Boolean;
    FStreamedActive: Boolean;
    FProvider: string;
    function GetTypes: TBrookMediaTypes;
    function IsActiveStored: Boolean;
    function IsDefaultTypeStored: Boolean;
    function IsFileNameStored: Boolean;
    function IsProviderStored: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetDefaultType(const AValue: string);
    procedure SetFileName(const AValue: TFileName);
    procedure SetProvider(const AValue: string);
    procedure InternalLibUnloadEvent(ASender: TObject);
  protected
    procedure Loaded; override;
    function CreateTypes(const AFileName: TFileName): TBrookMediaTypes; virtual;
    function GetHandle: Pointer; override;
    procedure DoOpen; virtual;
    procedure DoClose; virtual;
    procedure CheckProvider; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckActive; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure CheckInactive; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    { Creates an instance of @code(TBrookMIME).
      @param(AOwner[in] Owner component.) }
    constructor Create(AOwner: TComponent); override;
    { Destroys an instance of @code(TBrookMIME). }
    destructor Destroy; override;
    { Gets a media type class from the classes register. }
    function GetProviderClass: TBrookMediaTypesClass;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Opens the media types provider. }
    procedure Open;
    { Closes the media types provider. }
    procedure Close;
    { Cached list containing the parsed media types. }
    property Types: TBrookMediaTypes read GetTypes;
  published
    { Activates the cached media types provider. }
    property Active: Boolean read FActive write SetActive stored IsActiveStored;
    { Default media type identifier returned by @code(TBrookMediaTypes.Find). }
    property DefaultType: string read FDefaultType write SetDefaultType
      stored IsDefaultTypeStored;
    { File name of the media types source. }
    property FileName: TFileName read FFileName write SetFileName
      stored IsFileNameStored;
    { Media types provider description. }
    property Provider: string read FProvider write SetProvider
      stored IsProviderStored;
  end;

implementation

uses
  SAGuiClasses;

var
  GBrookMIMEFileName: TFileName;

{ TBrookMediaTypes }

constructor TBrookMediaTypes.Create;
begin
  inherited Create;
  FCache := CreateCache;
  SetDefaultType(BROOK_CT_OCTET_STREAM);
end;

destructor TBrookMediaTypes.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

function TBrookMediaTypes.CreateCache: TBrookStringMap;
begin
  Result := TBrookStringMap.Create(@FHandle);
end;

class function TBrookMediaTypes.GetRegisterAlias: string;
begin
  Result := Concat(BROOK_MIME_TAG, GetDescription);
end;

class function TBrookMediaTypes.IsValid(const AType: string): Boolean;
begin
  Result := AType.Length > 3;
end;

class function TBrookMediaTypes.IsText(const AType: string): Boolean;
begin
  Result := AType.StartsWith('text/') and IsValid(AType);
end;

class function TBrookMediaTypes.IsExt(const AExt: string): Boolean;
begin
  Result := (Length(AExt) > 0) and (AExt <> '.') and (AExt <> '..');
end;

class function TBrookMediaTypes.NormalizeExt(const AExt: string): string;
begin
  Result := AExt;
  if (Length(AExt) > 0) and (AExt[1] <> '.') then
    Result := Concat('.', Result);
end;

procedure TBrookMediaTypes.CheckExt(const AExt: string);
begin
  if AExt.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyMediaExt);
  if not IsExt(AExt) then
    raise EBrookMediaTypes.CreateFmt(SBrookInvalidMediaExt, [AExt]);
end;

procedure TBrookMediaTypes.CheckPrepared;
begin
  if not IsPrepared then
    Prepare;
end;

procedure TBrookMediaTypes.CheckType(const AType: string);
begin
  if AType.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyMediaType);
  if not IsValid(AType) then
    raise EBrookMediaTypes.CreateResFmt(@SBrookInvalidMediaType, [AType]);
end;

function TBrookMediaTypes.GetHandle: Pointer;
begin
  Result := FHandle;
end;

procedure TBrookMediaTypes.SetDefaultType(const AValue: string);
begin
  if FDefaultType = AValue then
    Exit;
  CheckType(AValue);
  FDefaultType := AValue;
end;

procedure TBrookMediaTypes.Add(const AExt, AType: string);
begin
  CheckExt(AExt);
  CheckType(AType);
  FCache.AddOrSet(NormalizeExt(AExt), AType);
end;

procedure TBrookMediaTypes.Remove(const AExt: string);
begin
  CheckExt(AExt);
  FCache.Remove(NormalizeExt(AExt));
end;

function TBrookMediaTypes.TryType(const AExt: string;
  out AType: string): Boolean;
begin
  CheckExt(AExt);
  CheckPrepared;
  Result := FCache.TryValue(NormalizeExt(AExt), AType);
end;

function TBrookMediaTypes.Find(const AExt, ADefType: string): string;
begin
  CheckExt(AExt);
  CheckType(ADefType);
  CheckPrepared;
  if not FCache.TryValue(NormalizeExt(AExt), Result) then
    Result := ADefType;
end;

function TBrookMediaTypes.Find(const AExt: string): string;
begin
  Result := Find(AExt, FDefaultType);
end;

function TBrookMediaTypes.Count: Integer;
begin
  Result := FCache.Count;
end;

procedure TBrookMediaTypes.Clear;
begin
  FCache.Clear;
end;

{ TBrookMediaTypesParser }

constructor TBrookMediaTypesParser.Create(AReader: TBrookTextReader;
  ATypes: TBrookMediaTypes);
begin
  inherited Create;
  if not Assigned(AReader) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['AReader']);
  if not Assigned(ATypes) then
    raise EArgumentNilException.CreateFmt(SParamIsNil, ['ATypes']);
  FReader := AReader;
  FTypes := ATypes;
end;

procedure TBrookMediaTypesParser.Parse;
var
  VPair, VExtensions: {$ifdef FPC}specialize{$endif} TArray<string>;
  VLine, VMediaType: string;
  VSep: Char;
  VSameSep: Boolean;
  I: Integer;
begin
  FTypes.Cache.Clear;
  VSep := #0;
  VSameSep := False;
  while not FReader.EOF do
  begin
    VLine := FReader.Read;
    if (Length(VLine) > 0) and (VLine[1] <> '#') then
    begin
      if VSep = #0 then
      begin
        if Pos(#9, VLine) > 0 then
          VSep := #9
        else
        begin
          if Pos(' ', VLine) = 0 then
            Continue;
          VSep := ' ';
        end;
        VSameSep := VSep = ' ';
      end;
      VPair := VLine.Split([VSep], TStringSplitOptions.ExcludeEmpty);
      if Length(VPair) > 1 then
      begin
        VMediaType := VPair[0];
        if VSameSep then
          for I := Succ(Low(VPair)) to High(VPair) do
            FTypes.Add(VPair[I], VMediaType)
        else
        begin
          VExtensions := VPair[1].Split([' '], TStringSplitOptions.ExcludeEmpty);
          for I := Low(VExtensions) to High(VExtensions) do
            FTypes.Add(VExtensions[I], VMediaType);
        end;
      end;
    end;
  end;
end;

{ TBrookMediaTypesParserNginx }

procedure TBrookMediaTypesParserNginx.Parse;
var
  I: Integer;
  VPair: {$ifdef FPC}specialize{$endif}TArray<string>;
  VLine, VMediaType: string;
begin
  while not FReader.EOF do
  begin
    VLine := Trim(FReader.Read);
    System.Delete(VLine, Length(VLine), Length(';'));
    if (Length(VLine) > 0) and (VLine[1] <> '#') and (VLine <> 'types ') then
    begin
      VPair := VLine.Split([' '], TStringSplitOptions.ExcludeEmpty);
      if Length(VPair) > 1 then
      begin
        VMediaType := VPair[0];
        for I := 1 to High(VPair) do
          FTypes.Add(VPair[I], VMediaType);
      end;
    end;
  end;
end;

{ TBrookMediaTypesPath }

constructor TBrookMediaTypesPath.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FReader := CreateReader;
  FParser := CreateParser;
end;

constructor TBrookMediaTypesPath.Create; //FI:W525
begin
  Create(GetFileName);
end;

destructor TBrookMediaTypesPath.Destroy;
begin
  FReader.Free;
  FParser.Free;
  inherited Destroy;
end;

function TBrookMediaTypesPath.CreateReader: TBrookTextReader;
begin
  Result := TBrookFileReader.Create(FFileName);
end;

function TBrookMediaTypesPath.CreateParser: TBrookMediaTypesParser;
begin
  Result := TBrookMediaTypesParser.Create(FReader, Self);
end;

class function TBrookMediaTypesPath.GetDescription: string;
begin
  Result := 'Path';
end;

class function TBrookMediaTypesPath.GetFileName: TFileName;
begin
  Result := GBrookMIMEFileName;
end;

function TBrookMediaTypesPath.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

procedure TBrookMediaTypesPath.Prepare;
begin
  if FPrepared then
    Exit;
  FParser.Parse;
  FPrepared := True;
end;

procedure TBrookMediaTypesPath.Clear;
begin
  if not FPrepared then
    Exit;
  inherited Clear;
  FPrepared := False;
end;

{ TBrookMediaTypesApache }

class function TBrookMediaTypesApache.GetDescription: string;
begin
  Result := 'Apache';
end;

{ TBrookMediaTypesNginx }

function TBrookMediaTypesNginx.CreateParser: TBrookMediaTypesParser;
begin
  Result := TBrookMediaTypesParserNginx.Create(FReader, Self);
end;

class function TBrookMediaTypesNginx.GetDescription: string;
begin
  Result := 'Nginx';
end;

class function TBrookMediaTypesNginx.GetFileName: TFileName;
begin
  Result := Concat(
{$IFDEF UNIX}'/etc/nginx/'{$ELSE}ExtractFilePath(ParamStr(0)){$ENDIF},
    BROOK_MIME_FILE);
end;

{ TBrookMediaTypesWindows }

class function TBrookMediaTypesWindows.GetDescription: string;
begin
  Result := 'Windows';
end;

{ TBrookMediaTypesUnix }

class function TBrookMediaTypesUnix.GetDescription: string;
begin
  Result := 'Unix';
end;

class function TBrookMediaTypesUnix.GetFileName: TFileName;
var
  FNs: {$ifdef FPC}specialize{$endif} TArray<TFileName>;
  FN: TFileName;
begin
  FNs := {$ifdef FPC}specialize{$endif}TArray<TFileName>.Create(
    Concat('/etc/', BROOK_MIME_FILE)
    // Put other 'mime.types' paths here...
  );
  for FN in FNs do
    if FileExists(FN) then
      Exit(FN);
  Result := BROOK_MIME_FILE;
end;

{ TBrookMIME }

constructor TBrookMIME.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TSgLib.UnloadEvents.Add({$ifdef FPC}@{$endif}InternalLibUnloadEvent, Self);
  FDefaultType := BROOK_CT_OCTET_STREAM;
  FFileName := GBrookMIMEFileName;
  FProvider := BROOK_MIME_PROVIDER;
end;

destructor TBrookMIME.Destroy;
begin
  try
    SetActive(False);
    TSgLib.UnloadEvents.Remove({$ifdef FPC}@{$endif}InternalLibUnloadEvent);
  finally
    inherited Destroy;
  end;
end;

function TBrookMIME.GetProviderClass: TBrookMediaTypesClass;
var
  D: string;
  C: TPersistentClass;
begin
  D := Concat(BROOK_MIME_TAG, FProvider);
  C := Classes.GetClass(D);
  if Assigned(C) and (not C.InheritsFrom(TBrookMediaTypes)) then
    raise EInvalidCast.CreateFmt(SBrookInvalidMIMEProviderClass, [C.ClassName]);
  Result := TBrookMediaTypesClass(C);
  if not Assigned(Result) then
    raise EClassNotFound.CreateFmt(SBrookUnknownMIMEProvider, [FProvider]);
end;

function TBrookMIME.CreateTypes(const AFileName: TFileName): TBrookMediaTypes;
var
  T: TBrookMediaTypesClass;
begin
  T := GetProviderClass;
  if T.InheritsFrom(TBrookMediaTypesPath) then
    Exit(TBrookMediaTypesPathClass(T).Create(AFileName));
  Result := T.Create;
  Result.DefaultType := FDefaultType;
end;

procedure TBrookMIME.CheckProvider;
begin
  if FProvider.IsEmpty then
    raise EArgumentException.Create(SBrookEmptyMIMEProvider);
end;

procedure TBrookMIME.CheckActive;
begin
  if not Active then
    raise EInvalidOpException.Create(SBrookInactiveMIMETypes);
end;

procedure TBrookMIME.CheckInactive;
begin
  if (not (csLoading in ComponentState)) and Active then
    raise EInvalidOpException.Create(SBrookActiveMIMETypes);
end;

procedure TBrookMIME.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      SetActive(True);
  except
    if csDesigning in ComponentState then
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr);
    end
    else
      raise;
  end;
end;

procedure TBrookMIME.InternalLibUnloadEvent(ASender: TObject);
begin
  if Assigned(ASender) then
    TBrookMIME(ASender).Close;
end;

function TBrookMIME.GetHandle: Pointer;
begin
  Result := FTypes.Handle;
end;

procedure TBrookMIME.DoOpen;
begin
  if Assigned(FTypes) then
    Exit;
  CheckProvider;
  FTypes := CreateTypes(FFileName);
  FActive := True;
end;

procedure TBrookMIME.DoClose;
begin
  if not Assigned(FTypes) then
    Exit;
  FTypes.Destroy;
  FTypes := nil;
  FActive := False;
end;

function TBrookMIME.IsActiveStored: Boolean;
begin
  Result := FActive;
end;

function TBrookMIME.GetTypes: TBrookMediaTypes;
begin
  CheckActive;
  Result := FTypes;
end;

function TBrookMIME.IsDefaultTypeStored: Boolean;
begin
  Result := FDefaultType <> BROOK_CT_OCTET_STREAM;
end;

function TBrookMIME.IsFileNameStored: Boolean;
begin
  Result := FFileName <> GBrookMIMEFileName;
end;

function TBrookMIME.IsProviderStored: Boolean;
begin
  Result := FProvider <> BROOK_MIME_PROVIDER;
end;

procedure TBrookMIME.SetActive(AValue: Boolean);
begin
  if AValue = FActive then
    Exit;
  if csDesigning in ComponentState then
  begin
    if not (csLoading in ComponentState) then
    begin
      if not FileExists(FFileName) then
        raise EFOpenError.CreateFmt(SFOpenError, [FFileName]);
      TSgLib.Check;
    end;
    FActive := AValue;
  end
  else
    if AValue then
    begin
      if csReading in ComponentState then
        FStreamedActive := True
      else
        DoOpen;
    end
    else
      DoClose;
end;

procedure TBrookMIME.SetDefaultType(const AValue: string);
begin
  if FDefaultType = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  if FDefaultType.IsEmpty or (not TBrookMediaTypes.IsValid(AValue)) then
    FDefaultType := BROOK_CT_OCTET_STREAM
  else
    FDefaultType := AValue;
end;

procedure TBrookMIME.SetFileName(const AValue: TFileName);
begin
  if FFileName = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FFileName := AValue;
  if FFileName = EmptyStr then
    FFileName := GBrookMIMEFileName;
end;

procedure TBrookMIME.SetProvider(const AValue: string);
begin
  if FProvider = AValue then
    Exit;
  if not FStreamedActive then
    CheckInactive;
  FProvider := AValue;
  if FProvider.IsEmpty then
    FProvider := BROOK_MIME_PROVIDER;
end;

procedure TBrookMIME.Open;
begin
  SetActive(True);
end;

procedure TBrookMIME.Close;
begin
  SetActive(False);
end;

initialization
  GBrookMIMEFileName := Concat(
{$IF DEFINED(UNIX) OR DEFINED(POSIX)}'/etc/'{$ELSE}ExtractFilePath(ParamStr(0)){$ENDIF},
    BROOK_MIME_FILE);
  RegisterClassAlias(TBrookMediaTypesPath, TBrookMediaTypesPath.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesApache, TBrookMediaTypesApache.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesNginx, TBrookMediaTypesNginx.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesWindows, TBrookMediaTypesWindows.GetRegisterAlias);
  RegisterClassAlias(TBrookMediaTypesUnix, TBrookMediaTypesUnix.GetRegisterAlias);

finalization
  UnregisterClass(TBrookMediaTypesPath);
  UnregisterClass(TBrookMediaTypesApache);
  UnregisterClass(TBrookMediaTypesNginx);
  UnregisterClass(TBrookMediaTypesWindows);
  UnregisterClass(TBrookMediaTypesUnix);

end.

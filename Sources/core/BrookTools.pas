unit BrookTools;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, DateUtils, TypInfo,
{$IFDEF FPC}
  SHA1, HttpProtocol
{$ELSE}
  System.Hash, System.NetEncoding
{$ENDIF};

type
  { Global Brook object containing general purpose functions. }
  TBrookTools = {$ifdef DELPHI}record{$else}class{$endif}
  public const
{$IFNDEF FPC}
  {$WRITEABLECONST ON}
{$ENDIF}
    { Holds the name of days as 'Aaa' format. }
    DAYS: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu',
      'Fri', 'Sat');
    { Holds the name of months as 'Aaa' format. }
    MONTHS: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May',
      'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
{$IFNDEF FPC}
  {$WRITEABLECONST OFF}
{$ENDIF}
    { Fixes a path by including the leading path delimiter and excluding the
      trailing one.
      @param(APath[in] Path as static string.)
      @returns(Fixed path, e.g.: path -> /path and /path/ -> /path) }
    class function FixPath(const APath: string): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Extracts and fixes an entry-point by including the leading path delimiter
      and excluding the trailing one.
      @param(APath[in] Path as static string.)
      @returns(Fixed entry-point, e.g.: /foo/bar -> /foo ) }
    class function FixEntryPoint(const APath: string): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Converts a given local time to UTC (Coordinated Universal Time).
      @param(ADateTime[in] Local date/time.)
      @returns(Local time converted to UTC.) }
    class function DateTimeToUTC(ADateTime: TDateTime): TDateTime; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Converts a given local time to GMT (Greenwich Mean Time).
      @param(ADateTime[in] Local date/time.)
      @returns(Local time converted to GMT string.) }
    class function DateTimeToGMT(ADateTime: TDateTime): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
    { Generates a given string to SHA-1 (Secure Hash Algorithm 1).
      @param(S[in] String to generate the SHA-1.)
      @returns(Generated SHA-1 as static string.) }
    class function SHA1(const S: string): string; static;
{$IFNDEF DEBUG}inline;{$ENDIF}
  end;


implementation

{ Brook }

class function TBrookTools.FixPath(const APath: string): string;
begin
  Result := APath;
  if not APath.StartsWith('/') then
    Result := Concat('/', Result);
  if (Length(APath) > SizeOf(Char)) and Result.EndsWith('/') then
    SetLength(Result, Length(Result) - Length('/'));
end;

class function TBrookTools.FixEntryPoint(const APath: string): string;
var
  PS: {$ifdef FPC}specialize{$endif} TArray<string>;
begin
  PS := APath.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  Result := '/';
  if Length(PS) > 0 then
    Result := Concat(Result, PS[0]);
end;

class function TBrookTools.DateTimeToUTC(ADateTime: TDateTime): TDateTime;
begin
  Result :=
{$IFDEF FPC}
    LocalTimeToUniversal
{$ELSE}
    TTimeZone.Local.ToUniversalTime
{$ENDIF}(ADateTime);
end;

class function TBrookTools.DateTimeToGMT(ADateTime: TDateTime): string;
var
  Y, M, D: Word;
begin
  DecodeDate(ADateTime, Y, M, D);
  DateTimeToString(Result, Format('"%s", dd "%s" yyy hh":"mm":"ss "GMT"', [
    DAYS[DayOfWeek(ADateTime)], MONTHS[M]]), ADateTime);
end;

class function TBrookTools.SHA1(const S: string): string;
begin
  Result :=
{$IFDEF FPC}SHA1Print(SHA1String(S)){$ELSE}THashSHA1.GetHashString(S){$ENDIF};
end;

end.


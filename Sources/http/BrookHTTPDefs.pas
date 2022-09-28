(*
  Brook for Free Pascal

  Copyright (C) 2014-2019 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{ HttpDefs classes. }

unit BrookHTTPDefs;

{$I BrookDefines.inc}

interface

uses
  HttpDefs;

type
  { Alias to @code(TRequest). }
  TBrookRequest = TRequest;

  { Alias to @code(TResponse). }
  TBrookResponse = TResponse;

  { Alias to @code(TUploadedFile). }
  TBrookUploadedFile = TUploadedFile;

  { Alias to @code(TUploadedFiles). }
  TBrookUploadedFiles = TUploadedFiles;

  { Alias to @code(TCookie). }
  TBrookCookie = TCookie;

  { Alias to @code(TCookies). }
  TBrookCookies = TCookies;

implementation

end.


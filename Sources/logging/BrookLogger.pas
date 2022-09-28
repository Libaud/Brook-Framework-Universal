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


{ Contains classes for basic logging. }

unit BrookLogger;

{$I BrookDefines.inc}

interface

uses
  RTLConsts,
{$IFDEF FPC}
  Math,
{$ELSE}
  Types,
  IOUtils,
{$ENDIF}
  SysUtils,
  DateUtils,
  Classes,
  BrookClasses, BrookExtra, BrookLoggingOutputs, BrookLoggingLevels;

type
  TBrookLoggerClass = class of TBrookLogger;

  TBrookLogger = class(TBrookComponent)

  end;

implementation

uses
  BrookLoggingConstants, BrookLoggingRessources;

initialization
  RegisterClassAlias(TBrookLoggerOutputConsole, TBrookLoggerOutputConsole.GetRegisterAlias);
  RegisterClassAlias(TBrookLoggerOutputFile, TBrookLoggerOutputFile.GetRegisterAlias);

finalization
  UnregisterClass(TBrookLoggerOutputConsole);
  UnregisterClass(TBrookLoggerOutputFile);

end.

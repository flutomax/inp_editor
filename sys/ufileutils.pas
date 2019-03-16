{ 
    Copyright (c) 2016-2019 by Vasily Makarov
	
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation(version 2);
    
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
    GNU General Public License version 2 for more details.
    
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit uFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  function ZFileExists(const FileName: string): Boolean;
  function ZFileReadOnly(const FileName: string): Boolean;
  function ZFileAccess(const FileName: string; Mode: Word): Boolean;
  function ZFileCreate(const FileName: string): THandle; overload; inline;
  function ZFileCreate(const FileName: string; Mode: LongWord): THandle; overload; inline;
  function ZFileCreate(const FileName: string; Mode, Rights: LongWord): THandle; overload;
  function ZFileOpen(const FileName: string; Mode: LongWord): THandle;
  function ZFileFlush(const Handle: THandle): Boolean;
  function ZReplaceInvalidFileNameChars(const aFileName: string; const aReplaceWith: Char = '_'): string;
  function ZIncFileName(const FileName: string): string;

implementation

uses
  {$IF DEFINED(MSWINDOWS)}
  Windows,
  {$ENDIF}
  {$IF DEFINED(UNIX)}
  Unix, BaseUnix,
  {$ENDIF}
  FileUtil, LazUTF8;

const

  InvalidFileNameChars = [#0..#31,'"', '*', '/', ':', '<', '>', '?', '\', '|'];


{$IF DEFINED(MSWINDOWS)}
const
  AccessModes: array[0..2] of DWORD = (GENERIC_READ,GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareModes: array[0..4] of DWORD = (0,0,FILE_SHARE_READ,FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE);
  OpenFlags: array[0..3] of DWORD = (0,FILE_FLAG_WRITE_THROUGH,
    FILE_FLAG_NO_BUFFERING,FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING);
{$ELSEIF DEFINED(UNIX)}
const

{$IF NOT DECLARED(O_SYNC)}
  O_SYNC = 0;
{$ENDIF}

{$IF NOT DECLARED(O_DIRECT)}
  O_DIRECT = 0;
{$ENDIF}
  AccessModes: array[0..2] of cInt = (O_RdOnly,O_WrOnly,O_RdWr);
  OpenFlags: array[0..3] of cInt = (0,O_SYNC,O_DIRECT,O_SYNC or O_DIRECT);
{$ENDIF}

const
{$IF DEFINED(LINUX)}
  FD_CLOEXEC = 1;
  O_CLOEXEC  = &02000000;
{$ELSEIF DEFINED(FREEBSD)}
  O_CLOEXEC  = &04000000;
{$ELSEIF DEFINED(NETBSD)}
  O_CLOEXEC  = $00400000;
{$ELSE}
  O_CLOEXEC  = 0;
{$ENDIF}

{$IF DEFINED(UNIX)}
procedure FileCloseOnExec(Handle: THandle); inline;
begin
  {$IF DECLARED(FD_CLOEXEC)}
    FpFcntl(Handle, F_SETFD, FpFcntl(Handle, F_GETFD) or FD_CLOEXEC);
  {$ENDIF}
end;

function FileLock(Handle: THandle; Mode: cInt): THandle;
var
  lockop: cint;
  lockres: cint;
  lockerr: cint;
begin
  Result:= Handle;
  case (Mode and $F0) of
    fmShareCompat,
    fmShareExclusive:
      lockop:=LOCK_EX or LOCK_NB;
    fmShareDenyWrite:
      lockop:=LOCK_SH or LOCK_NB;
    else
      Exit;
  end;
  repeat
    lockres:= fpFlock(Handle, lockop);
  until (lockres=0) or (fpgeterrno<>ESysEIntr);
  lockerr:= fpgeterrno;
  if (lockres <> 0) and ((lockerr=ESysEAGAIN)
    or (lockerr=ESysEDEADLK)) then begin
    Result:=-1;
    FileClose(Handle);
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function UTF16LongName(const FileName: String): UnicodeString;
var
  Temp: PWideChar;
begin
  if Pos('\\',FileName)=0 then
    Result:='\\?\'+UTF8Decode(FileName)
  else
    Result:='\\?\UNC\'+UTF8Decode(Copy(FileName,3,MaxInt));
  Temp:=Pointer(Result)+4;
  while Temp^<>#0 do begin
    if Temp^='/' then
      Temp^:='\';
    Inc(Temp);
  end;
  if ((Temp-1)^=DriveSeparator) then
    Result:=Result+'\';
end;
{$ENDIF}

function ZFileAccess(const FileName: string; Mode: Word): Boolean;
{$IFDEF MSWINDOWS}
var
  hFile: System.THandle;
  dwDesiredAccess: DWORD;
  dwShareMode: DWORD = 0;
begin
  dwDesiredAccess:=AccessModes[Mode and 3];
  if Mode=fmOpenRead then // If checking Read mode no sharing mode given
    Mode:=Mode or fmShareDenyNone;
  dwShareMode:=ShareModes[(Mode and $F0) shr 4];
  hFile:=CreateFileW(PWideChar(UTF16LongName(FileName)),dwDesiredAccess,dwShareMode,
    nil,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS,0);
  Result:=hFile<>INVALID_HANDLE_VALUE;
  if Result then
    FileClose(hFile);
end;
{$ELSE}
const
  AccessMode: array[0..2] of LongInt = (R_OK,W_OK,R_OK or W_OK);
begin
  Result:=fpAccess(UTF8ToSys(FileName),AccessMode[Mode and 3])=0;
end;
{$ENDIF}

function ZFileReadOnly(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:=(GetFileAttributesW(PWideChar(UTF16LongName(FileName))) and faReadOnly)<> 0;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  result:=false;
  if fpLStat(UTF8ToSys(FileName),@Info)>=0 then
    result:=(Info.st_mode and S_IWUSR)=0;
end;
{$ENDIF}

function ZFileExists(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  Attr: DWORD;
begin
  Attr:=GetFileAttributesW(PWideChar(UTF16LongName(FileName)));
  if Attr<>$ffffffff then
    Result:=(Attr and FILE_ATTRIBUTE_DIRECTORY)=0
  else
    Result:=false;
end;
{$ELSE}
var
  Info: BaseUnix.Stat;
begin
  if fpStat(UTF8ToSys(FileName),Info)>=0 then
    Result:=fpS_ISREG(Info.st_mode)
  else
    Result:=False;
end;
{$ENDIF}

function ZFileCreate(const FileName: string; Mode, Rights: LongWord): THandle;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= CreateFileW(PWideChar(UTF16LongName(FileName)), GENERIC_READ or GENERIC_WRITE,
    ShareModes[(Mode and $F0) shr 4], nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL,
    OpenFlags[(Mode shr 16) and 3]);
end;
{$ELSE}
begin
  repeat
    Result:=fpOpen(UTF8ToSys(FileName),O_Creat or O_RdWr or O_Trunc or
      OpenFlags[(Mode shr 16) and 3] or O_CLOEXEC,Rights);
  until (Result<>-1) or (fpgeterrno<>ESysEINTR);
  if Result<>feInvalidHandle then
  begin
    FileCloseOnExec(Result);
    Result:=FileLock(Result,Mode and $FF);
  end;
end;
{$ENDIF}

function ZFileCreate(const FileName: string; Mode: LongWord): THandle;
begin
  Result:=ZFileCreate(FileName, Mode, 438); // 438 is 666 octal
end;

function ZFileCreate(const FileName: string): System.THandle;
begin
  Result:=ZFileCreate(FileName,fmShareDenyWrite);
end;

function ZFileOpen(const FileName: string; Mode: LongWord): THandle;
{$IFDEF MSWINDOWS}
const
  ft: TFileTime = (dwLowDateTime: $FFFFFFFF; dwHighDateTime: $FFFFFFFF;);
  fmOpenNoATime = $40000;
begin
  Result:=CreateFileW(PWideChar(UTF16LongName(FileName)),
    AccessModes[Mode and 3] or ((Mode and fmOpenNoATime) shr 10),
    ShareModes[(Mode and $F0) shr 4],nil,OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,OpenFlags[(Mode shr 16) and 3]);
  if (Mode and fmOpenNoATime<>0) then begin
    if (Result<>feInvalidHandle) then
      SetFileTime(Result,nil,@ft,@ft)
    else if GetLastError=ERROR_ACCESS_DENIED then
      Result:=ZFileOpen(FileName,Mode and not fmOpenNoATime);
  end;
end;
{$ELSE}
begin
  repeat
    Result:=fpOpen(UTF8ToSys(FileName),AccessModes[Mode and 3] or
      OpenFlags[(Mode shr 16) and 3] or O_CLOEXEC);
  until (Result <>-1) or (fpgeterrno<>ESysEINTR);
  if Result<>feInvalidHandle then begin
    FileCloseOnExec(Result);
    Result:=FileLock(Result,Mode and $FF);
  end;
end;
{$ENDIF}

function ZFileFlush(const Handle: THandle): Boolean;
{$IFDEF MSWINDOWS}
begin
  Result:=FlushFileBuffers(Handle);
end;
{$ELSE}
begin
  Result:=(fpfsync(Handle) = 0);
end;
{$ENDIF}

function ZReplaceInvalidFileNameChars(const aFileName: string; const aReplaceWith: Char = '_'): string;
var
  i: integer;
begin
  Result := aFileName;
  for i := Low(Result) to High(Result) do
    if CharInSet(Result[i],InvalidFileNameChars) then
      Result[i] := aReplaceWith;
  end;

procedure ZDecodeFileName(const Input: string; out Stem, Ext: string; out Number: Integer);
var
  p: Integer;
begin
  Ext:=ExtractFileExt(Input);
  Stem:=ExtractFileNameWithoutExt(Input);
  Number:=0;
  p:=LastDelimiter('_',Stem);
  if p=0 then
    exit;
  if TryStrToInt(Copy(Stem,p+1,MaxInt),Number) then
    Stem:=Copy(Stem,1,p-1);
end;

function ZIncFileName(const FileName: string): string;
var
  Stem, Ext: string;
  Number: Integer;
begin
  ZDecodeFileName(FileName,Stem,Ext,Number);
  repeat
    Inc(Number);
    Result:=Format('%s_%d%s',[Stem,Number,Ext]);
  until not ZFileExists(Result);
end;

end.


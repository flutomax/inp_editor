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

unit uSysInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLVersion;



  function GetProgramVersion: string;
  function GetLazarusVersion: string;
  function GetFPCVersion: string;
  function GetBuildDate: string;
  function GetPlatform: string;
  function GetOSVersion: string;

implementation

uses
  {$IFDEF MSWINDOWS}
    Windows, JwaNative, JwaNtStatus, JwaWinType,
  {$ENDIF}
  {$IF DEFINED(UNIX)}
    BaseUnix, {DCOSUtils, uDCUtils, DCClassesUtf8,}
    {$IFDEF DARWIN}
      MacOSAll,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLQT}
    qt4,
  {$ENDIF}
  {$IFDEF LCLQT5}
    qt5,
  {$ENDIF}
  {$IFDEF LCLGTK2}
    gtk2,
  {$ENDIF}
  {$if (lcl_fullversion)>=1070000}
    LCLPlatformDef,
  {$endif}
  InterfaceBase, uFileUtils, uDerivedClasses, StrUtils, fileinfo;

{$I revision.inc}

const
  BuildDate   = {$I %DATE%};
  lazVersion  = lcl_version;         // Lazarus version (major.minor.micro)
  lazRevision = RevisionStr;         // Lazarus SVN revision
  fpcVersion  = {$I %FPCVERSION%};   // FPC version (major.minor.micro)
  TargetCPU   = {$I %FPCTARGETCPU%}; // Target CPU of FPC
  TargetOS    = {$I %FPCTARGETOS%};  // Target Operating System of FPC

var

  TargetWS: string;
  OSVersion: string;
  WSVersion: string;

function GetProgramVersion: string;
var
  pv: TProgramVersion;
begin
  if fileinfo.GetProgramVersion(pv) then
    result:=ProgramversionToStr(pv);
end;

function GetLazarusVersion: string;
var
  i: Integer = 1;
begin
  Result:=lazVersion;
  while (i<=Length(lazRevision)) and (lazRevision[i] in ['0'..'9']) do
    Inc(i);
  if i>1 then
    Result+='-'+Copy(lazRevision,1,i-1);
end;

function GetFPCVersion: string;
begin
  result:=fpcVersion;
end;

function GetBuildDate: string;
const
  dd: TSysCharSet = ['/'];
var
  s: string;
  y,m,d: Word;
begin
  // in local format
  y:=StrToIntDef(ExtractDelimited(1,BuildDate,dd),2019);
  m:=StrToIntDef(ExtractDelimited(2,BuildDate,dd),0);
  d:=StrToIntDef(ExtractDelimited(3,BuildDate,dd),0);
  result:=FormatDateTime('dd.mm.yyyy',EncodeDate(y,m,d));
end;

function GetPlatform: string;
begin
  result:=Format('%s-%s-%s',[TargetCPU,TargetOS,TargetWS]);
end;

function GetOSVersion: string;
begin
  result:=OSVersion;
  if WSVersion<>'' then
    result:=result+','+LineEnding+WSVersion;
end;

{$IF DEFINED(UNIX)}

function LoadStringList(const FileName: string; out sl: TStringListEx): Boolean;
begin
  Result:=false;
  sl:=nil;
  if ZFileAccess(FileName,fmOpenRead) then begin
    sl:=TStringListEx.Create;
    try
      sl.LoadFromFile(FileName);
      Result:=true;
    except
      on EFilerError do; // Bypass
    end;
  end;
end;

function LoadStringFromFile(const FileName: string; out Str: string): Boolean;
var
  sl: TStringListEx;
begin
  str:='';
  Result:=LoadStringList(FileName,sl);
  if Result then
  try
    if sl.Count>0 then
      Str:=sl.Strings[0];
  finally
    sl.Free;
  end;
end;

function FindStringInFile(const FileName, FindStr: string): Boolean;
var
  s: string;
begin
  Result:=LoadStringFromFile(FileName,s) and (FindStr=s);
end;

function GetOsFromLsbRelease: String;
var
  sl: TStringListEx;
begin
  Result:='';
  if LoadStringList('/etc/lsb-release',sl) then
  try
    if sl.Count>0 then begin
      Result:=sl.Values['DISTRIB_DESCRIPTION'];
      if Result<>'' then
        Result:=TrimSet(Result,['"',''''])
      else
        Result:=sl.Values['DISTRIB_ID']+sl.Values['DISTRIB_RELEASE'] +
          sl.Values['DISTRIB_CODENAME'];
    end;
  finally
    sl.Free;
  end;
end;

function GetOsFromProcVersion: String;
var
  i: Integer;
  s: string;
begin
  Result:='';
  if LoadStringFromFile('/proc/version',s) then begin
    // Get first three strings separated by space.
    i:=Pos(' ',s);
    if i>0 then
      Result:=Result+Copy(s,1,i);
    Delete(s,1,i);
    i:=Pos(' ',s);
    if i>0 then
      Result:=Result+Copy(s,1,i);
    Delete(s,1,i);
    i:=Pos(' ',s);
    if i>0 then
      Result:=Result+Copy(s,1,i-1);
    Delete(s,1,i);
  end;
end;

function GetOsFromIssue: string;
begin
  if not LoadStringFromFile('/etc/issue',Result) then
    Result:='';
end;

function GetDebianVersion: string;
var
  s: string;
begin
  if LoadStringFromFile('/etc/debian_version',s) then begin
    Result:='Debian';
    if s<>'' then
      Result:=Result+' '+s;
  end
  else
    Result:='';
end;

function GetSuseVersion: string;
begin
  if LoadStringFromFile('/etc/SuSE-release',Result) or
     LoadStringFromFile('/etc/suse-release',Result) then begin
    if Result='' then
      Result:='Suse';
  end else
    Result:='';
end;

function GetRedHatVersion: string;
begin
  if LoadStringFromFile('/etc/redhat-release', Result) then begin
    if Result='' then
      Result:='RedHat';
  end else
    Result:='';
end;

function GetMandrakeVersion: string;
begin
  if LoadStringFromFile('/etc/mandrake-release', Result) then begin
    if Result='' then
      Result:='Mandrake';
  end
  else
    Result:='';
end;

function GetVersionNumber: string;
var
  Info: utsname;
  i: Integer = 1;
begin
  FillChar(Info,SizeOf(Info),0);
  fpUname(Info);
  Result:=Info.release;
  while (i<=Length(Result)) and (Result[i] in ['0'..'9','.']) do
    Inc(i);
  Result:=Copy(Result,1,i-1);
end;

{$IFDEF DARWIN}
function GetMacOSXVersion: string;
var
  versionMajor,
  versionMinor, versionBugFix: SInt32;
begin
  Result:= EmptyStr;
  if (Gestalt(gestaltSystemVersionMajor, versionMajor) <> noErr) then Exit;
  if (Gestalt(gestaltSystemVersionMinor, versionMinor) <> noErr) then Exit;
  if (Gestalt(gestaltSystemVersionBugFix, versionBugFix) <> noErr) then Exit;
  Result:= Format('Mac OS X %d.%d.%d', [versionMajor, versionMinor, versionBugFix]);
end;
{$ENDIF}
{$ENDIF}


{$IF DEFINED(MSWINDOWS)}

function RegReadKey(ARoot: HKEY; const APath, AName: UnicodeString; out AValue: UnicodeString): Boolean;
var
  AKey: HKEY = 0;
  dwSize: DWORD = MaxSmallint;
begin
  Result:= RegOpenKeyExW(ARoot, PWideChar(APath), 0, KEY_READ, AKey) = ERROR_SUCCESS;
  if Result then
  begin
    SetLength(AValue, MaxSmallint);
    Result:= RegQueryValueExW(AKey, PWideChar(AName), nil, nil, PByte(AValue), @dwSize) = ERROR_SUCCESS;
    if Result then
    begin
      dwSize:= dwSize div SizeOf(WideChar);
      if (dwSize > 0) and (AValue[dwSize] = #0) then Dec(dwSize);
      SetLength(AValue, dwSize);
    end;
    RegCloseKey(AKey);
  end;
end;

procedure TryGetNativeSystemInfo(var SystemInfo: TSystemInfo);
type
  TGetNativeSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
var
  hLib: HANDLE;
  GetNativeSystemInfoProc: TGetNativeSystemInfo;
begin
  hLib := LoadLibrary(LPCTSTR('kernel32.dll'));
  if hLib <> 0 then
  begin
    try
      GetNativeSystemInfoProc := TGetNativeSystemInfo(GetProcAddress(hLib, 'GetNativeSystemInfo'));
      if Assigned(GetNativeSystemInfoProc) then
        GetNativeSystemInfoProc(SystemInfo)
      else
        GetSystemInfo(SystemInfo);
    finally
      FreeLibrary(hLib);
    end;
  end
  else
    GetSystemInfo(SystemInfo);
end;
{$ENDIF}


procedure InitializeVersionInfo;
{$IF DEFINED(MSWINDOWS)}
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  CURRENT_VERSION = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
var
  si: SYSTEM_INFO;
  osvi: TOsVersionInfoExW;
  ReleaseId: UnicodeString;
{$ENDIF}
begin
  TargetWS := LCLPlatformDirNames[WidgetSet.LCLPlatform];

  {$IF DEFINED(MSWINDOWS)}
  OSVersion := 'Windows';

  ZeroMemory(@osvi, SizeOf(TOsVersionInfoExW));
  osvi.dwOSVersionInfoSize := SizeOf(TOsVersionInfoExW);

  if (RtlGetVersion(@osvi) = STATUS_SUCCESS) or GetVersionExW(@osvi) then
  begin
    ZeroMemory(@si, SizeOf(si));
    TryGetNativeSystemInfo(si);

    case osvi.dwPlatformId of
      VER_PLATFORM_WIN32_WINDOWS:
        case osvi.dwMajorVersion of
          4: case osvi.dwMinorVersion of
                0: OSVersion := OSVersion + ' 95';
               10: OSVersion := OSVersion + ' 98';
               90: OSVersion := OSVersion + ' ME';
             end;
        end;

      VER_PLATFORM_WIN32_NT:
        begin
          case osvi.dwMajorVersion of
            3: OSVersion := OSVersion + ' NT 3.5';
            4: OSVersion := OSVersion + ' NT 4';
            5: case osvi.dwMinorVersion of
                 0: OSVersion := OSVersion + ' 2000';
                 1: begin
                      OSVersion := OSVersion + ' XP';
                      if osvi.wSuiteMask = $0000 then
                        OSVersion := OSVersion + ' Home'
                      else if osvi.wSuiteMask = $0200 then
                        OSVersion := OSVersion + ' Professional';
                    end;
                 2: if (osvi.wProductType = VER_NT_WORKSTATION) and
                       (si.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
                    begin
                      OSVersion := OSVersion + ' XP Professional x64'
                    end
                    else if (osvi.wProductType = VER_NT_SERVER) then
                    begin
                      if osvi.wSuiteMask = $8000 then
                        OSVersion := OSVersion + ' Home Server'
                      else
                        OSVersion := OSVersion + ' Server 2003';
                    end;
               end;
            6: case osvi.dwMinorVersion of
                 0: if (osvi.wProductType = VER_NT_WORKSTATION) then
                    begin
                      OSVersion := OSVersion + ' Vista';
                      if osvi.wSuiteMask = $0000 then
                        OSVersion := OSVersion + ' Ultimate'
                      else if osvi.wSuiteMask = $0200 then
                        OSVersion := OSVersion + ' Home';
                    end
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      OSVersion := OSVersion + ' Server 2008';
                 1: if (osvi.wProductType = VER_NT_WORKSTATION) then
                      OSVersion := OSVersion + ' 7'
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      OSVersion := OSVersion + ' Server 2008 R2';
                 2: if (osvi.wProductType = VER_NT_WORKSTATION) then
                      OSVersion := OSVersion + ' 8'
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      OSVersion := OSVersion + ' Server 2012';
                 3: if (osvi.wProductType = VER_NT_WORKSTATION) then
                      OSVersion := OSVersion + ' 8.1'
                    else if (osvi.wProductType = VER_NT_SERVER) then
                      OSVersion := OSVersion + ' Server 2012 R2';
               end;
           10: case osvi.dwMinorVersion of
                 0: if (osvi.wProductType = VER_NT_WORKSTATION) then
                    begin
                      OSVersion := OSVersion + ' 10';
                      if (osvi.wSuiteMask and VER_SUITE_PERSONAL <> 0) then
                        OSVersion := OSVersion + ' Home';
                      if RegReadKey(HKEY_LOCAL_MACHINE, CURRENT_VERSION, 'ReleaseId', ReleaseId) then
                        OSVersion := OSVersion + ' ' + String(ReleaseId);
                    end
              end;
          end;
        end;
    end;

    // If something detected then add service pack number and architecture.
    if OSVersion <> 'Windows' then
    begin
      if osvi.wServicePackMajor > 0 then
      begin
        OSVersion := OSVersion + ' SP' + IntToStr(osvi.wServicePackMajor);
        if osvi.wServicePackMinor > 0 then
          OSVersion := OSVersion + '.' + IntToStr(osvi.wServicePackMinor);
      end;

      if si.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_AMD64] then
        OSVersion := OSVersion + ' x86_64'
      else
        OSVersion := OSVersion + ' i386';
    end
    else
      OSVersion := OSVersion + ' Build ' + IntToStr(osvi.dwBuildNumber);
  end;

  {$ELSEIF DEFINED(UNIX)}
  // Try using linux standard base.
  OSVersion:=GetOsFromLsbRelease;

  // Try some distribution-specific files.
  if OSVersion='' then
    OSVersion:=GetDebianVersion;
  if OSVersion='' then
    OSVersion:=GetRedHatVersion;
  if OSVersion='' then
    OSVersion:=GetSuseVersion;
  if OSVersion='' then
    OSVersion:=GetMandrakeVersion;
  {$IFDEF DARWIN}
  if OSVersion='' then
    OSVersion:=GetMacOSXVersion;
  {$ENDIF}

  // Other methods.
  if OSVersion='' then
    OSVersion:=GetOsFromIssue;
  if OSVersion='' then
    OSVersion:=GetOsFromProcVersion;
  // Set default names.
  if OSVersion='' then
  begin
    {$IF DEFINED(LINUX)}
    OSVersion:='Linux';
    {$ELSEIF DEFINED(DARWIN)}
    OSVersion:='Darwin';  // MacOS
    {$ELSEIF DEFINED(FREEBSD)}
    OSVersion:='FreeBSD';
    {$ELSEIF DEFINED(BSD)}
    OSVersion:='BSD';
    {$ELSE}
    OSVersion:='Unix';
    {$ENDIF}
    OSVersion+=' '+GetVersionNumber;
  end;
  {$ENDIF}
  {$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  WSVersion := 'Qt ' + QtVersion + ', libQt' + QtVersion[0] + 'Pas ';

  WSVersion := WSVersion + IntToStr((QT_VERSION shr 16) and 255) + '.' +
                           IntToStr((QT_VERSION shr  8) and 255) + '.' +
                           IntToStr((QT_VERSION       ) and 255);
  {$ENDIF}

  {$IFDEF LCLGTK2}
  WSVersion:=Format('GTK %d.%d.%d',
    [gtk_major_version,gtk_minor_version,gtk_micro_version]);
  {$ENDIF}

end;


procedure Initialize;
begin
  LCLPlatformDirNames[lpQT]:='qt4';
  LCLPlatformDirNames[lpWin32]:='win32/win64';
  InitializeVersionInfo;
end;

initialization
  Initialize;


end.


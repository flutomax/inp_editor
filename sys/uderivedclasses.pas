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

unit uDerivedClasses;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, Graphics, IniFiles;

type

  { TFileStreamEx }
  TFileStreamEx = class(THandleStream)
  private
    fHandle: THandle;
    fFileName: string;
  public
    constructor Create(const AFileName: string; Mode: LongWord);
    destructor Destroy; override;
    function Flush: Boolean;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    property FileName: string read fFileName;
  end;

  { TStringStreamEx }

  TStringStreamEx = class(TStringStream)
  public
    procedure Write(const aString: string); overload;
    procedure Write(const aFmt: string; const Args: array of const); overload;
    procedure WriteLn; overload;
    procedure WriteLn(const aString: string); overload;
    procedure WriteLn(const aFmt: string; const Args: array of const); overload;
  end;

  { TStringListEx }

  TStringListEx = class(TStringList)
  public
    function IndexOfValue(const Value: string): Integer;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
  end;


  { TFileReader }

  TFileReader = class
  private
    fList: TStringListEx;
    fCurr: Integer;
    fFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const aFileName: string); overload;
    procedure Parse(lines: TStrings; const aFileName: string); overload;
    procedure Reset;
    function ReadLine: string;
    function Eof: Boolean;
    property CurrentLine: Integer read fCurr;
    property FileName: string read fFileName;
  end;

  { TIniFileEx }

  TIniFileEx = class(TMemIniFile)
  private
    fReadOnly: Boolean;
  public
    constructor Create(const aFileName: string; Mode: Word); virtual;
    constructor Create(const aFileName: string; aEscapeLineFeeds: Boolean = false); override;
    procedure UpdateFile; override;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteExtended(const Section, Name: string; Value: Extended);
    procedure WriteHex(const Section, Name: string; Value: Cardinal);
    procedure WriteColor(const Section, Name: string; Value: TColor);
    procedure WriteSize(const Section, Name: string; const aSize: TSize);
    procedure WriteStringList(const Section, Name: string; Value: TStrings; const Limit: integer = -1);
    procedure WriteObject(const Section, Name: string; const Buffer; const Size: Integer);
    procedure WriteMemoryStream(const Section, Name: string; MemoryStream: TMemoryStream);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadExtended(const Section, Name: string; Default: Extended): Extended;
    function ReadHex(const Section, Name: string; Default: Cardinal): Cardinal;
    function ReadColor(const Section, Name: string; Default: TColor): TColor;
    function ReadSize(const Section, Name: string; var aSize: TSize): Boolean;
    function ReadStringList(const Section, Name: string; Value: TStrings): Boolean;
    function ReadObject(const Section, Name: string; var Buffer; const Size: Integer): Boolean;
    function ReadMemoryStream(const Section, Name: string; MemoryStream: TMemoryStream): Boolean;
    property ReadOnly: Boolean read fReadOnly;
  end;

implementation

uses
  RtlConsts, Math, uFileUtils;

var
  FS: TFormatSettings;


function BinToText(const Buffer; const Size: Integer): string;
var
  p: PByte;
  i: Integer;
begin
  Result:='';
  p:=@Buffer;
  for i:=1 to Size do begin
    Result:=Result+IntToHex(p^,2);
    Inc(p);
  end;
end;

function TextToBin(Text: String; var Buffer; const Size: Integer): Boolean;

  function ExtractByte(var B: Byte): Boolean;
  var
    Code: Integer;
  begin
    Val('$'+Copy(Text,1,2),B,Code);
    Result:=Code=0;
    if Result then
      Delete(Text,1,2);
  end;

var
  P: PByte;
begin
  Result:=Length(Text)=Size*2;
  if not Result then
    Exit;
  P:=@Buffer;
  while Text<>'' do begin
    if not ExtractByte(P^) then begin
      Result:=false;
      Exit;
    end;
    Inc(P);
  end;
end;


{ TFileStreamEx }

constructor TFileStreamEx.Create(const aFileName: string; Mode: LongWord);
begin
  if (Mode and fmCreate)<>0 then begin
    fHandle:=ZFileCreate(aFileName,Mode);
    if fHandle=feInvalidHandle then
      raise EFCreateError.CreateFmt(SFCreateError,[aFileName])
    else
      inherited Create(fHandle);
  end else begin
    fHandle:=ZFileOpen(aFileName, Mode);
    if fHandle=feInvalidHandle then
      raise EFOpenError.CreateFmt(SFOpenError,[aFilename])
    else
      inherited Create(FHandle);
  end;
  fFileName:=aFileName;
end;

destructor TFileStreamEx.Destroy;
begin
  inherited Destroy;
  // Close handle after destroying the base object, because it may use Handle in Destroy.
  if fHandle<>feInvalidHandle then
    FileClose(FHandle);
end;

function TFileStreamEx.Flush: Boolean;
begin
  Result:=ZFileFlush(fHandle);
end;

function TFileStreamEx.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result:=FileRead(fHandle,Buffer,Count);
  if Result=-1 then
    raise EReadError.Create(SysErrorMessage(GetLastOSError));
end;


{ TStringStreamEx }

procedure TStringStreamEx.Write(const aString: string);
begin
  Write(PChar(aString)[0],Length(AString));
end;

procedure TStringStreamEx.Write(const aFmt: string; const Args: array of const);
begin
  Write(Format(aFmt,Args));
end;

procedure TStringStreamEx.WriteLn;
begin
  Write(LineEnding);
end;

procedure TStringStreamEx.WriteLn(const aString: string);
begin
  Write(aString+LineEnding);
end;

procedure TStringStreamEx.WriteLn(const aFmt: string; const Args: array of const);
begin
  WriteLn(Format(aFmt,Args));
end;

{ TStringListEx }

function TStringListEx.IndexOfValue(const Value: string): Integer;
var
  i: Integer;
  s: string;
begin
  CheckSpecialChars;
  Result:=0;
  while (Result<Count) do begin
    s:=Strings[Result];
    i:=Pos(NameValueSeparator,s)+1;
    if (i>0) and (DoCompareText(Value,Copy(s,i,MaxInt))=0) then
      Exit;
    Inc(result);
    end;
  Result:=-1;
end;

procedure TStringListEx.LoadFromFile(const FileName: String);
var
  fs: TFileStreamEx;
begin
  fs:=TFileStreamEx.Create(FileName,fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TStringListEx.SaveToFile(const FileName: String);
var
  fs: TFileStreamEx = nil;
begin
  try
    if ZFileExists(FileName) then begin
      fs:=TFileStreamEx.Create(FileName,fmOpenWrite or fmShareDenyWrite);
      fs.Position:=0;
      fs.Size:=0;
    end
    else
      fs:=TFileStreamEx.Create(FileName,fmCreate);
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;


{ TFileReader }

constructor TFileReader.Create;
begin
  fCurr:=-1;
  fList:=TStringListEx.Create;
end;

destructor TFileReader.Destroy;
begin
  fList.Free;
  inherited;
end;

procedure TFileReader.Reset;
begin
  fCurr:=0;
end;

procedure TFileReader.Parse(const aFileName: string);
begin
  try
    fList.LoadFromFile(aFileName);
  except
    Exit;
  end;
  fFileName:=aFileName;
  Reset;
end;

procedure TFileReader.Parse(lines: TStrings; const aFileName: string);
begin
  fList.Assign(lines);
  fFileName:=aFileName;
  Reset;
end;


function TFileReader.Eof: Boolean;
begin
  Eof:=fCurr>=fList.Count;
end;

function TFileReader.ReadLine: string;
begin
  if Eof then
    Exit('');
  result:=Trim(fList[fCurr]);
  inc(fCurr);
end;

{ TIniFileEx }

constructor TIniFileEx.Create(const aFileName: string; Mode: Word);
var
  sl: TStringListEx;
begin
  fReadOnly:=((Mode and $03)=fmOpenRead);
  inherited Create(EmptyStr);
  if ((Mode and $03) <> fmOpenWrite) then begin
    if ZFileExists(aFileName) then begin
      sl:=TStringListEx.Create;
      try
        sl.LoadFromFile(aFileName);
        SetStrings(sl);
      finally
        sl.Free;
      end;
    end;
  end;
  Rename(aFileName,False);
end;

constructor TIniFileEx.Create(const aFileName: string;
  aEscapeLineFeeds: Boolean);
var
  Mode: Word;
begin
  if not ZFileExists(aFileName) then
    Mode:=fmOpenWrite or fmShareDenyWrite
  else if ZFileAccess(aFileName,fmOpenReadWrite or fmShareDenyWrite) then
    Mode:=fmOpenReadWrite or fmShareDenyWrite
  else begin
    Mode:=fmOpenRead or fmShareDenyNone;
  end;
  Create(aFileName,Mode);
end;

procedure TIniFileEx.UpdateFile;
var
  sl: TStringListEx;
begin
  if not fReadOnly then begin
    sl:=TStringListEx.Create;
    try
      GetStrings(sl);
      sl.SaveToFile(FileName);
      PBoolean(@Dirty)^:=false;
    finally
      sl.Free;
    end;
  end;
end;

function TIniFileEx.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  result:=StrToBoolDef(ReadString(Section,Ident,''),Default);
end;

function TIniFileEx.ReadFloat(const Section, Name: string;
  Default: Double): Double;
var
  FloatStr: string;
begin
  FloatStr:=ReadString(Section,Name,'');
  Result:=StrToFloatDef(FloatStr,Default,FS);
end;

function TIniFileEx.ReadExtended(const Section, Name: string;
  Default: Extended): Extended;
var
  FloatStr: string;
begin
  FloatStr:=ReadString(Section,Name,'');
  Result:=StrToFloatDef(FloatStr,Default,FS);
end;

function TIniFileEx.ReadHex(const Section, Name: string;
  Default: Cardinal): Cardinal;
var
  s: string;
  e: integer;
begin
  s:=Format('$%.4x',[Default]);
  s:=ReadString(Section, Name, s);
  Val(s,Result,e);
  if e<>0 then
    Result:=Default;
end;

function TIniFileEx.ReadColor(const Section, Name: string; Default: TColor
  ): TColor;
var
  s: string;
begin
  s:=ReadString(Section, Name, ColorToString(Default));
  Result:=StringToColor(s);
end;

function TIniFileEx.ReadSize(const Section, Name: string;
  var aSize: TSize): Boolean;
var
  s: string;
  L: TStringList;
begin
  result:=false;
  s:=Format('%d, %d',[aSize.cx, aSize.cy]);
  s:=ReadString(Section, Name, s);
  L := TStringList.Create;
  try
    L.CommaText := s;
    if L.Count = 2 then begin
      aSize.cx := StrToIntDef(L[0], aSize.cx);
      aSize.cy := StrToIntDef(L[1], aSize.cy);
      Result := True;
    end;
  finally
    L.Free;
  end;
end;

procedure TIniFileEx.WriteBool(const Section, Ident: string; Value: Boolean);
const
  Values: array[Boolean] of string = ('False','True');
begin
  WriteString(Section, Ident, Values[Value]);
end;

procedure TIniFileEx.WriteFloat(const Section, Name: string; Value: Double);
begin
  WriteString(Section,Name,FloatToStr(Value,FS));
end;

procedure TIniFileEx.WriteExtended(const Section, Name: string; Value: Extended);
begin
  WriteString(Section,Name,FloatToStrF(Value,ffgeneral,18,22,FS));
end;

procedure TIniFileEx.WriteHex(const Section, Name: string; Value: Cardinal);
begin
  WriteString(Section,Name,Format('$%.8x',[Value]));
end;

procedure TIniFileEx.WriteColor(const Section, Name: string; Value: TColor);
begin
  WriteString(Section,Name,ColorToString(Value));
end;

procedure TIniFileEx.WriteSize(const Section, Name: string; const aSize: TSize);
begin
  WriteString(Section, Name, Format('%d, %d',[aSize.cx, aSize.cy]));
end;

procedure TIniFileEx.WriteStringList(const Section, Name: string;
  Value: TStrings; const Limit: integer);
var
  i,m: integer;
  s: string;
begin
  s:='';
  if Limit<0 then
    m:=Value.Count
  else
    m:=Min(Value.Count,Limit);

  for i:=0 to m-1 do
    s:=s+'"'+Value[i]+'", ';

  s:=TrimRight(s);
  Delete(s,Length(s),1);
  WriteString(Section, Name, s);
end;

procedure TIniFileEx.WriteObject(const Section, Name: string; const Buffer;
  const Size: Integer);
begin
  WriteString(Section,Name,BinToText(Buffer,Size));
end;

procedure TIniFileEx.WriteMemoryStream(const Section, Name: string;
  MemoryStream: TMemoryStream);
begin
  WriteString(Section,Name,BinToText(MemoryStream.Memory^,MemoryStream.Size));
end;

function TIniFileEx.ReadStringList(const Section, Name: string;
  Value: TStrings): Boolean;
var
  s: string;
begin
  s:=ReadString(Section,Name,'');
  result:=s<>'';
  if not result then
    Exit;
  Value.BeginUpdate;
  try
    Value.Clear;
    Value.CommaText:=s;
  finally
    Value.EndUpdate;
  end;
end;

function TIniFileEx.ReadObject(const Section, Name: string; var Buffer;
  const Size: Integer): Boolean;
var
  s: string;
begin
  s:=ReadString(Section,Name,'');
  result:=TextToBin(s,Buffer,Size);
end;

function TIniFileEx.ReadMemoryStream(const Section, Name: string;
  MemoryStream: TMemoryStream): Boolean;
var
  s: string;
begin
  result:=false;
  s:=ReadString(Section,Name,'');
  if s='' then
    exit;
  MemoryStream.SetSize(Length(s) div 2);
  result:=TextToBin(s,MemoryStream.Memory^,MemoryStream.Size);
  if result then
    MemoryStream.Position:=0;
end;

initialization
  FS:=DefaultFormatSettings;
  FS.DecimalSeparator:='.';

end.


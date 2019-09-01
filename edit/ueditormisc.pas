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

unit uEditorMisc;

{$mode objfpc}{$H+}

{$ifdef FPC_OS_UNICODE}
  {$define UNICODE}
{$endif}

{$IfDef Windows}
  {$Define UseShellFunc}
{$EndIf}


interface

uses
  {$IfDef UseShellFunc} Windows,{$endif} Classes, Controls, SysUtils, Graphics,
  FileCtrl, Forms, Dialogs, StdCtrls, ExtCtrls, ImgList, SynEdit, Menus;

type

  TTextLevel = (tlSelection, tlLines, tlFullText);
  TTextOperationLevel = set of TTextLevel;
  TTextOperation = function(const Param: string): string;

const

  DefaultOperationLevel = [tlSelection, tlLines];


procedure DrawDisabledImagelist(src, dst: TCustomImageList);
procedure DrawThemedControl(pnl: TPanel);
procedure DrawMainToolbar(pnl: TPanel);
procedure SortMenu(MenuItem: TMenuItem);
procedure CentredLabels(aControl: TWinControl);
procedure ShowFileLocation(const s: string);
function AllowValueChar(const c: char): boolean;
function ParseValue(const s: string): extended;
function Sf(const n: double): string;
function ExtendedToStr(const x: extended): string;
function GetControlHint(Control: TControl): string;
function TabsToSpace(const s: string): string;
function RemoveMultipleSpaces(const s: string): string;
function TitleCase(const s: string): string;
function SentenceCase(const s: string): string;
function ToggleCase(const s: string): string;
function JoinText(const s: string): string;
function WrapToList(const SLine: string; MaxCol: integer;
  const BreakStr: string = sLineBreak;
  const BreakChars: TSysCharSet = [' ', '-', #9, '.', ',', ';'];
  const SpaceChars: TSysCharSet = [' ', #9]): string;


implementation

uses
  Types, Themes, LazUTF8, LCLType,
  LCLIntf, character, FPImage, IntfGraphics, GraphType, uFrmMain;

type

  TControlAccess = class(TControl);
  TControlActionLinkAccess = class(TControlActionLink);

var
  FS: TFormatSettings;


procedure DrawDisabledImagelist(src, dst: TCustomImageList);
var
  i, x, y: integer;
  b: TBitmap;
  m: TLazIntfImage;
  fd: TRawImageDescription;
  c: TFPColor;
  ih, mh: HBitmap;
begin
  dst.Clear;
  b := TBitmap.Create;
  m := TLazIntfImage.Create(0, 0);
  try
    b.Width := src.Width;
    b.Height := src.Height;
    fd.Init_BPP32_B8G8R8_BIO_TTB(b.Width, b.Height);
    m.DataDescription := fd;
    for i := 0 to src.Count - 1 do
    begin
      src.GetBitmap(i, b);
      m.LoadFromBitmap(b.Handle, b.MaskHandle);
      for y := 0 to m.Height - 1 do
        for x := 0 to m.Width - 1 do
        begin
          c := m.Colors[x, y];
          c.alpha := c.alpha div 3;
          m.Colors[x, y] := c;
        end;
      m.CreateBitmaps(ih, mh, False);
      b.Handle := ih;
      b.MaskHandle := mh;
      dst.Add(b, nil);
    end;
  finally
    b.Free;
    m.Free;
  end;
end;

{$IfDef UseShellFunc}

const
  OFASI_EDIT = $0001;
  OFASI_OPENDESKTOP = $0002;

  shell32 = 'shell32.dll';

  {$IFDEF UNICODE}
  function ILCreateFromPath(pszPath: PChar): PItemIDList stdcall; external shell32
    Name 'ILCreateFromPathW';
  {$ELSE}
  function ILCreateFromPath(pszPath: PChar): PItemIDList stdcall; external shell32
    Name 'ILCreateFromPathA';
  {$ENDIF}
  procedure ILFree(pidl: PItemIDList) stdcall; external shell32;
  function SHOpenFolderAndSelectItems(pidlFolder: PItemIDList;
    cidl: cardinal; apidl: pointer; dwFlags: DWORD): HRESULT; stdcall; external shell32;

{$EndIf}

procedure ShowFileLocation(const s: string);
{$IfDef UseShellFunc}
var
  IIDL: PItemIDList;
{$EndIf}
begin
  {$IfDef Windows}
    {$IfDef UseShellFunc}
    IIDL := ILCreateFromPath(PChar(s));
    if IIDL <> nil then
      try
        SHOpenFolderAndSelectItems(IIDL, 0, nil, 0);
      finally
        ILFree(IIDL);
      end;
    {$Else}
    with FrmMain.apRuner do
    begin
      Executable := 'explorer.exe';
      Parameters.Clear;
      Parameters.Add('/e,');
      Parameters.Add('/select,');
      Parameters.Add(AnsiQuotedStr(s, '"'));
      Execute;
    end;
    {$EndIf}
  {$Else}
  OpenURL(ExtractFileDir(s));
  {$EndIf}
end;

function AllowValueChar(const c: char): boolean;
const
  CHRS = ['0'..'9', '+', '-', '.', 'E', 'e', #8, ^A, ^C, ^V, ^X];
begin
  Result := CharInSet(c, CHRS);
end;

function ParseValue(const s: string): extended;
var
  e: integer;
begin
  if Length(s) = 0 then
    Exit(0);
  Val(s, Result, e);
  if e <> 0 then
    Exit(0);
end;

function ExtendedToStr(const x: extended): string;
begin
  Result := FloatToStrF(x, ffgeneral, 18, 22, FS);
end;

function Sf(const n: double): string;
var
  p, d: integer;
begin
  Str(n: 14, Result);
  // delete leading spaces
  while Result[1] = ' ' do
    Delete(Result, 1, 1);
  // make two digit exponent and replace "E" to "e"
  p := Pos('E', Result);
  if p <> 0 then
  begin
    Result[p] := 'e';
    Inc(p, 2);
    d := Length(Result) - p - 1;
    if d < 0 then
      Insert(Copy('00', 1, -d), Result, p)
    else
      while (d > 0) and (Result[p] = '0') do
      begin
        Delete(Result, p, 1);
        if p > Length(Result) then
        begin
          Delete(Result, p - 2, 2);
          break;
        end;
        Dec(d);
      end;
  end;
end;

function GetControlHint(Control: TControl): string;
begin
  Result := '';
  while (Control <> nil) and (Result = '') do
  begin
    Result := GetShortHint(Control.Hint);
    if Assigned(TControlAccess(Control).ActionLink) then
      TControlActionLinkAccess(TControlAccess(Control).ActionLink).DoShowHint(Result);
    Control := Control.Parent;
  end;
end;

procedure DrawThemedControl(pnl: TPanel);
var
  r: TRect;
  bg: TThemedElementDetails;
begin
  r := pnl.ClientRect;
  Dec(r.Left); // remove left border
  bg := ThemeServices.GetElementDetails(trRebarRoot);
  ThemeServices.DrawElement(pnl.Canvas.Handle, bg, r);
end;

procedure DrawMainToolbar(pnl: TPanel);
var
  r: TRect;
begin
  if not ThemeServices.ThemesEnabled then
  begin
    r := pnl.ClientRect;
    pnl.Canvas.Pen.Color := clBtnShadow;
    pnl.Canvas.Line(0, 0, r.Right - 1, 0);
    pnl.Canvas.Line(0, r.Bottom - 2, r.Right - 1, r.Bottom - 2);
    pnl.Canvas.Pen.Color := clBtnHighlight;
    pnl.Canvas.Line(0, 1, r.Right - 1, 1);
    pnl.Canvas.Line(0, r.Bottom - 1, r.Right - 1, r.Bottom - 1);
  end;
end;

procedure SortMenu(MenuItem: TMenuItem);

  procedure SotrMenuItem(mi: TMenuItem);
  var
    i: integer;
    sl: TStringList;
  begin
    if mi.Count = 0 then
      exit;
    sl := TStringList.Create;
    try
      sl.Sorted := True;
      for i := 0 to mi.Count - 1 do
        sl.AddObject(mi[i].Caption, mi[i]);
      sl.Sort;
      for i := 0 to sl.Count - 1 do
        TMenuItem(sl.Objects[i]).MenuIndex := i;
    finally
      sl.Free;
    end;
    for i := 0 to mi.Count - 1 do
    begin
      if mi.Items[i].Count = 0 then
        continue;
      SotrMenuItem(mi.Items[i]);
    end;
  end;

begin
  SotrMenuItem(MenuItem);
end;

procedure CentredLabels(aControl: TWinControl);

  procedure Task(a: TWinControl);
  var
    i: integer;
    b: TLabel;
  begin
    for i := 0 to a.ControlCount - 1 do
    begin
      if a.Controls[i] is TWinControl then
        Task(a.Controls[i] as TWinControl)
      else
      begin
        if a.Controls[i] is TLabel then
        begin
          b := TLabel(a.Controls[i]);
          if Assigned(b.FocusControl) then
          begin
            b.AutoSize := False;
            b.Layout := tlCenter;
            b.AnchorVerticalCenterTo(b.FocusControl);
          end;
        end;
      end;
    end;
  end;

begin
  Task(aControl);
end;

function TabsToSpace(const s: string): string;
const
  NUMBEROFSPACEFORTAB = 2;
begin
  Result := StringReplace(s, #9, StringOfChar(#32, NUMBEROFSPACEFORTAB), [rfReplaceAll]);
end;

function RemoveMultipleSpaces(const s: string): string;
var
  p: integer;
begin
  Result := Trim(s);
  p := Pos(#32#32, Result);
  while p > 0 do
  begin
    Result := StringReplace(Result, #32#32, #32, [rfReplaceAll]);
    p := Pos(#32#32, Result);
  end;
end;

function JoinText(const s: string): string;
begin
  Result := StringReplace(AdjustLineBreaks(s), LineEnding, ' ', [rfReplaceAll]);
end;

function ToggleCase(const s: string): string;
begin
  Result := UTF8SwapCase(s);
end;

function TitleCase(const s: string): string;
var
  i: integer;
  t, t2: UnicodeString;
begin
  t2 := UTF8ToUTF16(s);
  for i := 2 to Length(t2) do
  begin
    if not IsLetter(t2, i - 1) then
      t2[i] := UpCase(t2[i])
    else
    begin
      t := LowerCase(t2[i]);
      t2[i] := t[1];
    end;
  end;
  Result := UTF16ToUTF8(t2);
end;

function SentenceCase(const s: string): string;
var
  i: integer;
  t, t2: UnicodeString;
  letter: UnicodeChar;
  cap: boolean;
begin
  t := '';
  cap := True;
  t2 := UTF8ToUTF16(UTF8LowerString(s));
  for i := 1 to Length(t2) do
  begin
    letter := t2[i];
    if CharInSet(letter, ['.', '!', '?']) then
      cap := True
    else
    if (not IsWhiteSpace(letter)) and cap then
    begin
      letter := UpCase(letter);
      cap := False;
    end;
    t += letter;
  end;
  Result := UTF16ToUTF8(t);
end;

function WrapToList(const SLine: string; MaxCol: integer;
  const BreakStr: string = sLineBreak;
  const BreakChars: TSysCharSet = [' ', '-', #9, '.', ',', ';'];
  const SpaceChars: TSysCharSet = [' ', #9]): string;
var
  i, nFullLength, nStartPos, nFind: integer;
  bFindNonBreak: boolean;
begin
  nFullLength := Length(SLine);
  if nFullLength <= MaxCol then
  begin
    Result := SLine;
    Exit;
  end;

  Result := '';
  if MaxCol <= 0 then
    Exit;

  nStartPos := 1;
  repeat
    if SLine[nStartPos + MaxCol - 1] in BreakChars then
    begin
      Result := Result + Copy(SLine, nStartPos, MaxCol);
      nStartPos := nStartPos + MaxCol;
    end
    else
    begin
      nFind := -1;
      for i := nStartPos + MaxCol - 1 downto nStartPos do
        if CharInSet(SLine[i], BreakChars) then
        begin
          nFind := i;
          Break;
        end;

      if nFind <> -1 then
      begin
        bFindNonBreak := False;
        for i := nFind downto nStartPos do
          if not CharInSet(SLine[i], BreakChars) then
          begin
            bFindNonBreak := True;
            Break;
          end;
        if not bFindNonBreak then
          nFind := -1;
      end;

      if nFind = -1 then
        nFind := nStartPos + MaxCol - 1;

      Result := Result + Copy(SLine, nStartPos, nFind - nStartPos + 1);
      nStartPos := nFind + 1;
    end;

    if nFullLength >= nStartPos then
    begin
      if SpaceChars <> [] then
      begin
        while (nFullLength >= nStartPos) and CharInSet(SLine[nStartPos], SpaceChars) do
          Inc(nStartPos);

        if nFullLength < nStartPos then
          Exit;
      end;

      Result := Result + BreakStr;
      if nFullLength - nStartPos + 1 <= MaxCol then
      begin
        Result := Result + Copy(SLine, nStartPos, MaxInt);
        Exit;
      end;
    end;
  until nFullLength < nStartPos;
end;

initialization
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

end.

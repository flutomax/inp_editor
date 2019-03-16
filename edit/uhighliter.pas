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

unit uHighliter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter,
  SynEditHighlighterFoldBase, uDerivedClasses;

const
  SYNS_AttrIncludeFName = 'Include Filename';

type
  TtkTokenKind = (tkNull, tkComment, tkIdentifier, tkText, tkDirect, tkKeyWord,
    tkInclude, tkIncludeFN, tkNumber, tkComma, tkSpace, tkSymbol, tkIllegal,
    tkUnknown);

  TProcTableProc = procedure of object;

  { TInpHighlighter }

  TInpHighlighter = class(TSynCustomFoldHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fOldLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fFoldBlock: Boolean;
    fParams: Boolean;
    fIsInclude: Boolean;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fEnabledCodeFolding: Boolean;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fDirectAttri: TSynHighlighterAttributes;
    fIncludeAttri: TSynHighlighterAttributes;
    fIncludeFNAttri: TSynHighlighterAttributes;
    fKeyWordAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fIllegalAttri: TSynHighlighterAttributes;
    procedure KeyProc;
    procedure CRProc;
    procedure CommaProc;
    procedure EqualProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SignProc;
    procedure SpaceProc;
    procedure MakeMethodTables;
    procedure SetEnabledCodeFolding(AValue: Boolean);
    procedure ClearAttribute(attr: TSynHighlighterAttributes);
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: string; LineNumber:Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure LoadFromIni(Ini: TIniFileEx; const aSection: string);
    procedure SaveToIni(Ini: TIniFileEx; const aSection: string);
    property IsInclude: Boolean read fIsInclude;
  published
    property EnabledCodeFolding: Boolean read fEnabledCodeFolding
      write SetEnabledCodeFolding default true;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property KeyAttri: TSynHighlighterAttributes read fDirectAttri
      write fDirectAttri;
    property KeyWordAttri: TSynHighlighterAttributes read fKeyWordAttri
      write fKeyWordAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property IllegalAttri: TSynHighlighterAttributes read fIllegalAttri
      write fIllegalAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property IncludeAttri: TSynHighlighterAttributes read fIncludeAttri
      write fIncludeFNAttri;
    property IncludeFNAttri: TSynHighlighterAttributes read fIncludeFNAttri
      write fIncludeFNAttri;
  end;

var

  KeywordsList: TStringList;

implementation

{$R keywords.res}

uses
  SynEditStrConst;

const
  // add this keywords for start of the code fold block
  FOLDED_TOKENS: array[0..6] of string =
    ('*NODE','*ELEMENT','*NSET','*ELSET','*EQUATION','*SURFACE','*BOUNDARY');




procedure InitModule;
var
  i: Integer;
  rs: TResourceStream;
begin
  KeywordsList:=TStringList.Create;
  rs:=TResourceStream.Create(hInstance,'KEYWORDS','TEXT');
  try
    rs.Position:=0;
    KeywordsList.LoadFromStream(rs);
  finally
    rs.Free;
  end;
  KeywordsList.Sorted := true;
  RegisterPlaceableHighlighter(TInpHighlighter);
end;

procedure FinishModule;
begin
  FreeAndNil(KeywordsList);
end;

constructor TInpHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFoldBlock := false;
  fParams := false;
  fIsInclude := false;
  fOldLineNumber := -1;
  fEnabledCodeFolding := true;

  fCommentAttri               := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style         := [fsItalic];
  fCommentAttri.Foreground    := clGreen;
  AddAttribute(fCommentAttri);

  fIdentifierAttri            := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := clTeal;
  AddAttribute(fIdentifierAttri);

  fTextAttri                  := TSynHighlighterAttributes.Create(SYNS_AttrText);
  fTextAttri.Foreground       := clBlack;
  AddAttribute(fTextAttri);

  fDirectAttri                := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword);
  fDirectAttri.Foreground     := clNavy;
  fDirectAttri.Style          := [fsBold];
  AddAttribute(fDirectAttri);

  fIncludeAttri               := TSynHighlighterAttributes.Create(SYNS_AttrInclude);
  fIncludeAttri.Foreground    := clGreen;
  fIncludeAttri.Style         := [fsBold];
  AddAttribute(fIncludeAttri);

  fIncludeFNAttri             := TSynHighlighterAttributes.Create(SYNS_AttrIncludeFName);
  fIncludeFNAttri.Foreground  := clBlue;
  AddAttribute(fIncludeFNAttri);

  fKeyWordAttri               := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyWordAttri.Foreground    := clBlack;
  fKeyWordAttri.Style         := [fsBold];
  ClearAttribute(fKeyWordAttri);
  AddAttribute(fKeyWordAttri);

  fNumberAttri                := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground     := $00A00000;
  AddAttribute(fNumberAttri);

  fSpaceAttri                 := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri                := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground     := clRed;
  AddAttribute(fSymbolAttri);

  fIllegalAttri               := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar);
  fIllegalAttri.Foreground    := clRed;
  AddAttribute(fIllegalAttri);


  SetAttributesOnChange(@DefHighlightChange);

  fDefaultFilter      := 'CCX Input Files (*.inp)|*.inp';
  MakeMethodTables;
  Enabled:=true;

  // join '.' in filename that is defined as whole word
  WordBreakChars:=WordBreakChars-['.','/','\','+','-'];
end;

function TInpHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result:=fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result:=fIdentifierAttri;
    SYN_ATTR_VARIABLE: Result:=fTextAttri;
    SYN_ATTR_KEYWORD: Result:=fKeyWordAttri;
    SYN_ATTR_WHITESPACE: Result:=fSpaceAttri;
    SYN_ATTR_NUMBER: Result:=fNumberAttri;
  else
    Result := nil;
  end;
end;


procedure TInpHighlighter.ClearAttribute(attr: TSynHighlighterAttributes);
begin
  attr.FrameColor:=clNone;
  attr.FrameEdges:=sfeNone;
end;

procedure TInpHighlighter.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0      : fProcTable[i] := @NullProc;
      #10 {LF}: fProcTable[i] := @LFProc;
      #13 {CR}: fProcTable[i] := @CRProc;
      '0'..'9','-': fProcTable[i] := @NumberProc;
      #42 {*} : fProcTable[i] := @SignProc;
      #44 {,} : fProcTable[i] := @CommaProc;
      #61 {=} : fProcTable[i] := @EqualProc;
      #1..#9, #11, #12, #14..#32: fProcTable[i] := @SpaceProc;
    else
      fProcTable[i] := @TextProc;
    end;
end;

procedure TInpHighlighter.SetLine(const NewValue: string; LineNumber:Integer);
begin
  inherited;
  fParams := false;
  fIsInclude := false;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TInpHighlighter.CRProc;
begin
  fTokenID := tkSpace;
  case fLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TInpHighlighter.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TInpHighlighter.SetEnabledCodeFolding(AValue: Boolean);
begin
  if fEnabledCodeFolding=AValue then Exit;
  fEnabledCodeFolding:=AValue;
  ScanAllRanges;
end;

procedure TInpHighlighter.KeyProc;
begin
  fTokenID := tkDirect;
  inc(Run);
  while fLine[Run] <> #0 do
    case fLine[Run] of
      '=': break;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TInpHighlighter.TextProc;
var
  illegal: boolean;
begin
  illegal:=false;
  inc(Run);
  while (fLine[Run] in [#128..#191]) do begin
    illegal:=true;
    inc(Run);
  end;
  if illegal then begin
    fTokenID := tkIllegal;
    exit;
  end;
  while ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @TextProc)) do
    inc(Run);
  if fParams and (fTokenID = tkComma) then
    fTokenID := tkIdentifier
  else begin
    if fIsInclude then
      fTokenID := tkIncludeFN
    else
      fTokenID := tkText;
  end;
end;

procedure TInpHighlighter.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TInpHighlighter.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TInpHighlighter.NumberProc;
begin
  if (fTokenID in [tkText,tkIncludeFN]) then begin
    while fLine[Run] in ['0'..'9', '.', 'e', 'E', '-', '+'] do begin
      Inc(Run);
    end;
    Exit;
  end;
  Inc(Run);
  fTokenID := tkNumber;
  while fLine[Run] in ['0'..'9', '.', 'e', 'E', '-', '+'] do
  begin
    case fLine[Run] of
      '.':
        if fLine[Run + 1] = '.' then
          Break
        else
          fTokenID := tkNumber;
      'e', 'E': fTokenID := tkNumber;
      '-', '+':
        begin
          if fTokenID <> tkNumber then // arithmetic
            Break;
          if not (fLine[Run - 1] in ['e', 'E']) then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;
end;

procedure TInpHighlighter.SignProc;
var
  aToken: string;
  n,x: integer;
begin
  if fLine[Run+1]='*' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else begin
    aToken:='';
    x:=Run;
    while not (fLine[x] in [#0, #10, #13, ',']) do begin      // #32,
      aToken+=fLine[x];
      inc(x);
    end;
    if UpperCase(aToken)='*INCLUDE' then begin
      fTokenID := tkInclude;
      fIsInclude := true;
      x:=Length(aToken);
      Inc(Run,x);
      exit;
    end;
    if KeywordsList.Find(TrimRight(UpperCase(aToken)),n) then begin
      fTokenID := tkKeyWord;
      x:=Length(aToken);
      Inc(Run,x);
      exit;
    end
    else
      fTokenID := tkDirect;
    inc(Run);
    while fLine[Run] <> #0 do
    case fLine[Run] of
         #1..#48: break;
         else inc(Run);
    end;
  end;
end;

procedure TInpHighlighter.CommaProc;
begin
  if fTokenID in [tkDirect..tkInclude] then
    fParams := true;
  inc(Run);
  fTokenID := tkComma;
  while fLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

procedure TInpHighlighter.SpaceProc;
begin

  if fTokenID = tkIdentifier then begin
    // Identifier can have space
    while not (fLine[Run] in [#0, #10, #13, ',', '=']) do
      inc(Run);
    exit;
  end;

  inc(Run);

  fTokenID := tkSpace;
  while fLine[Run] in [#1..#9, #11, #12, #14..#32] do
    inc(Run);
end;

procedure TInpHighlighter.Next;
var
  s: string;
  i, n, Len: Integer;
  found, empty: Boolean;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];

  if not fEnabledCodeFolding then
    Exit;
  // at first we will close the code fold block if it is open
  if fFoldBlock and (fLineNumber<>fOldLineNumber) then
    if fLineNumber<CurrentLines.Count-1 then begin
      n := fLineNumber+1;
      found := false;
      empty := false;
      repeat
        s := Trim(CurrentLines[n]);
        empty := Length(s)=0;
        if empty then begin
          Inc(n);
          if n>=CurrentLines.Count then
            break;
          continue;
        end;
        found := (Length(s)>1) and (s[1]='*') and (s[2]<>'*');
      until not empty;
      if found then begin
        fFoldBlock := false;
        EndCodeFoldBlock;
      end;
      fOldLineNumber := fLineNumber;
    end;

  // we will clarify whether there are in an array FOLDED_TOKENS
  // keywords for start of the code fold block
  Len := Run - fTokenPos;
  SetString(s, (fLine + fTokenPos), Len);
  if fTokenId=tkKeyWord then
    for i := Low(FOLDED_TOKENS) to High(FOLDED_TOKENS) do
      if AnsiSameText(s,FOLDED_TOKENS[i]) then begin
        StartCodeFoldBlock(nil);
        fFoldBlock := true;
        break;
      end;
end;

function TInpHighlighter.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TInpHighlighter.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (fLine + fTokenPos), Len);
end;

procedure TInpHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := fLine + fTokenPos;
end;

function TInpHighlighter.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TInpHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkText   : Result := fTextAttri;
    tkDirect : Result := fDirectAttri;
    tkKeyWord: Result := fKeyWordAttri;
    tkInclude: Result := fIncludeAttri;
    tkIncludeFN: Result := fIncludeFNAttri;
    tkNumber : Result := fNumberAttri;
    tkComma,
    tkSpace  : Result := fSpaceAttri;
    tkSymbol : Result := fSymbolAttri;
    tkIllegal: Result := fIllegalAttri;
    tkUnknown: Result := fTextAttri;
    else Result := nil;
  end;
end;

function TInpHighlighter.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TInpHighlighter.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;

function TInpHighlighter.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TInpHighlighter.GetLanguageName: string;
begin
  Result := 'Abaqus/CalculiX';
end;

function TInpHighlighter.GetSampleSource: string;
begin
  Result := 'Test';
end;

procedure TInpHighlighter.SaveToIni(Ini: TIniFileEx; const aSection: string);
var
  i: Integer;
  a: TSynHighlighterAttributes;
  s: string;
begin
  ini.WriteBool(aSection,'Enabled',Enabled);
  for i:=0 to AttrCount-1 do begin
    a:=Attribute[i];
    s:=Format('Attribute_%s_',[StringReplace(a.StoredName,' ','_',[rfReplaceAll])]);
    Ini.WriteHex(aSection,s+'Background', a.Background);
    Ini.WriteHex(aSection,s+'Foreground', a.Foreground);
    Ini.WriteInteger(aSection,s+'Style', a.IntegerStyle);
    Ini.WriteInteger(aSection,s+'StyleMask', a.IntegerStyleMask);
  end;
end;


procedure TInpHighlighter.LoadFromIni(Ini: TIniFileEx; const aSection: string);
var
  i: Integer;
  a: TSynHighlighterAttributes;
  s: string;
begin
  Enabled:=ini.ReadBool(aSection,'Enabled',Enabled);
  for i:=0 to AttrCount-1 do begin
    a:=Attribute[i];
    s:=Format('Attribute_%s_',[StringReplace(a.StoredName,' ','_',[rfReplaceAll])]);
    a.Background:=ini.ReadHex(aSection,s+'Background', a.Background);
    a.Foreground:=Ini.ReadHex(aSection,s+'Foreground', a.Foreground);
    a.IntegerStyle:=Ini.ReadInteger(aSection,s+'Style', a.IntegerStyle);
    a.IntegerStyleMask:=Ini.ReadInteger(aSection,s+'StyleMask', a.IntegerStyleMask);
  end;
end;

initialization
  InitModule;

finalization
  FinishModule;



end.

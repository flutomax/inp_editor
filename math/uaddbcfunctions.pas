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

// Note:
// Add Faces -> CGX comp <name> do
// Export Faces -> CGX send <set name> abq sur
// Add Pressure To Body -> CGX send <set name> abq pres <value>
// Add Convection -> CGX send <set name> abq film <Temperature> <film koeff>

unit uAddBCFunctions;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, FileUtil, Controls, Dialogs, uInpTypes,
  uInpTranslator, uInpEditor;

type

  EAddBC = class(Exception);

  TAddBCCmd = (bcAddFaces, bcAddPressure, bcAddConvection, bcAddFluxOnFace,
    bcAddRadiation);

  { TAddBCProc }

  TAddBCProc = class
  private
    fInpFile: TInpFile;
    fEditor: TInpEditor;
    fCmd: TAddBCCmd;
    procedure CompleteSet(const sn: integer);
    function ExportData(const sn: integer; lst: TStrings;
      const aCmd: TAddBCCmd; const Args: array of const): boolean;
    function AddFaces(const sn: integer): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(aList: TStrings; const aCmd: TAddBCCmd);
    procedure Work(const sn: integer; const Args: array of const);
    property Editor: TInpEditor read fEditor write fEditor;
  end;

implementation

uses
  Math, uInpFunctions, uDerivedClasses, uFileUtils, uConsts, uDialogs,
  uEditorMisc;

const
  INCL_EXT: array[TAddBCCmd] of string = (
    '.sur', '.dlo', '.flm', '.dfl', '.rad'
    );

  CMD_DESCR: array[TAddBCCmd] of string = (
    'Surfaces', 'Pressure', 'Convection', 'DFlux', 'Radiate'
    );

  CMD_TITLE: array[TAddBCCmd] of string = (
    'SURFACE', 'DLOAD', 'FILM', 'DFLUX', 'RADIATE'
    );

function IFind(ipnt: TIntegerDynArray; const n, x0: integer): integer;
var
  i, ii, m, n1, n2: integer;
begin
  i := 0;
  if (n = 0) or (x0 < ipnt[0]) then
    Exit(-1)
  else if x0 > ipnt[n - 1] then
    // is no member of array
    Exit(-2)
  else
  begin
    // search the intersection, regula falsi
    n1 := 0;
    n2 := n;
    for ii := 0 to n - 1 do
    begin
      m := (n2 + n1) div 2;
      if x0 >= ipnt[m] then
        n1 := m;
      if x0 < ipnt[m] then
        n2 := m;
      if (n2 - n1) = 1 then
        break;
    end;
    i := n1;
    if x0 = ipnt[i] then
      // is member of array at pos i
      Exit(i);
  end;
  // is no member of array
  Exit(-3);
end;

function GetFNumFromCategory(const c: TElementCategory): integer;
begin
  case c of
    ecTria3: Result := 3;
    ecTria6: Result := 6;
    ecQuad4: Result := 4;
    ecQuad8: Result := 8;
    ecSeg2: Result := 2;
    ecSeg3: Result := 3;
    else
      Result := 0;
  end;
end;

function PromptNewFileName(var fn: string): boolean;
var
  i: integer;
begin
  fn := ZIncFileName(fn);
  Result := InputQueryEx(sNewFileName, sEnterNewFileName, fn);
end;

function ChechkFileName(var fn: string): boolean;
var
  s: string;
begin
  Result := True;
  if ZFileExists(fn) then
  begin
    Result := False;
    s := ExtractFileName(fn);
    case MessageDlg(sOverwriteConfirm, Format(sOverwriteMsg, [fn]),
        mtConfirmation, mbYesNoCancel, 0) of
      mrCancel: Exit;
      mrNo:
      begin
        Result := PromptNewFileName(s);
        if Result then
          fn := ExtractFilePath(fn) + s;
      end;
      mrYes: Result := True;
    end;
  end;
end;

{ TAddBCProc }


constructor TAddBCProc.Create;
begin
  fEditor := nil;
  fInpFile := TInpFile.Create;
end;

destructor TAddBCProc.Destroy;
begin
  FreeAndNil(fInpFile);
  inherited Destroy;
end;

procedure TAddBCProc.Load(aList: TStrings; const aCmd: TAddBCCmd);
var
  i: integer;
  t: TSet;
begin
  if fEditor = nil then
    raise EAddBC.Create('Not assigned editor');
  fInpFile.Parse(fEditor.Lines, fEditor.FileName);
  fCmd := aCmd;
  aList.BeginUpdate;
  try
    aList.Clear;
    for i := 0 to fInpFile.Summen.sets - 1 do
    begin
      t := fInpFile.Sets[i];
      if (t.NumNodes = 0) or AnsiSameText(t.Name, 'nall') then
        continue;
      case fCmd of
        bcAddFaces: if (t.NumElements > 0) or (t.NumFaces > 0) then
            continue;
      end;
      aList.AddObject(t.Name, TObject(PtrUint(i)));
    end;
  finally
    aList.EndUpdate;
  end;

end;

function TAddBCProc.ExportData(const sn: integer; lst: TStrings;
  const aCmd: TAddBCCmd; const Args: array of const): boolean;
const
  CV: array[boolean] of string = ('', 'CR');
var
  i, j, n, e, f: integer;
  cat: TElementCategory;
begin
  Result := False;
  with fInpFile.Sets[sn] do
    n := IfThen(aCmd = bcAddFaces, NumFaces, NumElfaces);
  for j := 0 to n - 1 do
  begin
    if aCmd = bcAddFaces then
    begin
      i := fInpFile.Sets[sn].Faces[j];
      e := fInpFile.Faces[i].ElemNumber;
      f := fInpFile.Faces[i].Number;
    end
    else
    begin
      i := j;
      e := fInpFile.Sets[sn].Elfaces[i].e;
      f := fInpFile.Sets[sn].Elfaces[i].f;
    end;

    cat := fInpFile.ElEnqire[e].Category;
    if cat in [ecTria3..ecQuad8, ecSeg2, ecSeg3] then
    begin
      if cat in [ecSeg2, ecSeg3] then
        case aCmd of
          bcAddFaces: lst.Add('%d, S?', [e]);
          bcAddPressure: lst.Add('%d, P?, %s', [e, Sf(Args[0].VExtended^)]);
          bcAddConvection: lst.Add('%d, F?, %.6f, %s',
              [e, Args[0].VExtended^, Sf(Args[1].VExtended^)]);
          bcAddFluxOnFace: lst.Add('%d, S?, %s', [e, Sf(Args[0].VExtended^)]);
          bcAddRadiation: lst.Add('%d, R?%s, %.6f, %s',
              [e, CV[Args[2].VBoolean], Args[0].VExtended^, Sf(Args[1].VExtended^)]);
        end
      else if fInpFile.ElEnqire[e].Attr > 3 then
      begin
        if fInpFile.Faces[i].Number = 1 then
          case aCmd of
            bcAddFaces: lst.Add('%d, SP ', [e]);
            bcAddPressure: lst.Add('%d, PP, %s', [e, Sf(Args[0].VExtended^)]);
            bcAddConvection: lst.Add('%d, FP, %.6f, %s',
                [e, Args[0].VExtended^, Sf(Args[1].VExtended^)]);
            bcAddFluxOnFace: lst.Add('%d, SP, %s', [e, Sf(Args[0].VExtended^)]);
            bcAddRadiation: lst.Add('%d, RP%s, %.6f, %s',
                [e, CV[Args[2].VBoolean], Args[0].VExtended^, Sf(Args[1].VExtended^)]);
          end
        else
          case aCmd of
            bcAddFaces: lst.Add('%d, S%d', [e, f - 1]);
            bcAddPressure: lst.Add('%d, P%d, %s', [e, f - 1, Sf(Args[0].VExtended^)]);
            bcAddConvection: lst.Add('%d, F%d, %.6f, %s',
                [e, f - 1, Args[0].VExtended^, Sf(Args[1].VExtended^)]);
            bcAddFluxOnFace: lst.Add('%d, S%d, %s', [e, f - 1, Sf(Args[0].VExtended^)]);
            bcAddRadiation: lst.Add('%d, R%d%s, %.6f, %s',
                [e, f - 1, CV[Args[2].VBoolean], Args[0].VExtended^,
                Sf(Args[1].VExtended^)]);
          end;
      end
      else
      begin
        if fInpFile.Faces[i].Number = 1 then
          case aCmd of
            bcAddFaces: lst.Add('%d, SPOS ', [e]);
            bcAddPressure: lst.Add('%d, PPOS, %s', [e, Sf(Args[0].VExtended^)]);
            bcAddConvection: lst.Add('%d, FP, %.6f, %s',
                [e, Args[0].VExtended^, Sf(Args[1].VExtended^)]);
            bcAddFluxOnFace: lst.Add('%d, SP, %s', [e, Sf(Args[0].VExtended^)]);
            bcAddRadiation: lst.Add('%d, RPOS%s, %.6f, %s',
                [e, CV[Args[2].VBoolean], Args[0].VExtended^, Sf(Args[1].VExtended^)]);
          end
        else
          case aCmd of
            bcAddFaces: lst.Add('%d, S%d', [e, f + 1]);
            bcAddPressure: lst.Add('%d, P%d, %s', [e, f + 1, Sf(Args[0].VExtended^)]);
            bcAddConvection: lst.Add('%d, F%d, %.6f, %s',
                [e, f + 1, Args[0].VExtended^, Sf(Args[1].VExtended^)]);
            bcAddFluxOnFace: lst.Add('%d, S%d, %s', [e, f + 1, Sf(Args[0].VExtended^)]);
            bcAddRadiation: lst.Add('%d, R%d%s, %.6f, %s',
                [e, f + 1, CV[Args[2].VBoolean], Args[0].VExtended^,
                Sf(Args[1].VExtended^)]);
          end;
      end;
    end
    else
      case aCmd of
        bcAddFaces: lst.Add('%d, S%d', [e, f + 1]);
        bcAddPressure: lst.Add('%d, P%d, %s', [e, f + 1, Sf(Args[0].VExtended^)]);
        bcAddConvection: lst.Add('%d, F%d, %.6f, %s',
            [e, f + 1, Args[0].VExtended^, Sf(Args[1].VExtended^)]);
        bcAddFluxOnFace: lst.Add('%d, S%d, %s', [e, f + 1, Sf(Args[0].VExtended^)]);
        bcAddRadiation: lst.Add('%d, R%d%s, %.6f, %s',
            [e, f + 1, CV[Args[2].VBoolean], Args[0].VExtended^,
            Sf(Args[1].VExtended^)]);
      end;
  end;
  Result := True;
end;

procedure TAddBCProc.CompleteSet(const sn: integer);
var
  i, j, k, n, m: integer;
begin
  m := fInpFile.Sets[sn].NumFaces;
  for i := 0 to fInpFile.Summen.f - 1 do
  begin
    n := GetFNumFromCategory(fInpFile.Faces[i].Category);
    k := 0;
    for j := 0 to n - 1 do
    begin
      with fInpFile.Sets[sn] do
        if IFind(Nodes, NumNodes, fInpFile.Faces[i].Node[j]) > -1 then
          Inc(k);
    end;
    if k = n then
    begin
      with fInpFile.Sets[sn] do
      begin
        SetLength(Faces, NumFaces + 1);
        Faces[NumFaces] := i;
        Inc(NumFaces);
      end;
    end;
  end; // for

  if fInpFile.Sets[sn].NumFaces - m <> 0 then
  begin
    QSort(@fInpFile.Sets[sn].Faces[0], fInpFile.Sets[sn].NumFaces,
      SizeOf(integer), @CompareInt);
    n := 0;
    for i := 1 to fInpFile.Sets[sn].NumFaces - 1 do
      if fInpFile.Sets[sn].Faces[n] <> fInpFile.Sets[sn].Faces[i] then
      begin
        Inc(n);
        fInpFile.Sets[sn].Faces[n] := fInpFile.Sets[sn].Faces[i];
      end;
    fInpFile.Sets[sn].NumFaces := n + 1;
  end;

  m := fInpFile.Sets[sn].NumFaces;
  for i := 0 to fInpFile.Summen.f - 1 do
  begin
    with fInpFile.Sets[sn] do
    begin
      if IFind(Elements, NumElements, fInpFile.Faces[i].ElemNumber) > -1 then
      begin
        SetLength(Faces, NumFaces + 1);
        Faces[NumFaces] := i;
        Inc(NumFaces);
      end;
    end;
  end;

  if fInpFile.Sets[sn].NumFaces - m <> 0 then
  begin
    QSort(@fInpFile.Sets[sn].Faces[0], fInpFile.Sets[sn].NumFaces,
      SizeOf(integer), @CompareInt);
    // erase multiple entities
    n := 0;
    for i := 1 to fInpFile.Sets[sn].NumFaces - 1 do
      if fInpFile.Sets[sn].Faces[n] <> fInpFile.Sets[sn].Faces[i] then
      begin
        Inc(n);
        fInpFile.Sets[sn].Faces[n] := fInpFile.Sets[sn].Faces[i];
      end;
    fInpFile.Sets[sn].NumFaces := n + 1;
  end;
end;

function TAddBCProc.AddFaces(const sn: integer): boolean;
var
  m: integer;

  procedure EraseMultipleEntities;
  var
    i, n: integer;
  begin
    if (fInpFile.Sets[sn].NumNodes - m) <> 0 then
    begin
      QSort(@fInpFile.Sets[sn].Nodes[0], fInpFile.Sets[sn].NumNodes,
        SizeOf(integer), @CompareInt);
      // erase multiple entities
      n := 0;
      for i := 1 to fInpFile.Sets[sn].NumNodes - 1 do
        if fInpFile.Sets[sn].Nodes[n] <> fInpFile.Sets[sn].Nodes[i] then
        begin
          Inc(n);
          fInpFile.Sets[sn].Nodes[n] := fInpFile.Sets[sn].Nodes[i];
        end;
      fInpFile.Sets[sn].NumNodes := n + 1;
    end;
  end;

var

  i, j, k, n: integer;
  lst: TStringListEx;
  s, fn, sb: string;
begin
  CompleteSet(sn);
  m := fInpFile.Sets[sn].NumNodes;
  for i := 0 to fInpFile.Sets[sn].NumFaces - 1 do
  begin
    k := fInpFile.Sets[sn].Faces[i];
    n := GetFNumFromCategory(fInpFile.Faces[k].Category);
    for j := 0 to n - 1 do
      with fInpFile.Sets[sn] do
      begin
        SetLength(Nodes, NumNodes + 1);
        Nodes[NumNodes] := fInpFile.Faces[Faces[i]].Node[j];
        Inc(NumNodes);
      end;
  end;
  EraseMultipleEntities;
  sb := Format('** Surfaces based on %s', [fInpFile.Sets[sn].Name]);
  lst := TStringListEx.Create;
  try
    lst.Add(sb);
    lst.Add('*SURFACE, NAME=%s', [fInpFile.Sets[sn].Name]);
    ExportData(sn, lst, bcAddFaces, []);
    fn := Format('%s%s%s', [ExtractFilePath(fEditor.FileName),
      ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name), INCL_EXT[bcAddFaces]]);
    if not ChechkFileName(fn) then
      exit;
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;
  s := Format('%s%s%s', [sb, fEditor.Lines.LineBreak, Format(
    sInclude, [ExtractFileName(fn)])]);
  fEditor.InsertLineHere(s);
  Result := True;
end;

procedure TAddBCProc.Work(const sn: integer; const Args: array of const);
var
  s, fn, sb: string;
  lst: TStringListEx;
begin
  if fCmd = bcAddFaces then
  begin
    AddFaces(sn);
    exit;
  end;
  // check NumFaces of group and add it if needed
  if fInpFile.Sets[sn].NumFaces = 0 then
  begin
    if not AddFaces(sn) then
      exit;
    fInpFile.Parse(fEditor.Lines, fEditor.FileName);
  end;
  // format include filename
  fn := Format('%s%s%s', [ExtractFilePath(fEditor.FileName),
    ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name), INCL_EXT[fCmd]]);
  if not ChechkFileName(fn) then
    exit;
  // format comment string on base file
  sb := Format('** %s based on %s', [CMD_DESCR[fCmd], fInpFile.Sets[sn].Name]);
  lst := TStringListEx.Create;
  try
    lst.Add(sb);
    ExportData(sn, lst, fCmd, Args);
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;
  // format comment string on include file
  s := '';
  case fCmd of
    bcAddPressure: s := Format('pressure=%s', [Sf(Args[0].VExtended^)]);
    bcAddConvection: s := Format('sink=%.6f; koef=%s', [Args[0].VExtended^,
        Sf(Args[1].VExtended^)]);
    bcAddFluxOnFace: s := Format('flux=%s', [Sf(Args[0].VExtended^)]);
    bcAddRadiation: s := Format('sink=%.6f; emissivity=%s; cavity=%s',
        [Args[0].VExtended^, Sf(Args[1].VExtended^), BoolToStr(Args[2].VBoolean, True)]);
  end;

  s := Format('** %s (%s) based on group %s%s', [CMD_DESCR[fCmd],
    s, fInpFile.Sets[sn].Name, fEditor.Lines.LineBreak]);
  s := Format('%s*%s%s', [s, CMD_TITLE[fCmd], fEditor.Lines.LineBreak]);
  s := Format('%s%s', [s, Format(sInclude, [ExtractFileName(fn)])]);
  fEditor.InsertLineHere(s);
end;

end.

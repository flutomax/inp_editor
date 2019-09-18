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
    procedure PrepareSet(const sn: integer);
    function ExportFaces(const sn: integer): boolean;
    function AddFaces(const sn: integer): boolean;
    function AddFlux(const sn: integer; const flux: double): boolean;
    function AddPressure(const sn: integer; const pressure: double): boolean;
    function AddConvertion(const sn: integer; const sink, koef: double): boolean;
    function AddRadiation(const sn: integer; const sink, emissivity: double;
      const cavity: boolean): boolean;
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

function TAddBCProc.ExportFaces(const sn: integer): boolean;
var
  lst: TStringListEx;
  s, fn, sb: string;
  i, j, e: integer;
  cat: TElementCategory;
begin
  Result := False;
  sb := Format('** Surfaces based on %s', [fInpFile.Sets[sn].Name]);
  lst := TStringListEx.Create;
  try
    lst.Add(sb);
    lst.Add('*SURFACE, NAME=%s', [fInpFile.Sets[sn].Name]);
    for j := 0 to fInpFile.Sets[sn].NumFaces - 1 do
    begin
      i := fInpFile.Sets[sn].Faces[j];
      cat := fInpFile.ElEnqire[fInpFile.Faces[i].ElemNumber].Category;
      e := fInpFile.Faces[i].ElemNumber;

      if cat in [ecTria3..ecQuad8, ecSeg2, ecSeg3] then
      begin
        if cat in [ecSeg2, ecSeg3] then
          lst.Add('%d, S?', [fInpFile.Faces[i].ElemNumber])
        else if fInpFile.ElEnqire[e].Attr > 3 then
        begin
          if fInpFile.Faces[i].Number = 1 then
            lst.Add('%d, SP ', [e])
          else
            lst.Add('%d, S%d', [e, fInpFile.Faces[i].Number - 1]);
        end
        else
        begin
          if fInpFile.Faces[i].Number = 1 then
            lst.Add('%d, SPOS ', [e])
          else
            lst.Add('%d, S%d', [e, fInpFile.Faces[i].Number + 1]);
        end;
      end
      else
        lst.Add('%d, S%d', [e, fInpFile.Faces[i].Number + 1]);
    end;
    fn := Format('%s%s.sur', [ExtractFilePath(fEditor.FileName),
      ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name)]);
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

procedure TAddBCProc.PrepareSet(const sn: integer);
begin
  CompleteSet(sn);
  {
  with fInpFile.Sets[sn] do
    if (NumFaces > 0) and (Faces[0] = 0) then
    begin
      Dec(NumFaces);
      Move(Faces[1], Faces[0], NumFaces * SizeOf(integer));
      SetLength(Faces, NumFaces);
    end;
  }
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
  Result := ExportFaces(sn);
  if Result then
    fInpFile.Parse(fEditor.Lines, fEditor.FileName);
end;

function TAddBCProc.AddFlux(const sn: integer; const flux: double): boolean;
var
  s, fn, sb: string;
  i, j, m: integer;
  cat: TElementCategory;
  lst: TStringListEx;
begin
  if fInpFile.Sets[sn].NumFaces = 0 then
  begin
    Result := AddFaces(sn);
    if not Result then
      exit;
    fInpFile.Parse(fEditor.Lines, fEditor.FileName);
  end;
  PrepareSet(sn);
  fn := Format('%s%s.dfl', [ExtractFilePath(fEditor.FileName),
    ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name)]);
  if not ChechkFileName(fn) then
    exit;

  sb := Format('** DFlux based on %s', [fInpFile.Sets[sn].Name]);
  lst := TStringListEx.Create;
  try
    lst.Add(sb);
    for j := 0 to fInpFile.Sets[sn].NumFaces - 1 do
    begin
      i := fInpFile.Sets[sn].Faces[j];
      m := fInpFile.Faces[i].ElemNumber;
      cat := fInpFile.ElEnqire[m].Category;
      if cat in [ecTria3..ecQuad8, ecSeg2, ecSeg3] then
      begin
        if cat in [ecSeg2, ecSeg3] then
          lst.Add('%d, S?, %s', [fInpFile.Faces[i].ElemNumber, Sf(flux)])
        else
        begin
          if fInpFile.ElEnqire[m].Attr > 3 then
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, SP, %s', [fInpFile.Faces[i].ElemNumber, Sf(flux)])
            else
              lst.Add('%d, S%d, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number - 1, Sf(flux)]);
          end
          else
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, SP, %s', [fInpFile.Faces[i].ElemNumber, Sf(flux)])
            else
              lst.Add('%d, S%d, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number + 1, Sf(flux)]);
          end;
        end;
      end
      else
        lst.Add('%d, S%d, %s', [fInpFile.Faces[i].ElemNumber,
          fInpFile.Faces[i].Number + 1, Sf(flux)]);
    end; // for
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;

  s := Format('** DFlux (flux=%s) based on group %s%s',
    [Sf(flux), fInpFile.Sets[sn].Name, fEditor.Lines.LineBreak]);
  s := Format('%s*DFLUX%s', [s, fEditor.Lines.LineBreak]);
  s := Format('%s%s', [s, Format(sInclude, [ExtractFileName(fn)])]);
  fEditor.InsertLineHere(s);
  Result := True;
end;

function TAddBCProc.AddPressure(const sn: integer; const pressure: double): boolean;
var
  s, fn, sb: string;
  i, j, m: integer;
  cat: TElementCategory;
  lst: TStringListEx;
begin
  if fInpFile.Sets[sn].NumFaces = 0 then
  begin
    Result := AddFaces(sn);
    if not Result then
      exit;
    fInpFile.Parse(fEditor.Lines, fEditor.FileName);
  end;
  PrepareSet(sn);
  fn := Format('%s%s.dlo', [ExtractFilePath(fEditor.FileName),
    ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name)]);
  if not ChechkFileName(fn) then
    exit;

  sb := Format('** Pressure based on %s', [fInpFile.Sets[sn].Name]);
  lst := TStringListEx.Create;
  try
    lst.Add(sb);
    for j := 0 to fInpFile.Sets[sn].NumFaces - 1 do
    begin
      i := fInpFile.Sets[sn].Faces[j];
      m := fInpFile.Faces[i].ElemNumber;
      cat := fInpFile.ElEnqire[m].Category;
      if cat in [ecTria3..ecQuad8, ecSeg2, ecSeg3] then
      begin
        if cat in [ecSeg2, ecSeg3] then
          lst.Add('%d, P?, %s', [fInpFile.Faces[i].ElemNumber, Sf(pressure)])
        else
        begin
          if fInpFile.ElEnqire[m].Attr > 3 then
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, PP, %s', [fInpFile.Faces[i].ElemNumber, Sf(pressure)])
            else
              lst.Add('%d, P%d, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number - 1, Sf(pressure)]);
          end
          else
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, PPOS, %s', [fInpFile.Faces[i].ElemNumber, Sf(pressure)])
            else
              lst.Add('%d, P%d, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number + 1, Sf(pressure)]);
          end;
        end;
      end
      else
        lst.Add('%d, P%d, %s', [fInpFile.Faces[i].ElemNumber,
          fInpFile.Faces[i].Number + 1, Sf(pressure)]);
    end; // for
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;

  s := Format('** Pressure (pressure=%g) based on group %s%s',
    [pressure, fInpFile.Sets[sn].Name, fEditor.Lines.LineBreak]);
  s := Format('%s*DLOAD%s', [s, fEditor.Lines.LineBreak]);
  s := Format('%s%s', [s, Format(sInclude, [ExtractFileName(fn)])]);
  fEditor.InsertLineHere(s);
  Result := True;
end;

function TAddBCProc.AddConvertion(const sn: integer; const sink, koef: double): boolean;
var
  s, fn, sb: string;
  i, j, m: integer;
  cat: TElementCategory;
  lst: TStringListEx;
begin
  if fInpFile.Sets[sn].NumFaces = 0 then
  begin
    Result := AddFaces(sn);
    if not Result then
      exit;
    fInpFile.Parse(fEditor.Lines, fEditor.FileName);
  end;
  PrepareSet(sn);
  fn := Format('%s%s.flm', [ExtractFilePath(fEditor.FileName),
    ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name)]);
  if not ChechkFileName(fn) then
    exit;

  sb := Format('** Film based on %s', [fInpFile.Sets[sn].Name]);
  lst := TStringListEx.Create;
  try
    lst.Add(sb);
    for j := 0 to fInpFile.Sets[sn].NumFaces - 1 do
    begin
      i := fInpFile.Sets[sn].Faces[j];
      m := fInpFile.Faces[i].ElemNumber;
      cat := fInpFile.ElEnqire[m].Category;
      if cat in [ecTria3..ecQuad8, ecSeg2, ecSeg3] then
      begin
        if cat in [ecSeg2, ecSeg3] then
          lst.Add('%d, F?, %.6f, %s', [fInpFile.Faces[i].ElemNumber, sink, Sf(koef)])
        else
        begin
          if fInpFile.ElEnqire[m].Attr > 3 then
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, FP, %.6f, %s', [fInpFile.Faces[i].ElemNumber, sink, Sf(koef)])
            else
              lst.Add('%d, F%d, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number - 1, sink, Sf(koef)]);
          end
          else
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, FP, %.6f, %s', [fInpFile.Faces[i].ElemNumber, sink, Sf(koef)])
            else
              lst.Add('%d, F%d, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number + 1, sink, Sf(koef)]);
          end;
        end;
      end
      else
        lst.Add('%d, F%d, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
          fInpFile.Faces[i].Number + 1, sink, Sf(koef)]);
    end; // for
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;

  s := Format('** Convection (sink=%.6f; koef=%s) based on group %s%s',
    [sink, Sf(koef), fInpFile.Sets[sn].Name, fEditor.Lines.LineBreak]);
  s := Format('%s*FILM%s', [s, fEditor.Lines.LineBreak]);
  s := Format('%s%s', [s, Format(sInclude, [ExtractFileName(fn)])]);
  fEditor.InsertLineHere(s);
  Result := True;
end;

function TAddBCProc.AddRadiation(const sn: integer; const sink, emissivity: double;
  const cavity: boolean): boolean;
const
  CV: array[boolean] of string = ('', 'CR');
var
  s, fn, sb: string;
  i, j, m: integer;
  cat: TElementCategory;
  lst: TStringListEx;
begin
  if fInpFile.Sets[sn].NumFaces = 0 then
  begin
    Result := AddFaces(sn);
    if not Result then
      exit;
    fInpFile.Parse(fEditor.Lines, fEditor.FileName);
  end;
  PrepareSet(sn);
  fn := Format('%s%s.rad', [ExtractFilePath(fEditor.FileName),
    ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name)]);
  if not ChechkFileName(fn) then
    exit;

  sb := Format('** Film based on %s', [fInpFile.Sets[sn].Name]);
  lst := TStringListEx.Create;
  try
    lst.Add(sb);
    for j := 0 to fInpFile.Sets[sn].NumFaces - 1 do
    begin
      i := fInpFile.Sets[sn].Faces[j];
      m := fInpFile.Faces[i].ElemNumber;
      cat := fInpFile.ElEnqire[m].Category;
      if cat in [ecTria3..ecQuad8, ecSeg2, ecSeg3] then
      begin
        if cat in [ecSeg2, ecSeg3] then
          lst.Add('%d, R?%s, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
            CV[cavity], sink, Sf(emissivity)])
        else
        begin
          if fInpFile.ElEnqire[m].Attr > 3 then
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, RP%s, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
                CV[cavity], sink, Sf(emissivity)])
            else
              lst.Add('%d, R%d%s, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number - 1, CV[cavity], sink, Sf(emissivity)]);
          end
          else
          begin
            if fInpFile.Faces[i].Number = 1 then
              lst.Add('%d, RPOS%s, %.6f, %s',
                [fInpFile.Faces[i].ElemNumber, CV[cavity], sink, Sf(emissivity)])
            else
              lst.Add('%d, R%d%s, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number + 1, CV[cavity], sink, Sf(emissivity)]);
          end;
        end;
      end
      else
        lst.Add('%d, R%d%s, %.6f, %s', [fInpFile.Faces[i].ElemNumber,
          fInpFile.Faces[i].Number + 1, CV[cavity], sink, Sf(emissivity)]);
    end; // for
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;

  s := Format('** Radiate (sink=%.6f; emissivity=%s; cavity=%s) based on group %s%s',
    [sink, Sf(emissivity), BoolToStr(cavity,true), fInpFile.Sets[sn].Name,
    fEditor.Lines.LineBreak]);
  s := Format('%s*RADIATE%s', [s, fEditor.Lines.LineBreak]);
  s := Format('%s%s', [s, Format(sInclude, [ExtractFileName(fn)])]);
  fEditor.InsertLineHere(s);
  Result := True;
end;

procedure TAddBCProc.Work(const sn: integer; const Args: array of const);
begin
  case fCmd of
    bcAddFaces: AddFaces(sn);
    bcAddPressure: AddPressure(sn, Args[0].VExtended^);
    bcAddConvection: AddConvertion(sn, Args[0].VExtended^, Args[1].VExtended^);
    bcAddFluxOnFace: AddFlux(sn, Args[0].VExtended^);
    bcAddRadiation: AddRadiation(sn, Args[0].VExtended^, Args[1].VExtended^,
        Args[2].VBoolean);
  end;
end;

end.

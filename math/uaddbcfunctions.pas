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

unit uAddBCFunctions;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, FileUtil, Controls, Dialogs, uInpTypes,
  uInpTranslator, uInpEditor;

type

  EAddBC = class(Exception);

  TAddBCCmd = (bcAddFaces, bcAddPressure);

  { TAddBCProc }

  TAddBCProc = class
  private
    fInpFile: TInpFile;
    fEditor: TInpEditor;
    fCmd: TAddBCCmd;
    fPressureValue: Double;

    procedure CompleteSet(const sn: Integer);
    function ExportFaces(const sn: Integer): Boolean;
    function AddFaces(const sn: Integer): Boolean;
    function AddPressure(const sn: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(aList: TStrings; const aCmd: TAddBCCmd);
    procedure Work(const sn: Integer);
    property Editor: TInpEditor read fEditor write fEditor;
    property PressureValue: Double read fPressureValue write fPressureValue;
  end;

implementation

uses
  Math, uInpFunctions, uDerivedClasses, uFileUtils, uConsts, uDialogs,
  uEditorMisc;

function IFind(ipnt: TIntegerDynArray; const n, x0: Integer): Integer;
var
  i,ii,m,n1,n2: Integer;
begin
  i:=0;
  if (n=0) or (x0<ipnt[0]) then
    Exit(-1)
  else if x0>ipnt[n-1] then
    // is no member of array
    Exit(-2)
  else begin
    // search the intersection, regula falsi
    n1:=0;
    n2:=n;
    for ii:=0 to n-1 do begin
      m:=(n2+n1) div 2;
      if x0>=ipnt[m] then
        n1:=m;
      if x0<ipnt[m] then
        n2:=m;
      if (n2-n1)=1 then
        break;
    end;
    i:=n1;
    if x0=ipnt[i] then
    // is member of array at pos i
      Exit(i);
  end;
  // is no member of array
  Exit(-3);
end;

function GetFNumFromCategory(const c: TElementCategory): Integer;
begin
  case c of
    ecTria3: result:=3;
    ecTria6: result:=6;
    ecQuad4: result:=4;
    ecQuad8: result:=8;
    ecSeg2: result:=2;
    ecSeg3: result:=3;
    else result:=0;
  end;
end;

function PromptNewFileName(var fn: string): Boolean;
var
  i: integer;
begin
  fn:=ZIncFileName(fn);
  result:=InputQueryEx(sNewFileName,sEnterNewFileName,fn);
end;

function ChechkFileName(var fn: string): Boolean;
var
  s: string;
begin
  result:=true;
  if ZFileExists(fn) then begin
    result:=false;
    s:=ExtractFileName(fn);
    case MessageDlg(sOverwriteConfirm,
      Format(sOverwriteMsg,[fn]),mtConfirmation,mbYesNoCancel,0) of
        mrCancel: Exit;
        mrNo: begin
          result:=PromptNewFileName(s);
          if result then
            fn:=ExtractFilePath(fn)+s;
        end;
        mrYes: result:=true;
      end;
  end;
end;

{ TAddBCProc }


constructor TAddBCProc.Create;
begin
  fEditor:=nil;
  fInpFile:=TInpFile.Create;
  fPressureValue:=0;
end;

destructor TAddBCProc.Destroy;
begin
  FreeAndNil(fInpFile);
  inherited Destroy;
end;

procedure TAddBCProc.Load(aList: TStrings; const aCmd: TAddBCCmd);
var
  i: Integer;
  t: TSet;
begin
  if fEditor=nil then
    raise EAddBC.Create('Not assigned editor');
  fInpFile.Parse(fEditor.Lines,fEditor.FileName);
  fCmd:=aCmd;
  aList.BeginUpdate;
  try
    aList.Clear;
    for i:=0 to fInpFile.Summen.sets-1 do begin
      t:=fInpFile.Sets[i];
      if (t.NumNodes=0) or AnsiSameText(t.Name,'nall') then
        continue;
      case fCmd of
        bcAddFaces: if (t.NumElements>0) or (t.NumFaces>0) then
          continue;
      end;
      aList.AddObject(t.Name,TObject(PtrUint(i)));
    end;
  finally
    aList.EndUpdate;
  end;

end;

function TAddBCProc.ExportFaces(const sn: Integer): Boolean;
var
  lst: TStringListEx;
  s,fn,sb: string;
  i,j,e,n: Integer;
  cat: TElementCategory;
begin
  result:=false;
  sb:=Format('** Surfaces based on %s',[fInpFile.Sets[sn].Name]);
  lst:=TStringListEx.Create;
  try
    lst.Add(sb);
    lst.Add('*SURFACE, NAME=%s',[fInpFile.Sets[sn].Name]);

    for j:=0 to fInpFile.Sets[sn].NumFaces-1 do begin
      i:=fInpFile.Sets[sn].Faces[j];
      cat:=fInpFile.ElEnqire[fInpFile.Faces[i].ElemNumber].Category;
      e:=fInpFile.Faces[i].ElemNumber;

      if cat in [ecTria3..ecQuad8,ecSeg2,ecSeg3] then begin
        if cat in [ecSeg2,ecSeg3] then
          lst.Add('%d, S?',[fInpFile.Faces[i].ElemNumber])
        else if fInpFile.ElEnqire[e].Attr>3 then begin
          n:=fInpFile.Faces[i].Number;
          if n=1 then
            lst.Add('%d, SP ',[e])
          else
            lst.Add('%d, S%d',[e,n-1]);
        end else begin
          n:=fInpFile.Faces[i].Number;
          if n=1 then
            lst.Add('%d, SPOS ',[e])
          else
            lst.Add('%d, S%d',[e,n+1]);
        end
      end else
        lst.Add('%d, S%d',[e,fInpFile.Faces[i].Number+1]);
    end;
    fn:=Format('%s%s.sur',[ExtractFilePath(fEditor.FileName),
      ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name)]);
    if not ChechkFileName(fn) then
      exit;
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;


  s:=Format('%s%s%s',[sb,fEditor.Lines.LineBreak,Format(sInclude,[ExtractFileName(fn)])]);
  with fEditor do
    InsertLine(s,CaretY,CaretX);
  result:=true;
end;

procedure TAddBCProc.CompleteSet(const sn: Integer);
var
  i,j,k,n,m: Integer;
begin
  m:=fInpFile.Sets[sn].NumFaces;
  for i:=0 to fInpFile.Summen.f-1 do begin
    n:=GetFNumFromCategory(fInpFile.Faces[i].Category);
    k:=0;
    for j:=0 to n-1 do begin
      with fInpFile.Sets[sn] do
        if IFind(Nodes,NumNodes,fInpFile.Faces[i].Node[j])>-1 then
          Inc(k);
    end;
    if k=n then begin
      with fInpFile.Sets[sn] do begin
        SetLength(Faces,NumFaces+1);
        Faces[NumFaces]:=i;
        Inc(NumFaces);
      end;
    end;
  end; // for

  if fInpFile.Sets[sn].NumFaces-m<>0 then begin
    QSort(@fInpFile.Sets[sn].Faces[0],fInpFile.Sets[sn].NumFaces,
      SizeOf(Integer),@CompareInt);
    n:=0;
    for i:=1 to fInpFile.Sets[sn].NumFaces-1 do
	    if fInpFile.Sets[sn].Faces[n]<>fInpFile.Sets[sn].Faces[i] then begin
        Inc(n);
        fInpFile.Sets[sn].Faces[n]:=fInpFile.Sets[sn].Faces[i];
      end;
    fInpFile.Sets[sn].NumFaces:=n+1;
  end;

  m:=fInpFile.Sets[sn].NumFaces;
  for i:=0 to fInpFile.Summen.f-1 do begin
    with fInpFile.Sets[sn] do begin
      if IFind(Elements,NumElements,fInpFile.Faces[i].ElemNumber)>-1 then begin
        SetLength(Faces,NumFaces+1);
        Faces[NumFaces]:=i;
        Inc(NumFaces);
      end;
    end;
  end;

  if fInpFile.Sets[sn].NumFaces-m<>0 then begin
    QSort(@fInpFile.Sets[sn].Faces[0],fInpFile.Sets[sn].NumFaces,
      SizeOf(Integer),@CompareInt);
    // erase multiple entities
    n:=0;
    for i:=1 to fInpFile.Sets[sn].NumFaces-1 do
	    if fInpFile.Sets[sn].Faces[n]<>fInpFile.Sets[sn].Faces[i] then begin
        Inc(n);
        fInpFile.Sets[sn].Faces[n]:=fInpFile.Sets[sn].Faces[i];
      end;
    fInpFile.Sets[sn].NumFaces:=n+1;
  end;
end;

function TAddBCProc.AddFaces(const sn: Integer): Boolean;
var
  m: Integer;

  procedure EraseMultipleEntities;
  var
    i,n: integer;
  begin
    if (fInpFile.Sets[sn].NumNodes-m)<>0 then begin
      QSort(@fInpFile.Sets[sn].Nodes[0],fInpFile.Sets[sn].NumNodes,
        SizeOf(Integer),@CompareInt);
      // erase multiple entities
      n:=0;
      for i:=1 to fInpFile.Sets[sn].NumNodes-1 do
	      if fInpFile.Sets[sn].Nodes[n]<>fInpFile.Sets[sn].Nodes[i] then begin
          Inc(n);
          fInpFile.Sets[sn].Nodes[n]:=fInpFile.Sets[sn].Nodes[i];
        end;
      fInpFile.Sets[sn].NumNodes:=n+1;
    end;
  end;

var

  i,j,k,n: integer;
begin
  CompleteSet(sn);
  m:=fInpFile.Sets[sn].NumNodes;
  for i:=0 to fInpFile.Sets[sn].NumFaces-1 do begin
    k:=fInpFile.Sets[sn].Faces[i];
    n:=GetFNumFromCategory(fInpFile.Faces[k].Category);
    for j:=0 to n-1 do
      with fInpFile.Sets[sn] do begin
        SetLength(Nodes,NumNodes+1);
        Nodes[NumNodes]:=fInpFile.Faces[Faces[i]].Node[j];
        Inc(NumNodes);
      end;
  end;
  EraseMultipleEntities;
  result:=ExportFaces(sn);
end;

function TAddBCProc.AddPressure(const sn: Integer): Boolean;
var
  s,fn,sb: string;
  i,j,n,m: Integer;
  cat: TElementCategory;
  lst: TStringListEx;
begin
  if fInpFile.Sets[sn].NumFaces=0 then begin
    result:=AddFaces(sn);
    if not result then
      exit;
    fInpFile.Parse(fEditor.Lines,fEditor.FileName);
  end;
  CompleteSet(sn);
  fn:=Format('%s%s.dlo',[ExtractFilePath(fEditor.FileName),
    ZReplaceInvalidFileNameChars(fInpFile.Sets[sn].Name)]);
  if not ChechkFileName(fn) then
    exit;
  n:=fInpFile.Sets[sn].NumFaces;

  sb:=Format('** Pressure based on %s',[fInpFile.Sets[sn].Name]);
  lst:=TStringListEx.Create;
  try
    lst.Add(sb);
    for j:=0 to n-1 do begin
      i:=fInpFile.Sets[sn].Faces[j];
      m:=fInpFile.Faces[i].ElemNumber;
      cat:=fInpFile.ElEnqire[m].Category;
      if cat in [ecTria3..ecQuad8,ecSeg2,ecSeg3] then begin
        if cat in [ecSeg2,ecSeg3] then
          lst.Add('%d, P?, %g',[fInpFile.Faces[i].ElemNumber,fPressureValue])
        else begin
          if fInpFile.ElEnqire[m].Attr>3 then begin
            if fInpFile.Faces[i].Number=1 then
              lst.Add('%d, PP, %g',[fInpFile.Faces[i].ElemNumber,fPressureValue])
            else
              lst.Add('%d, P%d, %g',[fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number-1,fPressureValue]);
          end else begin
            if fInpFile.Faces[i].Number=1 then
              lst.Add('%d, PPOS, %g',[fInpFile.Faces[i].ElemNumber,fPressureValue])
            else
              lst.Add('%d, P%d, %g',[fInpFile.Faces[i].ElemNumber,
                fInpFile.Faces[i].Number+1,fPressureValue]);
          end;
        end;
      end else
        lst.Add('%d, P%d, %g',[fInpFile.Faces[i].ElemNumber,
          fInpFile.Faces[i].Number+1,fPressureValue]);
    end; // for
    lst.SaveToFile(fn);
  finally
    lst.Free;
  end;

  s:=Format('** Pressure %g on group %s%s',
    [fPressureValue,fInpFile.Sets[sn].Name,fEditor.Lines.LineBreak]);
  s:=Format('%s*DLOAD%s',[s,fEditor.Lines.LineBreak]);
  s:=Format('%s%s',[s,Format(sInclude,[ExtractFileName(fn)])]);
  with fEditor do
    InsertLine(s,CaretY,CaretX);
  result:=true;
end;

procedure TAddBCProc.Work(const sn: Integer);
begin
  case fCmd of
    bcAddFaces: AddFaces(sn);
    bcAddPressure: AddPressure(sn);
  end;
end;

end.




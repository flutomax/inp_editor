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

unit uInpTranslator;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, Contnrs, SysUtils, uInpTypes, uDerivedClasses;

type

  TReadResult = (rrNormal,rrNextIns,rrEnd);


  EInpFile = class(Exception);

  { TInpFile }

  TInpFile = class
  private
    fLog: TStrings;
    fCurr: Integer;
    fFileStack: TObjectStack;
    fFileName: string;
    fReader: TFileReader;
    fLastProcessLine: Integer;
    function ParseNumberString(const s: string): Double;
    function GetSetIndex(const s: string): Integer;
    function GetShapeIndex(const s: string): Integer;
    function SetupSetIndex(const s, typ: string): Integer;
    function AddToSet(const nset: Integer; const typ: string; const number: Integer): Integer;
    function GetElemFaceNodes(const el, face: Integer; nface: PIntegerArray): Integer;
    function GetLine(out line: string): TReadResult;
    function ExtractNodes(var s: string): Boolean;
    function ExtractElements(var s: string): Boolean;
    function ExtractElset(var s: string): Boolean;
    function ExtractNset(var s: string): Boolean;
    function ExtractSurface(var s: string): Boolean;
    procedure SetCurrentReader(path: string);
    procedure CommonEdge2(a, b: PIntegerArray);
    procedure ExtractElement(const category: TElementCategory;
      const number: Integer; buff: TElementBuffer);
    procedure InitElements;
    procedure GetElemNormales;
    procedure GetFaceNormales;
    procedure MakeSurfaces;
    procedure SelectDisplayFaces;
    procedure CalcElemNormQuad4(const i, n1, n2, n3, n4, f: Integer);
    procedure CalcElemNormTri3(const i, n1, n2, n3, f: Integer);
    procedure CalcFaceNormQuad4(const i, n1, n2, n3, n4, f: Integer);
    procedure CalcFaceNormTri3(const i, n1, n2, n3, f: Integer);
    procedure CalcScale;
    procedure CalcStat;
    procedure ScaleNodes;
    procedure ResetReaders;
  public
    Nodes: TNodeDynArray;
    Elements: TElementDynArray;
    ElEnqire: TElementDynArray;
    Faces: TFaceDynArray;
    Edges: TEdgeDynArray;
    Sets: TSetDynArray;
    Summen: TSummen;
    Scale: TScale;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Parse(lines: TStrings; const aFileName: string);
    property LastProcessLine: Integer read fLastProcessLine;
    property Log: TStrings read fLog;
  end;

  function GetCategoreNumN(const c: TElementCategory): Integer;

implementation

uses
  {$IF FPC_FULLVERSION>=30004}LazFileUtils,{$ENDIF}
  FileUtil, Math, uInpFunctions;

type

  TStrArray = array of string;

resourcestring

  sErrBadNodeNum = 'ERROR: In file "%s", line %d. Bad node number.';
  sErrBadFloatValue = 'ERROR: In file "%s", line %d. Bad number value "%s".';
  sErrBadNodeRange = 'ERROR: Node:%d used in element:%d not existend. Highest node-nr:%d';
  sErrOpenFile = 'ERROR: Can''t open file "%s"!';
  sErrElSetNotDef = 'ERROR: In *ELSET set "%s" not defined.';
  sErrNSetNotDef = 'ERROR: In *NSET set "%s" not defined.';
  sErrSetIndxOut = 'ERROR: Index "%d" of set out of bounds (0..%d).';
  sErrSetUndef = 'ERROR: Set "%d" is undefined.';
  sErrSetNoExst = 'ERROR: Set "%d" does not exist.';
  sErrShpNoExst = 'ERROR: Shape "%d" does not exist.';
  sErrIncFileNoExst = 'ERROR: Include file "%s" does not exist.';
  sErrSurfNotDef = 'ERROR: In *SURFACE set "%s" not defined.';
  sErrSurfInvDat1 = 'ERROR: In *SURFACE invalid data format "%s".';
  sErrSurfInvDat2 = 'ERROR: In *SURFACE invalid data format in file "%s", line %d.';
  sErrFaceNoExst = 'ERROR: In set "%s", face %d of element %d does not exist.';

  sWrnSetNoRec = 'WARNING: In set type "%s" not recognized.';

const

  INP_ELEMENT_NAME: array[TElementType] of string = (
    'S3', 'DS3', 'M3D3', 'SFM3D3', 'STRI3', 'CPS3', 'CPE3', 'CAX3',
    'S6', 'DS6', 'M3D6', 'SC6', 'SFM3D6', 'STRI65', 'CPS6', 'CPE6', 'CAX6',
    'S4', 'S4R', 'CPS4', 'CPS4R', 'CPE4', 'CPE4R', 'CAX4', 'CAX4R',
    'S8', 'S8R', 'CPS8', 'CPS8R', 'CPE8', 'CPE8R', 'CAX8', 'CAX8R',
    'C3D4', 'C3D10', 'C3D8', 'C3D8R',
    'C3D8I', 'C3D20', 'C3D20R', 'C3D20RI', 'C3D6', 'C3D15', 'B21', 'B31', 'B31R',
    'T3D2', 'B22', 'B32', 'B32R', 'T3D3', '');

   INP_NUMBER_OF_NODES: array[TElementCategory] of Integer = (
    3, 6, 4, 8, 4, 10, 8, 20, 6, 15, 2, 3, 0);

   INP_NUMBER_OF_FACES: array[TElementCategory] of Integer = (
    1, 4, 2, 8, 4, 16, 6, 48, 6, 48, 1, 1, 0);


function GetCategoreNumN(const c: TElementCategory): Integer;
begin
  result:=INP_NUMBER_OF_NODES[c];
end;

function SplitString(s: string; delimiter: char): TStrArray;
var
  p: Integer;
begin
  result:=nil;
  repeat
    SetLength(Result,Length(Result)+1);
    p:=Pos(Delimiter,s);
    if p=0 then
      p:=Length(s)+1;
    Result[Length(Result)-1]:=Trim(Copy(s,1,p-1));
    Delete(s,1,p);
  until Length(s)=0;
end;

function ElementTypeToCategory(const et: TElementType): TElementCategory;
begin
  case et of
    etS3..etCAX3: result:=ecTria3;
    etS6..etCAX6: result:=ecTria6;
    etS4..etCAX4R: result:=ecQuad4;
    etS8..etCAX8R: result:=ecQuad8;
    etC3D4: result:=ecTetra4;
    etC3D10: result:=ecTetra10;
    etC3D8..etC3D8I: result:=ecHexa8;
    etC3D20..etC3D20RI: result:=ecHexa20;
    etC3D6: result:=ecPenta6;
    etC3D15: result:=ecPenta15;
    etB21..etT3D2: result:=ecSeg2;
    etB22..etT3D3: result:=ecSeg3;
    else result:=ecUnknown;
  end;
end;

function ExtractElementType(s: string): TElementType;
var
  p: integer;
  i: TElementType;
begin
  result:=etUnknown;
  s:=AnsiUpperCase(Trim(s));
  for i:=Low(TElementType) to Pred(High(TElementType)) do
    if s=INP_ELEMENT_NAME[i] then begin
      result:=i;
      break;
    end;
end;

function IsStringStarted(const line, pattern: string): Boolean;
begin
  result:=AnsiSameText(Copy(line,1,Length(pattern)),pattern);
end;

function ExtractArgumentValue(const line, pattern: string): string;
var
  list: TStringList;
begin
  list:=TStringList.Create;
  try
    list.CommaText:=line;
    result:=list.Values[pattern];
  finally
    list.Free;
  end;
end;

function ArgumentNameExists(const line, pattern: string): Boolean;
var
  list: TStringList;
begin
  list:=TStringList.Create;
  try
    list.CommaText:=line;
    result:=list.IndexOfName(pattern)>=0;
  finally
    list.Free;
  end;
end;


{ TInpFile }

constructor TInpFile.Create;
begin
  fFileStack:=TObjectStack.Create;
  fLog:=TStringList.Create;
  fCurr:=0;
  fFileName:='';
  fReader:=nil;
  ElEnqire:=nil;
  Elements:=nil;
  Nodes:=nil;
  Faces:=nil;
  Edges:=nil;
  Sets:=nil;
end;

destructor TInpFile.Destroy;
begin
  Clear;
  fLog.Free;
  fFileStack.Free;
  inherited;
end;

procedure TInpFile.Clear;
var
  i: integer;
begin
  fLog.Clear;
  for i:=0 to Length(Elements)-1 do begin
    Elements[i].Side:=nil;
    FillChar(Elements[i],SizeOf(TElement),0);
  end;
  for i:=0 to Length(ElEnqire)-1 do begin
    ElEnqire[i].Side:=nil;
    FillChar(ElEnqire[i],SizeOf(TElement),0);
  end;
  for i:=0 to Length(Faces)-1 do begin
    Faces[i].Side:=nil;
    FillChar(Faces[i],SizeOf(TFace),0);
  end;
  for i:=0 to Length(Sets)-1 do begin
    Sets[i].Name:='';
    Sets[i].Sets:=nil;
    Sets[i].Nodes:=nil;
    Sets[i].Faces:=nil;
    Sets[i].Elements:=nil;
    Sets[i].Shapes:=nil;
    Sets[i].Elfaces:=nil;
    FillChar(Sets[i],SizeOf(TSet),0);
  end;
  ResetReaders;
  ElEnqire:=nil;
  Elements:=nil;
  Nodes:=nil;
  Faces:=nil;
  Edges:=nil;
  Sets:=nil;
  fReader:=nil;
  Summen.Model:='';
  Summen.UHeader:='';
  Summen.PHeader:='';
  FillChar(Summen,SizeOf(TSummen),0);
  Summen.emin:=MaxInt;
  Summen.emax:=-MaxInt;
  Summen.nmin:=MaxInt;
  Summen.nmax:=-MaxInt;
  FillChar(Scale,SizeOf(TScale),0);
  fLastProcessLine:=0;
end;


procedure TInpFile.ResetReaders;
var
  i: integer;
  obj: TObject;
begin
  for i:=fFileStack.Count-1 downto 0 do begin
    obj:=fFileStack.Pop;
    FreeAndNil(obj);
  end;
end;

function TInpFile.ParseNumberString(const s: string): Double;
var
  e: Integer;
begin
  result:=0;
  if Length(s)=0 then
    Exit; // Z - coord can be empty
  Val(s,result,e);
  if e<>0 then
    fLog.Add(sErrBadFloatValue,[fReader.FileName,fReader.CurrentLine,s]);
end;

procedure TInpFile.CalcElemNormQuad4(const i, n1, n2, n3, n4, f: Integer);
var
  v1,v2,v3: TPoint3D;
begin
  v1:=VSub(Nodes[n3],Nodes[n1]);
  v2:=VSub(Nodes[n4],Nodes[n3]);
  v3:=VCross(v1,v2);
  VNormal(v3,ElEnqire[i].Side[f]);
end;

procedure TInpFile.CalcElemNormTri3(const i, n1, n2, n3, f: Integer);
var
  v1,v2,v3: TPoint3D;
begin
  v1:=VSub(Nodes[n2],Nodes[n1]);
  v2:=VSub(Nodes[n3],Nodes[n1]);
  v3:=VCross(v1,v2);
  VNormal(v3,ElEnqire[i].Side[f]);
end;

procedure TInpFile.CalcFaceNormQuad4(const i, n1, n2, n3, n4, f: Integer);
var
  v1,v2,v3: TPoint3D;
begin
  v1:=VSub(Nodes[n3],Nodes[n1]);
  v2:=VSub(Nodes[n4],Nodes[n3]);
  v3:=VCross(v1,v2);
  VNormal(v3,Faces[i].Side[f]);
end;

procedure TInpFile.CalcFaceNormTri3(const i, n1, n2, n3, f: Integer);
var
  v1,v2,v3: TPoint3D;
begin
  v1:=VSub(Nodes[n2],Nodes[n1]);
  v2:=VSub(Nodes[n3],Nodes[n1]);
  v3:=VCross(v1,v2);
  VNormal(v3,Faces[i].Side[f]);
end;

procedure TInpFile.GetElemNormales;
var
  i,j: integer;
begin
  for i:=0 to Summen.e-1 do begin
    j:=ElEnqire[i].Number;
    case ElEnqire[j].Category of

      ecTria3: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[0], ElEnqire[j].Node[1], ElEnqire[j].Node[2],0);
      end; // ecTria3

      ecTria6: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[0], ElEnqire[j].Node[3], ElEnqire[j].Node[5],0);
        CalcElemNormTri3(j,ElEnqire[j].Node[2], ElEnqire[j].Node[5], ElEnqire[j].Node[4],1);
        CalcElemNormTri3(j,ElEnqire[j].Node[4], ElEnqire[j].Node[5], ElEnqire[j].Node[3],2);
        CalcElemNormTri3(j,ElEnqire[j].Node[3], ElEnqire[j].Node[1], ElEnqire[j].Node[4],3);
      end; // ecTria6

      ecQuad4: begin
        CalcElemNormQuad4(j,ElEnqire[j].Node[0], ElEnqire[j].Node[1],
          ElEnqire[j].Node[2], ElEnqire[j].Node[3], 0);
      end; // ecQuad4

      ecQuad8: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[0], ElEnqire[j].Node[4], 0);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[4], ElEnqire[j].Node[1], 1);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[1], ElEnqire[j].Node[5], 2);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[5], ElEnqire[j].Node[2], 3);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[2], ElEnqire[j].Node[6], 4);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[6], ElEnqire[j].Node[3], 5);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[3], ElEnqire[j].Node[7], 6);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[7], ElEnqire[j].Node[0], 7);
      end; // ecQuad8

      ecTetra4: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[0], ElEnqire[j].Node[1], ElEnqire[j].Node[3], 0);
        CalcElemNormTri3(j,ElEnqire[j].Node[1], ElEnqire[j].Node[2], ElEnqire[j].Node[3], 1);
        CalcElemNormTri3(j,ElEnqire[j].Node[2], ElEnqire[j].Node[0], ElEnqire[j].Node[3], 2);
        CalcElemNormTri3(j,ElEnqire[j].Node[0], ElEnqire[j].Node[2], ElEnqire[j].Node[1], 3);
      end; // ecTetra4

      ecTetra10: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[0], ElEnqire[j].Node[4], ElEnqire[j].Node[7], 0);
        CalcElemNormTri3(j,ElEnqire[j].Node[7], ElEnqire[j].Node[4], ElEnqire[j].Node[8], 1);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[4], ElEnqire[j].Node[1], 2);
        CalcElemNormTri3(j,ElEnqire[j].Node[7], ElEnqire[j].Node[8], ElEnqire[j].Node[3], 3);
        CalcElemNormTri3(j,ElEnqire[j].Node[1], ElEnqire[j].Node[5], ElEnqire[j].Node[8], 4);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[5], ElEnqire[j].Node[9], 5);
        CalcElemNormTri3(j,ElEnqire[j].Node[9], ElEnqire[j].Node[5], ElEnqire[j].Node[2], 6);
        CalcElemNormTri3(j,ElEnqire[j].Node[8], ElEnqire[j].Node[9], ElEnqire[j].Node[3], 7);
        CalcElemNormTri3(j,ElEnqire[j].Node[2], ElEnqire[j].Node[6], ElEnqire[j].Node[9], 8);
        CalcElemNormTri3(j,ElEnqire[j].Node[9], ElEnqire[j].Node[6], ElEnqire[j].Node[7], 9);
        CalcElemNormTri3(j,ElEnqire[j].Node[7], ElEnqire[j].Node[6], ElEnqire[j].Node[0],10);
        CalcElemNormTri3(j,ElEnqire[j].Node[9], ElEnqire[j].Node[7], ElEnqire[j].Node[3],11);
        CalcElemNormTri3(j,ElEnqire[j].Node[0], ElEnqire[j].Node[6], ElEnqire[j].Node[4],12);
        CalcElemNormTri3(j,ElEnqire[j].Node[4], ElEnqire[j].Node[6], ElEnqire[j].Node[5],13);
        CalcElemNormTri3(j,ElEnqire[j].Node[5], ElEnqire[j].Node[6], ElEnqire[j].Node[2],14);
        CalcElemNormTri3(j,ElEnqire[j].Node[4], ElEnqire[j].Node[5], ElEnqire[j].Node[1],15);
      end; // ecTetra10

      ecHexa8: begin
        CalcElemNormQuad4(j,ElEnqire[j].Node[1], ElEnqire[j].Node[2],
          ElEnqire[j].Node[6], ElEnqire[j].Node[5], 0);
        CalcElemNormQuad4(j,ElEnqire[j].Node[5], ElEnqire[j].Node[6],
          ElEnqire[j].Node[7], ElEnqire[j].Node[4], 1);
        CalcElemNormQuad4(j,ElEnqire[j].Node[4], ElEnqire[j].Node[7],
          ElEnqire[j].Node[3], ElEnqire[j].Node[0], 2);
        CalcElemNormQuad4(j,ElEnqire[j].Node[5], ElEnqire[j].Node[4],
          ElEnqire[j].Node[0], ElEnqire[j].Node[1], 3);
        CalcElemNormQuad4(j,ElEnqire[j].Node[1], ElEnqire[j].Node[0],
          ElEnqire[j].Node[3], ElEnqire[j].Node[2], 4);
        CalcElemNormQuad4(j,ElEnqire[j].Node[2], ElEnqire[j].Node[3],
          ElEnqire[j].Node[7], ElEnqire[j].Node[6], 5);
      end; // ecHexa8

      ecHexa20: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[ 1], ElEnqire[j].Node[ 9], 0);
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[ 9], ElEnqire[j].Node[ 2], 1);
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[ 2], ElEnqire[j].Node[14], 2);
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[14], ElEnqire[j].Node[ 6], 3);
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[ 6], ElEnqire[j].Node[17], 4);
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[17], ElEnqire[j].Node[ 5], 5);
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[ 5], ElEnqire[j].Node[13], 6);
        CalcElemNormTri3(j,ElEnqire[j].Node[21], ElEnqire[j].Node[13], ElEnqire[j].Node[ 1], 7);

        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[ 2], ElEnqire[j].Node[10], 8);
        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[10], ElEnqire[j].Node[ 3], 9);
        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[ 3], ElEnqire[j].Node[15],10);
        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[15], ElEnqire[j].Node[ 7],11);
        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[ 7], ElEnqire[j].Node[18],12);
        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[18], ElEnqire[j].Node[ 6],13);
        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[ 6], ElEnqire[j].Node[14],14);
        CalcElemNormTri3(j,ElEnqire[j].Node[22], ElEnqire[j].Node[14], ElEnqire[j].Node[ 2],15);

        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[ 0], ElEnqire[j].Node[12],16);
        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[12], ElEnqire[j].Node[ 4],17);
        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[ 4], ElEnqire[j].Node[19],18);
        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[19], ElEnqire[j].Node[ 7],19);
        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[ 7], ElEnqire[j].Node[15],20);
        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[15], ElEnqire[j].Node[ 3],21);
        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[ 3], ElEnqire[j].Node[11],22);
        CalcElemNormTri3(j,ElEnqire[j].Node[23], ElEnqire[j].Node[11], ElEnqire[j].Node[ 0],23);

        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[ 1], ElEnqire[j].Node[ 8],24);
        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[ 8], ElEnqire[j].Node[ 0],25);
        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[ 0], ElEnqire[j].Node[11],26);
        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[11], ElEnqire[j].Node[ 3],27);
        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[ 3], ElEnqire[j].Node[10],28);
        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[10], ElEnqire[j].Node[ 2],29);
        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[ 2], ElEnqire[j].Node[ 9],30);
        CalcElemNormTri3(j,ElEnqire[j].Node[24], ElEnqire[j].Node[ 9], ElEnqire[j].Node[ 1],31);

        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[ 4], ElEnqire[j].Node[16],32);
        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[16], ElEnqire[j].Node[ 5],33);
        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[ 5], ElEnqire[j].Node[17],34);
        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[17], ElEnqire[j].Node[ 6],35);
        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[ 6], ElEnqire[j].Node[18],36);
        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[18], ElEnqire[j].Node[ 7],37);
        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[ 7], ElEnqire[j].Node[19],38);
        CalcElemNormTri3(j,ElEnqire[j].Node[25], ElEnqire[j].Node[19], ElEnqire[j].Node[ 4],39);

        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[ 0], ElEnqire[j].Node[ 8],40);
        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[ 8], ElEnqire[j].Node[ 1],41);
        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[ 1], ElEnqire[j].Node[13],42);
        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[13], ElEnqire[j].Node[ 5],43);
        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[ 5], ElEnqire[j].Node[16],44);
        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[16], ElEnqire[j].Node[ 4],45);
        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[ 4], ElEnqire[j].Node[12],46);
        CalcElemNormTri3(j,ElEnqire[j].Node[20], ElEnqire[j].Node[12], ElEnqire[j].Node[ 0],47);
      end; // ecHexa20

      ecPenta6: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[0], ElEnqire[j].Node[2], ElEnqire[j].Node[1], 0);
        CalcElemNormTri3(j,ElEnqire[j].Node[5], ElEnqire[j].Node[3], ElEnqire[j].Node[4], 1);
        CalcElemNormQuad4(j,ElEnqire[j].Node[0], ElEnqire[j].Node[1],
          ElEnqire[j].Node[4], ElEnqire[j].Node[3], 2);
        CalcElemNormQuad4(j,ElEnqire[j].Node[1], ElEnqire[j].Node[2],
          ElEnqire[j].Node[5], ElEnqire[j].Node[4], 3);
        CalcElemNormQuad4(j,ElEnqire[j].Node[3], ElEnqire[j].Node[5],
          ElEnqire[j].Node[2], ElEnqire[j].Node[0], 4);
      end; // ecPenta6

      ecPenta15: begin
        CalcElemNormTri3(j,ElEnqire[j].Node[ 1], ElEnqire[j].Node[10], ElEnqire[j].Node[ 6], 0);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 6], ElEnqire[j].Node[10], ElEnqire[j].Node[15], 1);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 6], ElEnqire[j].Node[15], ElEnqire[j].Node[ 0], 2);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 0], ElEnqire[j].Node[15], ElEnqire[j].Node[ 9], 3);

        CalcElemNormTri3(j,ElEnqire[j].Node[10], ElEnqire[j].Node[ 4], ElEnqire[j].Node[15], 4);
        CalcElemNormTri3(j,ElEnqire[j].Node[15], ElEnqire[j].Node[ 4], ElEnqire[j].Node[12], 5);
        CalcElemNormTri3(j,ElEnqire[j].Node[15], ElEnqire[j].Node[12], ElEnqire[j].Node[ 9], 6);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 9], ElEnqire[j].Node[12], ElEnqire[j].Node[ 3], 7);

        CalcElemNormTri3(j,ElEnqire[j].Node[ 2], ElEnqire[j].Node[11], ElEnqire[j].Node[ 7], 8);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 7], ElEnqire[j].Node[11], ElEnqire[j].Node[16], 9);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 7], ElEnqire[j].Node[16], ElEnqire[j].Node[ 1],10);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 1], ElEnqire[j].Node[16], ElEnqire[j].Node[10],11);

        CalcElemNormTri3(j,ElEnqire[j].Node[11], ElEnqire[j].Node[ 5], ElEnqire[j].Node[16],12);
        CalcElemNormTri3(j,ElEnqire[j].Node[16], ElEnqire[j].Node[ 5], ElEnqire[j].Node[13],13);
        CalcElemNormTri3(j,ElEnqire[j].Node[16], ElEnqire[j].Node[13], ElEnqire[j].Node[10],14);
        CalcElemNormTri3(j,ElEnqire[j].Node[10], ElEnqire[j].Node[13], ElEnqire[j].Node[ 4],15);

        CalcElemNormTri3(j,ElEnqire[j].Node[ 0], ElEnqire[j].Node[ 9], ElEnqire[j].Node[ 8],16);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 8], ElEnqire[j].Node[ 9], ElEnqire[j].Node[17],17);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 8], ElEnqire[j].Node[17], ElEnqire[j].Node[ 2],18);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 2], ElEnqire[j].Node[17], ElEnqire[j].Node[11],19);

        CalcElemNormTri3(j,ElEnqire[j].Node[ 9], ElEnqire[j].Node[ 3], ElEnqire[j].Node[17],20);
        CalcElemNormTri3(j,ElEnqire[j].Node[17], ElEnqire[j].Node[ 3], ElEnqire[j].Node[14],21);
        CalcElemNormTri3(j,ElEnqire[j].Node[17], ElEnqire[j].Node[14], ElEnqire[j].Node[11],22);
        CalcElemNormTri3(j,ElEnqire[j].Node[11], ElEnqire[j].Node[14], ElEnqire[j].Node[ 5],23);

        CalcElemNormTri3(j,ElEnqire[j].Node[ 2], ElEnqire[j].Node[ 7], ElEnqire[j].Node[ 8],24);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 8], ElEnqire[j].Node[ 7], ElEnqire[j].Node[18],25);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 8], ElEnqire[j].Node[18], ElEnqire[j].Node[ 0],26);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 0], ElEnqire[j].Node[18], ElEnqire[j].Node[ 0],27);

        CalcElemNormTri3(j,ElEnqire[j].Node[ 7], ElEnqire[j].Node[ 1], ElEnqire[j].Node[18],28);
        CalcElemNormTri3(j,ElEnqire[j].Node[18], ElEnqire[j].Node[ 1], ElEnqire[j].Node[ 6],29);
        CalcElemNormTri3(j,ElEnqire[j].Node[18], ElEnqire[j].Node[ 6], ElEnqire[j].Node[ 0],30);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 0], ElEnqire[j].Node[ 6], ElEnqire[j].Node[ 0],31);

        CalcElemNormTri3(j,ElEnqire[j].Node[ 4], ElEnqire[j].Node[13], ElEnqire[j].Node[12],32);
        CalcElemNormTri3(j,ElEnqire[j].Node[12], ElEnqire[j].Node[13], ElEnqire[j].Node[19],33);
        CalcElemNormTri3(j,ElEnqire[j].Node[12], ElEnqire[j].Node[19], ElEnqire[j].Node[ 3],34);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 3], ElEnqire[j].Node[19], ElEnqire[j].Node[ 3],35);

        CalcElemNormTri3(j,ElEnqire[j].Node[13], ElEnqire[j].Node[ 5], ElEnqire[j].Node[19],36);
        CalcElemNormTri3(j,ElEnqire[j].Node[19], ElEnqire[j].Node[ 5], ElEnqire[j].Node[14],37);
        CalcElemNormTri3(j,ElEnqire[j].Node[19], ElEnqire[j].Node[14], ElEnqire[j].Node[ 3],38);
        CalcElemNormTri3(j,ElEnqire[j].Node[ 3], ElEnqire[j].Node[14], ElEnqire[j].Node[ 3],39);
      end; // ecPenta15
    end; // case
  end;  // for
end;

procedure TInpFile.GetFaceNormales;
var
  i: integer;
begin
  for i:=0 to Summen.f-1 do begin
    case Faces[i].Category of
      ecTria3: CalcFaceNormTri3(i,Faces[i].Node[0],Faces[i].Node[1],Faces[i].Node[2],0);
      ecTria6: begin
        CalcFaceNormTri3(i,Faces[i].Node[0],Faces[i].Node[3],Faces[i].Node[5],0);
        CalcFaceNormTri3(i,Faces[i].Node[2],Faces[i].Node[5],Faces[i].Node[4],1);
        CalcFaceNormTri3(i,Faces[i].Node[4],Faces[i].Node[5],Faces[i].Node[3],2);
        CalcFaceNormTri3(i,Faces[i].Node[3],Faces[i].Node[1],Faces[i].Node[4],3);
      end;
      ecQuad4: CalcFaceNormQuad4(i,Faces[i].Node[0],Faces[i].Node[1],Faces[i].Node[2],Faces[i].Node[3],0);
      ecQuad8: begin
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[0],Faces[i].Node[4],0);
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[4],Faces[i].Node[1],1);
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[1],Faces[i].Node[5],2);
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[5],Faces[i].Node[2],3);
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[2],Faces[i].Node[6],4);
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[6],Faces[i].Node[3],5);
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[3],Faces[i].Node[7],6);
        CalcFaceNormTri3(i,Faces[i].Node[8],Faces[i].Node[7],Faces[i].Node[0],7);
      end;
    end; // case Faces[i].Category
  end; // for i
end;

function TInpFile.GetElemFaceNodes(const el, face: Integer;
  nface: PIntegerArray): Integer;
var
  i, collapseFlag: Integer;
begin
  collapseFlag:=0;
  case ElEnqire[el].Category of
    ecHexa8: begin
      case face of
        0: begin
          nface^[3]:=ElEnqire[el].Node[0];
          nface^[2]:=ElEnqire[el].Node[1];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[0]:=ElEnqire[el].Node[3];
        end;

        1: begin
          nface^[3]:=ElEnqire[el].Node[4];
          nface^[2]:=ElEnqire[el].Node[7];
          nface^[1]:=ElEnqire[el].Node[6];
          nface^[0]:=ElEnqire[el].Node[5];
        end;

        2: begin
          nface^[3]:=ElEnqire[el].Node[0];
          nface^[2]:=ElEnqire[el].Node[4];
          nface^[1]:=ElEnqire[el].Node[5];
          nface^[0]:=ElEnqire[el].Node[1];
        end;

        3: begin
          nface^[3]:=ElEnqire[el].Node[1];
          nface^[2]:=ElEnqire[el].Node[5];
          nface^[1]:=ElEnqire[el].Node[6];
          nface^[0]:=ElEnqire[el].Node[2];
        end;

        4: begin
          nface^[3]:=ElEnqire[el].Node[2];
          nface^[2]:=ElEnqire[el].Node[6];
          nface^[1]:=ElEnqire[el].Node[7];
          nface^[0]:=ElEnqire[el].Node[3];
        end;

        5: begin
          nface^[3]:=ElEnqire[el].Node[3];
          nface^[2]:=ElEnqire[el].Node[7];
          nface^[1]:=ElEnqire[el].Node[4];
          nface^[0]:=ElEnqire[el].Node[0];
        end;
      end; //case face
      Exit(4);
    end; // ecHexa8

    ecPenta6: begin
      case face of
        0: begin
          nface^[0]:=ElEnqire[el].Node[0];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[2]:=ElEnqire[el].Node[1];
          Exit(3);
        end;

        1: begin
          nface^[0]:=ElEnqire[el].Node[3];
          nface^[1]:=ElEnqire[el].Node[4];
          nface^[2]:=ElEnqire[el].Node[5];
          Exit(3);
        end;

        2: begin
          nface^[0]:=ElEnqire[el].Node[0];
          nface^[1]:=ElEnqire[el].Node[1];
          nface^[2]:=ElEnqire[el].Node[4];
          nface^[3]:=ElEnqire[el].Node[3];
        end;

        3: begin
          nface^[0]:=ElEnqire[el].Node[1];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[2]:=ElEnqire[el].Node[5];
          nface^[3]:=ElEnqire[el].Node[4];
        end;

        4: begin
          nface^[0]:=ElEnqire[el].Node[2];
          nface^[1]:=ElEnqire[el].Node[0];
          nface^[2]:=ElEnqire[el].Node[3];
          nface^[3]:=ElEnqire[el].Node[5];
        end;
      end; // case face

      // faces might be collapsed, investigate only surfs with 4 nodes and ev. redefine
      if nface^[0]=nface^[1] then begin
        nface^[0]:=nface^[1];
        nface^[1]:=nface^[2];
        nface^[2]:=nface^[3];
        Inc(collapseFlag);
      end
      else
      if nface^[1]=nface^[2] then begin
        nface^[0]:=nface^[0];
        nface^[1]:=nface^[2];
        nface^[2]:=nface^[3];
        Inc(collapseFlag);
      end
      else
      if nface^[2]=nface^[3] then
        Inc(collapseFlag)
      else
      if nface^[3]=nface^[0] then
        Inc(collapseFlag);

      if collapseFlag>1 then Exit(0);
      if collapseFlag=1 then Exit(3);
      Exit(4);
    end; // ecPenta6

    ecTetra4:
    begin
      case face of
        0: begin
          nface^[0]:=ElEnqire[el].Node[0];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[2]:=ElEnqire[el].Node[1];
        end;

        1: begin
          nface^[0]:=ElEnqire[el].Node[0];
          nface^[1]:=ElEnqire[el].Node[1];
          nface^[2]:=ElEnqire[el].Node[3];
        end;

        2: begin
          nface^[0]:=ElEnqire[el].Node[1];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[2]:=ElEnqire[el].Node[3];
        end;

        3: begin
          nface^[0]:=ElEnqire[el].Node[2];
          nface^[1]:=ElEnqire[el].Node[0];
          nface^[2]:=ElEnqire[el].Node[3];
        end;
      end; // case face
      Exit(3);
    end; // ecTetra4

    ecHexa20: begin
      case face of

        0: begin
          PInteger(nface)^:=ElEnqire[el].Node[3];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[2];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[1];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[0];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[10]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[9];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[8];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[11]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[24];
        end;

        1: begin
          PInteger(nface)^:=ElEnqire[el].Node[5];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[6];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[7];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[4];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[17]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[18]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[19]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[16]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[25];
        end;

        2: begin
          PInteger(nface)^:=ElEnqire[el].Node[1];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[5];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[4];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[0];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[13]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[16]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[12]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[8];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[20];
        end;

        3: begin
          PInteger(nface)^:=ElEnqire[el].Node[2];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[6];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[5];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[1];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[14]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[17]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[13]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[9];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[21];
        end;

        4: begin
          PInteger(nface)^:=ElEnqire[el].Node[3];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[7];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[6];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[2];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[15]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[18]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[14]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[10]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[22];
        end;

        5: begin
          PInteger(nface)^:=ElEnqire[el].Node[0];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[4];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[7];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[3];  Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[12]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[19]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[15]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[11]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[23];
        end;
      end; // case face
      Exit(8);
    end; // ecHexa20

    ecPenta15: begin
      case face of

        0: begin
          nface^[0]:=ElEnqire[el].Node[0];
          nface^[3]:=ElEnqire[el].Node[8];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[4]:=ElEnqire[el].Node[7];
          nface^[2]:=ElEnqire[el].Node[1];
          nface^[5]:=ElEnqire[el].Node[6];
          Exit(6);
        end;

        1: begin
          nface^[0]:=ElEnqire[el].Node[3];
          nface^[3]:=ElEnqire[el].Node[12];
          nface^[1]:=ElEnqire[el].Node[4];
          nface^[4]:=ElEnqire[el].Node[13];
          nface^[2]:=ElEnqire[el].Node[5];
          nface^[5]:=ElEnqire[el].Node[14];
          Exit(6);
        end;

        2: begin
          nface^[0]:=ElEnqire[el].Node[0];
          nface^[4]:=ElEnqire[el].Node[6];
          nface^[1]:=ElEnqire[el].Node[1];
          nface^[5]:=ElEnqire[el].Node[10];
          nface^[2]:=ElEnqire[el].Node[4];
          nface^[6]:=ElEnqire[el].Node[12];
          nface^[3]:=ElEnqire[el].Node[3];
          nface^[7]:=ElEnqire[el].Node[9];
          nface^[8]:=ElEnqire[el].Node[15];
        end;

        3: begin
          nface^[0]:=ElEnqire[el].Node[1];
          nface^[4]:=ElEnqire[el].Node[7];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[5]:=ElEnqire[el].Node[11];
          nface^[2]:=ElEnqire[el].Node[5];
          nface^[6]:=ElEnqire[el].Node[13];
          nface^[3]:=ElEnqire[el].Node[4];
          nface^[7]:=ElEnqire[el].Node[10];
          nface^[8]:=ElEnqire[el].Node[16];
        end;

        4: begin
          nface^[0]:=ElEnqire[el].Node[2];
          nface^[4]:=ElEnqire[el].Node[8];
          nface^[1]:=ElEnqire[el].Node[0];
          nface^[5]:=ElEnqire[el].Node[9];
          nface^[2]:=ElEnqire[el].Node[3];
          nface^[6]:=ElEnqire[el].Node[14];
          nface^[3]:=ElEnqire[el].Node[5];
          nface^[7]:=ElEnqire[el].Node[11];
          nface^[8]:=ElEnqire[el].Node[17];
        end;

      end; // case face

      // faces might be collapsed, investigate only surfs with 4 nodes and ev. redefine
      if nface^[0]=nface^[1] then begin
        nface^[0]:=nface^[1];
        nface^[1]:=nface^[2];
        nface^[2]:=nface^[3];
        nface^[3]:=nface^[5];
        nface^[4]:=nface^[6];
        nface^[5]:=nface^[7];
        Inc(collapseFlag);
      end
      else if  nface^[1]=nface^[2] then begin
        nface^[0]:=nface^[0];
        nface^[1]:=nface^[2];
        nface^[2]:=nface^[3];
        nface^[3]:=nface^[4];
        nface^[4]:=nface^[6];
        nface^[5]:=nface^[7];
        Inc(collapseFlag);
      end
      else if nface^[2]=nface^[3] then begin
        nface^[3]:=nface^[4];
        nface^[4]:=nface^[5];
        nface^[5]:=nface^[7];
        Inc(collapseFlag);
      end
      else if(nface^[3]=nface^[0]) then begin
        nface^[3]:=nface^[4];
        nface^[4]:=nface^[5];
        nface^[5]:=nface^[6];
        Inc(collapseFlag);
      end;
      if collapseFlag>1 then Exit(0);
      if collapseFlag=1 then Exit(6);
      Exit(8);
    end; // ecPenta15

    ecTetra10: begin
      case face of

        0: begin
          PInteger(nface)^:=ElEnqire[el].Node[0]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[2]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[1]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[6]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[5]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[4];
        end;

        1: begin
          PInteger(nface)^:=ElEnqire[el].Node[0]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[1]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[3]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[4]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[8]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[7];
        end;

        2: begin
          PInteger(nface)^:=ElEnqire[el].Node[1]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[2]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[3]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[5]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[9]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[8];
        end;

        3: begin
          PInteger(nface)^:=ElEnqire[el].Node[2]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[0]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[3]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[6]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[7]; Inc(PInteger(nface));
          PInteger(nface)^:=ElEnqire[el].Node[9];
        end;
      end; // case face
      Exit(6);
    end;  // ecTetra10

    ecTria3: begin
      case face of
        0: Exit(0);

        1: begin
          for i:=0 to 2 do
            nface^[i]:=ElEnqire[el].Node[i];
          Exit(3);
        end;

        2: begin
          for i:=0 to 1 do
            nface^[i]:=ElEnqire[el].Node[i];
        end;

        3: begin
          nface^[0]:=ElEnqire[el].Node[1];
          nface^[1]:=ElEnqire[el].Node[2];
        end;

        4: begin
          nface^[0]:=ElEnqire[el].Node[2];
          nface^[1]:=ElEnqire[el].Node[0];
        end;
      end; // case face
      Exit(2);
    end; // ecTria3

    ecTria6: begin
      case face of

        0: Exit(0);

        1: begin
          for i:=0 to 5 do
            nface^[i]:=ElEnqire[el].Node[i];
          Exit(6);
        end;

        2: begin
          for i:=0 to 1 do
            nface^[i]:=ElEnqire[el].Node[i];
          nface^[2]:=ElEnqire[el].Node[3];
        end;

        3: begin
          nface^[0]:=ElEnqire[el].Node[1];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[2]:=ElEnqire[el].Node[4];
        end;

        4: begin
          nface^[0]:=ElEnqire[el].Node[2];
          nface^[1]:=ElEnqire[el].Node[0];
          nface^[2]:=ElEnqire[el].Node[5];
        end;
      end; // case face
      Exit(3);
    end; // ecTria6

    ecQuad4: begin
      case face of

        0: Exit(0);

        1: begin
          for i:=0 to 3 do
            nface^[i]:=ElEnqire[el].Node[i];
          Exit(4);
        end;

        2: begin
          for i:=0 to 1 do
            nface^[i]:=ElEnqire[el].Node[i];
        end;

        3: begin
          nface^[0]:=ElEnqire[el].Node[1];
          nface^[1]:=ElEnqire[el].Node[2];
        end;

        4: begin
          nface^[0]:=ElEnqire[el].Node[2];
          nface^[1]:=ElEnqire[el].Node[3];
        end;

        5: begin
          nface^[0]:=ElEnqire[el].Node[3];
          nface^[1]:=ElEnqire[el].Node[0];
        end;

      end; // case face
      Exit(2);
    end; // ecQuad4

    ecQuad8: begin
      case face of

        0: Exit(0);

        1: begin
          for i:=0 to 8 do
            nface^[i]:=ElEnqire[el].Node[i];
          Exit(8);
        end;

        2: begin
          for i:=0 to 1 do
            nface^[i]:=ElEnqire[el].Node[i];
          nface^[2]:=ElEnqire[el].Node[4];
        end;

        3: begin
          nface^[0]:=ElEnqire[el].Node[1];
          nface^[1]:=ElEnqire[el].Node[2];
          nface^[2]:=ElEnqire[el].Node[5];
        end;

        4: begin
          nface^[0]:=ElEnqire[el].Node[2];
          nface^[1]:=ElEnqire[el].Node[3];
          nface^[2]:=ElEnqire[el].Node[6];
        end;

        5: begin
          nface^[0]:=ElEnqire[el].Node[3];
          nface^[1]:=ElEnqire[el].Node[0];
          nface^[2]:=ElEnqire[el].Node[7];
        end;

      end; // case face
      Exit(3);
    end; // ecQuad8

  end; // case ElEnqire[el].Category

  result:=0;
end;

procedure TInpFile.CommonEdge2(a, b: PIntegerArray);
begin
  case ElEnqire[a^[0]].Category of
    ecHexa8: begin
      SetLength(Edges,Summen.g+1);
      case a^[1] of
        0: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end; //2
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //3
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end; //4
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end; //5
        end; //0

        1: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //2
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[5];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end; //3
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end; //4
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[7];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end; //5
        end; //1

        2: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //1
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //3
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end; //5
        end; //2

        3: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[5];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end; //1
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //2
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end; //4
        end; //3

        4: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end; //1
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end; //3
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end; //5
        end; //4

        5: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end; //1
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end; //2
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end; //4
        end; //5
      end; // case a^[1]
    end; // ecHexa8

    ecPenta6: begin
      SetLength(Edges,Summen.g+1);
      case a^[1] of
        0: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end; //2
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //3
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end; //4
        end; //0

        1: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end; //2
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //3
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //4
        end; //1

        2: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end; //1
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end; //3
        end; //2

        3: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //1
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end; //2
        end; //3

        4: case b^[1] of
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //1
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end; //2
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end; //3
        end; //4
      end; // case a^[1]
    end; // ecPenta6

    ecTetra4: begin
      SetLength(Edges,Summen.g+1);
      case a^[1] of
        0: case b^[1] of
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end; //1
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //2
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end; //3
        end; //0

        1: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end; //0
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end; //2
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end; //3
        end; //1

        2: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end; //1
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //3
        end; //2

        3: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //0
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end; //1
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end; //2
        end; //3
      end; // case a^[1]
    end; // ecTetra4

    ecHexa20: begin
      SetLength(Edges,Summen.g+2);
      case a^[1] of
        0: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[8];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[8];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[9];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[9];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[10];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[10];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end;
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[11];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[11];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end;
        end; //0

        1: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[16];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[16];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[5];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[17];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[17];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end;
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[18];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[18];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end;
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[7];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[19];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[19];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end;
        end; //1

        2: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[8];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[8];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[16];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[16];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[13];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[13];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[12];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[12];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end;
        end; //2

        3: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[9];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[9];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[5];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[17];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[17];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end;
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[13];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[13];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[14];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[14];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end;
        end; //3

        4: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[10];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[10];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[18];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[18];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[14];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[14];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
          end;
          5: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[15];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[15];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end;
        end; //4

        5: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[11];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[11];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[19];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[19];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end;
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[12];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[12];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end;
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[15];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[15];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
          end;
        end; //5
      end; // case a^[1]
    end; // ecHexa20

    ecPenta15: begin
      SetLength(Edges,Summen.g+2);
      case a^[1] of
        0: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[7];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[8];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[8];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end;
        end; //0

        1: case b^[1] of
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[12];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[12];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[13];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[13];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
          4: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[14];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[14];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
        end; //1

        2: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[12];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[12];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[10];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[10];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end;
        end; //2

        3: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[7];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[13];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[13];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[10];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[10];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
          end;
        end; //3

        4: case b^[1] of
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[14];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[14];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[9];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[9];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[11];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[11];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
          end;
        end; //4
      end; // case a^[1]
    end; //ecPenta15

    ecTetra10: begin
      SetLength(Edges,Summen.g+2);
      case a^[1] of
        0: case b^[1] of
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end;
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[5];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[2];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end;
        end; //0

        1: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[4];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[4];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[1];
            Inc(Summen.g);
          end;
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[8];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[8];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[7];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[0];
            Inc(Summen.g);
          end;
        end; //1

        2: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[5];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[5];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[1];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[8];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[8];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end;
          3: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[9];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[9];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
        end; //2
        3: case b^[1] of
          0: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[6];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[6];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
          1: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[0];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[7];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[7];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[3];
            Inc(Summen.g);
          end;
          2: begin
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[3];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[9];
            Inc(Summen.g);
            Edges[Summen.g].p1:=ElEnqire[a^[0]].Node[9];
            Edges[Summen.g].p2:=ElEnqire[a^[0]].Node[2];
            Inc(Summen.g);
          end;
        end; //3
      end; //case a^[1]
    end; //ecTetra10
  end; // case ElEnqire[a^[0]].Category
end;

procedure TInpFile.SelectDisplayFaces;
const
  MAX_NODE_NR = 2100000000;
var
  size: Cardinal;
  e,i,j,nfaces,nface,
  NumFaces,NumDisplayFaces: Integer;
  pfaces,pCurrentFace,pFaceN,
  pFaceN1,pmax,pmaxd: PInteger;
  flag: TElementCategory;
begin
  flag:=ecUnknown;
  nfaces:=0;
  NumFaces:=0;
  NumDisplayFaces:=0;
  SetLength(Faces,Summen.emax+1);
  for i:=0 to Summen.emax-1 do begin
    FillChar(Faces[i],SizeOf(TFace),0);
    for j:=0 to 5 do
      Faces[i].Index[j]:=-1;
  end;

  size:=Max((Summen.e*36+6)*SizeOf(Integer),1920);
  pfaces:=AllocMem(size);
  pCurrentFace:=pfaces;

  for e:=0 to Summen.e-1 do begin
    j:=ElEnqire[e].Number;
    case ElEnqire[j].Category of
      ecTria3: nfaces:=5;
      ecTria6: begin
        ElEnqire[j].Category:=ecTria3;
        flag:=ecTria6;
        nfaces:=5;
      end;
      ecQuad4: nfaces:=6;
      ecQuad8: begin
        ElEnqire[j].Category:=ecQuad4;
        flag:=ecQuad8;
        nfaces:=6;
      end;
      ecTetra4: nfaces:=4;
      ecTetra10: begin
        ElEnqire[j].Category:=ecTetra4;
        flag:=ecTetra10;
        nfaces:=4;
      end;
      ecHexa8: nfaces:=6;
      ecHexa20: begin
        ElEnqire[j].Category:=ecHexa8;
        flag:=ecHexa20;
        nfaces:=6;
      end;
      ecPenta6: nfaces:=5;
      ecPenta15: begin
        ElEnqire[j].Category:=ecPenta6;
        flag:=ecPenta15;
        nfaces:=5;
      end;
      ecSeg2: begin
        if Summen.f>Summen.emax then
          SetLength(Faces,Summen.f+1);
        Faces[Summen.f].Category:=ecSeg2;
        Faces[Summen.f].Node[0]:=ElEnqire[j].Node[0];
        Faces[Summen.f].Node[1]:=ElEnqire[j].Node[1];
        Faces[Summen.f].ElemNumber:=j;
        Faces[Summen.f].Group:=ElEnqire[j].Group;
        Faces[Summen.f].Mat:=0;
        Faces[Summen.f].Number:=0;
        Faces[Summen.f].Side:=nil;
        Faces[j].Index[Faces[Summen.f].Number]:=Summen.f;
        Inc(Summen.f);
        nfaces:=0;
      end;
      ecSeg3: begin
        if Summen.f>Summen.emax then
           SetLength(Faces,Summen.f+1);
        Faces[Summen.f].Category:=ecSeg3;
        Faces[Summen.f].Node[0]:=ElEnqire[j].Node[0];
        Faces[Summen.f].Node[1]:=ElEnqire[j].Node[1];
        Faces[Summen.f].Node[2]:=ElEnqire[j].Node[2];
        Faces[Summen.f].ElemNumber:=j;
        Faces[Summen.f].Group:=ElEnqire[j].Group;
        Faces[Summen.f].Mat:=0;
        Faces[Summen.f].Number:=0;
        Faces[Summen.f].Side:=nil;
        Faces[j].Index[Faces[Summen.f].Number]:=Summen.f;
        Inc(Summen.f);
        nfaces:=0;
      end; // ecSeg3
    end; // case

    for i:=0 to nfaces-1 do begin
      nface:=GetElemFaceNodes(j,i,PIntegerArray(pCurrentFace));
      if nface<4 then begin
        if nface=0 then continue;
        if nface=2 then
          (pCurrentFace+2)^:=MAX_NODE_NR-1;
        (pCurrentFace+3)^:=MAX_NODE_NR;
      end;
      VdSort(PIntegerArray(pCurrentFace));
      pCurrentFace+=4;
      pCurrentFace^:=j;
      Inc(pCurrentFace);
      pCurrentFace^:=i;
      Inc(pCurrentFace);
    end;
    if flag<>ecUnknown then begin
      ElEnqire[j].Category:=flag;
      flag:=ecUnknown;
    end;
  end; // for e

  numFaces:=Integer(pCurrentFace-pfaces) div 6;
  pmax:=pCurrentFace;

  QSort(pfaces,numFaces,6*SizeOf(Integer),@CompareFaces4);

  pCurrentFace:=pfaces;
  pFaceN:=pfaces;
  pFaceN1:=pfaces+6;

  while pFaceN<pmax do begin
    if CompareFaces4(pFaceN,pFaceN1)=0 then begin
      pFaceN+=12;
      pFaceN1+=12;
    end
    else if ((pFaceN^=(pFaceN+1)^) and ((pFaceN+2)^=(pFaceN+3)^)) then begin
      pFaceN+=6;
      pFaceN1+=6;
    end
    else begin
      // elem nr
      (pCurrentFace)^:=(pFaceN+4)^;
      // face nr
      (pCurrentFace+1)^:=(pFaceN+5)^;

      // generate the face
      if Summen.f>Summen.emax then
        SetLength(Faces,Summen.f+1);

      nface:=GetElemFaceNodes((pFaceN+4)^,(pFaceN+5)^,
        PIntegerArray(@Faces[Summen.f].Node[0]));

      case nface of

        2: begin
          Faces[Summen.f].Category:=ecSeg2;
          SetLength(Edges,Summen.g+1);
          Edges[Summen.g].p1:=Faces[Summen.f].Node[0];
          Edges[Summen.g].p2:=Faces[Summen.f].Node[1];
          Inc(Summen.g);
        end;

        3: begin
          if ElEnqire[(pFaceN+4)^].Category in [ecTria6..ecQuad8,ecSeg2,ecSeg3] then begin
            Faces[Summen.f].Category:=ecSeg3;
            SetLength(Edges,Summen.g+2);
            Edges[Summen.g].p1:=Faces[Summen.f].Node[0];
            Edges[Summen.g].p2:=Faces[Summen.f].Node[2];
            Inc(Summen.g);
            Edges[Summen.g].p1:=Faces[Summen.f].Node[2];
            Edges[Summen.g].p2:=Faces[Summen.f].Node[1];
            Inc(Summen.g);
          end
          else
            Faces[Summen.f].Category:=ecTria3;
        end;

        4: Faces[Summen.f].Category:=ecQuad4;
        6: Faces[Summen.f].Category:=ecTria6;
        8: Faces[Summen.f].Category:=ecQuad8;
      end; // case nface

      Faces[Summen.f].ElemNumber:=(pFaceN+4)^;
      Faces[Summen.f].Group:=ElEnqire[(pFaceN+4)^].Group;
      Faces[Summen.f].Mat:=0;
      Faces[Summen.f].Number:=(pFaceN+5)^;
      Faces[Summen.f].Side:=nil;
      Faces[Faces[Summen.f].ElemNumber].Index[Faces[Summen.f].Number]:=Summen.f;
      Inc(Summen.f);

      pFaceN+=6;
      pFaceN1+=6;
      pCurrentFace+=2;
      numDisplayFaces+=1;
    end;
    if pFaceN1>pmax then
      pFaceN1-=12;
  end;
  pmaxd:=pCurrentFace-2;
  QSort(pfaces,numDisplayFaces,2*SizeOf(Integer),@CompareElnum1);

  pFaceN:=pfaces;
  pFaceN1:=pfaces+2;

  while pFaceN<pmaxd do begin
    if pFaceN^=pFaceN1^ then begin
      CommonEdge2(PIntegerArray(pFaceN),PIntegerArray(pFaceN1));
      pFaceN1+=2;
      if pFaceN1>pmaxd then begin
        pFaceN+=2;
        pFaceN1:=pFaceN+2;
      end;
    end else begin
      pFaceN+=2;
      pFaceN1:=pFaceN+2;
    end;
  end;
  FreeMem(pfaces);
end;

procedure TInpFile.MakeSurfaces;
var
  i: Integer;
begin
  Summen.f:=0;
  Summen.g:=0;
  SelectDisplayFaces;
  for i:=0 to Summen.f-1 do begin
    SetLength(Faces[i].Side,INP_NUMBER_OF_FACES[Faces[i].Category]+1);
  end;
  GetFaceNormales;
end;



procedure TInpFile.InitElements;
var
  i,j,n,n1,n2,ipuf: integer;
  c: TElementCategory;
begin
  // flag the nodes
  for i:=0 to Summen.n-1 do
    Nodes[Nodes[i].Number].Flag:=0;

  Summen.orignmax:=Summen.nmax;
  Summen.orign:=Summen.n;
  Summen.olc:=Summen.l;

  SetLength(ElEnqire,Summen.emax+1);

  for i:=0 to Summen.e-1 do begin
    j:=Elements[i].Number;
    c:=Elements[i].Category;
    ElEnqire[i].Number:=j;
    ElEnqire[j].Attr:=Elements[i].Attr;
    ElEnqire[j].Category:=c;
    ElEnqire[j].Group:=Elements[i].Group;
    ElEnqire[j].Mat:=Elements[i].Mat;
    ElEnqire[j].Side:=nil;

    ipuf:=INP_NUMBER_OF_NODES[c];
    if ipuf>0 then
      for n:=0 to ipuf-1 do begin
        if Elements[i].Node[n]>Summen.nmax then begin
          fLog.Add(sErrBadNodeRange,[Elements[i].Node[n],Elements[i].Number,Summen.nmax]);
          Abort;
        end;
        ElEnqire[j].Node[n]:=Elements[i].Node[n];
      end;
    SetLength(ElEnqire[j].Side,INP_NUMBER_OF_FACES[c]);

    case c of

      ecHexa20: begin
        SetLength(Nodes,Summen.nmax+7);
        for n:=0 to 2 do begin
          Inc(Summen.nmax);
          Nodes[Summen.n].Number:=Summen.nmax;
          Nodes[Summen.nmax].Flag:=1;
          Nodes[Summen.nmax].X:=-1/4*(
            Nodes[Elements[i].Node[0+n]].X+Nodes[Elements[i].Node[1+n]].X+
            Nodes[Elements[i].Node[5+n]].X+Nodes[Elements[i].Node[4+n]].X)+1/2*(
            Nodes[Elements[i].Node[8+n]].X+Nodes[Elements[i].Node[13+n]].X+
            Nodes[Elements[i].Node[16+n]].X+Nodes[Elements[i].Node[12+n]].X);
          Nodes[Summen.nmax].Y:=-1/4*(
            Nodes[Elements[i].Node[0+n]].Y+Nodes[Elements[i].Node[1+n]].Y+
            Nodes[Elements[i].Node[5+n]].Y+Nodes[Elements[i].Node[4+n]].Y)+1/2*(
            Nodes[Elements[i].Node[8+n]].Y+Nodes[Elements[i].Node[13+n]].Y+
            Nodes[Elements[i].Node[16+n]].Y+Nodes[Elements[i].Node[12+n]].Y);
          Nodes[Summen.nmax].Z:=-1/4*(
            Nodes[Elements[i].Node[0+n]].Z+Nodes[Elements[i].Node[1+n]].Z+
            Nodes[Elements[i].Node[5+n]].Z+Nodes[Elements[i].Node[4+n]].Z)+1/2*(
            Nodes[Elements[i].Node[8+n]].Z+Nodes[Elements[i].Node[13+n]].Z+
            Nodes[Elements[i].Node[16+n]].Z+Nodes[Elements[i].Node[12+n]].Z);
          ElEnqire[Elements[i].Number].Node[n+20]:=Nodes[Summen.n].Number;
          Inc(Summen.n);
        end;
        Inc(Summen.nmax);
        Nodes[Summen.n].Number:=Summen.nmax;
        Nodes[Summen.nmax].Flag:=1;
        Nodes[Summen.nmax].X:=-1/4*(
          Nodes[Elements[i].Node[3]].X+Nodes[Elements[i].Node[0]].X+
          Nodes[Elements[i].Node[4]].X+Nodes[Elements[i].Node[7]].X)+1/2*(
          Nodes[Elements[i].Node[11]].X+Nodes[Elements[i].Node[12]].X+
          Nodes[Elements[i].Node[19]].X+Nodes[Elements[i].Node[15]].X);
        Nodes[Summen.nmax].Y:=-1/4*(
          Nodes[Elements[i].Node[3]].Y+Nodes[Elements[i].Node[0]].Y+
          Nodes[Elements[i].Node[4]].Y+Nodes[Elements[i].Node[7]].Y)+1/2*(
          Nodes[Elements[i].Node[11]].Y+Nodes[Elements[i].Node[12]].Y+
          Nodes[Elements[i].Node[19]].Y+Nodes[Elements[i].Node[15]].Y);
        Nodes[Summen.nmax].Z:=-1/4*(
          Nodes[Elements[i].Node[3]].Z+Nodes[Elements[i].Node[0]].Z+
          Nodes[Elements[i].Node[4]].Z+Nodes[Elements[i].Node[7]].Z)+1/2*(
          Nodes[Elements[i].Node[11]].Z+Nodes[Elements[i].Node[12]].Z+
          Nodes[Elements[i].Node[19]].Z+Nodes[Elements[i].Node[15]].Z);
        ElEnqire[Elements[i].Number].Node[23]:=Nodes[Summen.n].Number;
        Inc(Summen.n);
        for n:=0 to 1 do begin
          Inc(Summen.nmax);
          Nodes[Summen.n].Number:=Summen.nmax;
          Nodes[Summen.nmax].Flag:=1;
          n1:=n*4;
          n2:=n*8;
          Nodes[Summen.nmax].X:=-1/4*(
            Nodes[Elements[i].Node[0+n1]].X+Nodes[Elements[i].Node[1+n1]].X+
            Nodes[Elements[i].Node[2+n1]].X+Nodes[Elements[i].Node[3+n1]].X)+1/2*(
            Nodes[Elements[i].Node[8+n2]].X+Nodes[Elements[i].Node[9+n2]].X+
            Nodes[Elements[i].Node[10+n2]].X+Nodes[Elements[i].Node[11+n2]].X);
          Nodes[Summen.nmax].Y:=-1/4*(
            Nodes[Elements[i].Node[0+n1]].Y+Nodes[Elements[i].Node[1+n1]].Y+
            Nodes[Elements[i].Node[2+n1]].Y+Nodes[Elements[i].Node[3+n1]].Y)+1/2*(
            Nodes[Elements[i].Node[8+n2]].Y+Nodes[Elements[i].Node[9+n2]].Y+
            Nodes[Elements[i].Node[10+n2]].Y+Nodes[Elements[i].Node[11+n2]].Y);
          Nodes[Summen.nmax].Z:=-1/4*(
            Nodes[Elements[i].Node[0+n1]].Z+Nodes[Elements[i].Node[1+n1]].Z+
            Nodes[Elements[i].Node[2+n1]].Z+Nodes[Elements[i].Node[3+n1]].Z)+1/2*(
            Nodes[Elements[i].Node[8+n2]].Z+Nodes[Elements[i].Node[9+n2]].Z+
            Nodes[Elements[i].Node[10+n2]].Z+Nodes[Elements[i].Node[11+n2]].Z);
          ElEnqire[Elements[i].Number].Node[n+24]:=Nodes[Summen.n].Number;
          Inc(Summen.n);
        end;
      end; // ecHexa20

      ecPenta15: begin
        SetLength(Nodes,Summen.nmax+6);
        for n:=0 to 1 do begin
          Inc(Summen.nmax);
          Nodes[Summen.n].Number:=Summen.nmax;
          Nodes[Summen.nmax].Flag:=1;
          Nodes[Summen.nmax].X:=-1/4*(
            Nodes[Elements[i].Node[0+n]].X+Nodes[Elements[i].Node[1+n]].X+
            Nodes[Elements[i].Node[4+n]].X+Nodes[Elements[i].Node[3+n]].X)+1/2*(
            Nodes[Elements[i].Node[6+n]].X+Nodes[Elements[i].Node[10+n]].X+
            Nodes[Elements[i].Node[12+n]].X+Nodes[Elements[i].Node[9+n]].X);
          Nodes[Summen.nmax].Y:=-1/4*(
            Nodes[Elements[i].Node[0+n]].Y+Nodes[Elements[i].Node[1+n]].Y+
            Nodes[Elements[i].Node[4+n]].Y+Nodes[Elements[i].Node[3+n]].Y)+1/2*(
            Nodes[Elements[i].Node[6+n]].Y+Nodes[Elements[i].Node[10+n]].Y +
            Nodes[Elements[i].Node[12+n]].Y+Nodes[Elements[i].Node[9+n]].Y);
          Nodes[Summen.nmax].Z:=-1/4*(
            Nodes[Elements[i].Node[0+n]].Z+Nodes[Elements[i].Node[1+n]].Z+
            Nodes[Elements[i].Node[4+n]].Z+Nodes[Elements[i].Node[3+n]].Z)+1/2*(
            Nodes[Elements[i].Node[6+n]].Z+Nodes[Elements[i].Node[10+n]].Z+
            Nodes[Elements[i].Node[12+n]].Z+Nodes[Elements[i].Node[9+n]].Z);
          ElEnqire[Elements[i].Number].Node[n+15]:=Nodes[Summen.n].Number;
          Inc(Summen.n);
        end;
        Inc(Summen.nmax);
        Nodes[Summen.n].Number:=Summen.nmax;
        Nodes[Summen.nmax].Flag:=1;
        Nodes[Summen.nmax].X:=-1/4*(
          Nodes[Elements[i].Node[2]].X+Nodes[Elements[i].Node[0]].X+
          Nodes[Elements[i].Node[3]].X+Nodes[Elements[i].Node[5]].X)+1/2*(
          Nodes[Elements[i].Node[8]].X+Nodes[Elements[i].Node[9]].X+
          Nodes[Elements[i].Node[14]].X+Nodes[Elements[i].Node[11]].X);
        Nodes[Summen.nmax].Y:=-1/4*(
          Nodes[Elements[i].Node[2]].Y+Nodes[Elements[i].Node[0]].Y+
          Nodes[Elements[i].Node[3]].Y+Nodes[Elements[i].Node[5]].Y)+1/2*(
          Nodes[Elements[i].Node[8]].Y+Nodes[Elements[i].Node[9]].Y+
          Nodes[Elements[i].Node[14]].Y+Nodes[Elements[i].Node[11]].Y);
        Nodes[Summen.nmax].Z:=-1/4*(
          Nodes[Elements[i].Node[2]].Z+Nodes[Elements[i].Node[0]].Z+
          Nodes[Elements[i].Node[3]].Z+Nodes[Elements[i].Node[5]].Z)+1/2*(
          Nodes[Elements[i].Node[8]].Z+Nodes[Elements[i].Node[9]].Z+
          Nodes[Elements[i].Node[14]].Z+Nodes[Elements[i].Node[11]].Z);
        ElEnqire[Elements[i].Number].Node[17]:=Nodes[Summen.n].Number;
        Inc(Summen.n);
        Inc(Summen.nmax);
        Nodes[Summen.n].Number:=Summen.nmax;
        Nodes[Summen.nmax].Flag:=1;
        Nodes[Summen.nmax].X:=-1/4*(
          Nodes[Elements[i].Node[0]].X+Nodes[Elements[i].Node[2]].X+
          Nodes[Elements[i].Node[1]].X+Nodes[Elements[i].Node[0]].X)+1/2*(
          Nodes[Elements[i].Node[8]].X+Nodes[Elements[i].Node[7]].X+
          Nodes[Elements[i].Node[6]].X+Nodes[Elements[i].Node[0]].X);
        Nodes[Summen.nmax].Y:=-1/4*(
          Nodes[Elements[i].Node[0]].Y+Nodes[Elements[i].Node[2]].Y+
          Nodes[Elements[i].Node[1]].Y+Nodes[Elements[i].Node[0]].Y)+1/2*(
          Nodes[Elements[i].Node[8]].Y+Nodes[Elements[i].Node[7]].Y+
          Nodes[Elements[i].Node[6]].Y+Nodes[Elements[i].Node[0]].Y);
        Nodes[Summen.nmax].Z:=-1/4*(
          Nodes[Elements[i].Node[0]].Z+Nodes[Elements[i].Node[2]].Z+
          Nodes[Elements[i].Node[1]].Z+Nodes[Elements[i].Node[0]].Z)+1/2*(
          Nodes[Elements[i].Node[8]].Z+Nodes[Elements[i].Node[7]].Z+
          Nodes[Elements[i].Node[6]].Z+Nodes[Elements[i].Node[0]].Z);
        ElEnqire[Elements[i].Number].Node[18]:=Nodes[Summen.n].Number;
        Inc(Summen.n);
        Inc(Summen.nmax);
        Nodes[Summen.n].Number:=Summen.nmax;
        Nodes[Summen.nmax].Flag:=1;
        Nodes[Summen.nmax].X:=-1/4*(
          Nodes[Elements[i].Node[3]].X+Nodes[Elements[i].Node[4]].X+
          Nodes[Elements[i].Node[5]].X+Nodes[Elements[i].Node[3]].X)+1/2*(
          Nodes[Elements[i].Node[12]].X+Nodes[Elements[i].Node[13]].X+
          Nodes[Elements[i].Node[14]].X+Nodes[Elements[i].Node[3]].X);
        Nodes[Summen.nmax].Y:=-1/4* (
          Nodes[Elements[i].Node[3]].Y+Nodes[Elements[i].Node[4]].Y+
          Nodes[Elements[i].Node[5]].Y+Nodes[Elements[i].Node[3]].Y)+1/2*(
          Nodes[Elements[i].Node[12]].Y+Nodes[Elements[i].Node[13]].Y+
          Nodes[Elements[i].Node[14]].Y+Nodes[Elements[i].Node[3]].Y);
        Nodes[Summen.nmax].Z:=-1/4*(
          Nodes[Elements[i].Node[3]].Z+Nodes[Elements[i].Node[4]].Z+
          Nodes[Elements[i].Node[5]].Z+Nodes[Elements[i].Node[3]].Z)+1/2*(
          Nodes[Elements[i].Node[12]].Z+Nodes[Elements[i].Node[13]].Z+
          Nodes[Elements[i].Node[14]].Z+Nodes[Elements[i].Node[3]].Z);
        ElEnqire[Elements[i].Number].Node[19]:=Nodes[Summen.n].Number;
        Inc(Summen.n);
      end; // ecPenta15

      ecQuad8: begin
        SetLength(Nodes,Summen.nmax+2);
        j:=ElEnqire[i].Number;
        Inc(Summen.nmax);
        Nodes[Summen.n].Number:=Summen.nmax;
        Nodes[Summen.nmax].Flag:=1;
        Nodes[Summen.nmax].X:=-1/4*(
          Nodes[ElEnqire[j].Node[0]].X+Nodes[ElEnqire[j].Node[1]].X+
          Nodes[ElEnqire[j].Node[3]].X+Nodes[ElEnqire[j].Node[2]].X)+1/2*(
          Nodes[ElEnqire[j].Node[4]].X+Nodes[ElEnqire[j].Node[6]].X+
          Nodes[ElEnqire[j].Node[7]].X+Nodes[ElEnqire[j].Node[5]].X);
        Nodes[Summen.nmax].Y:=-1/4*(
          Nodes[ElEnqire[j].Node[0]].Y+Nodes[ElEnqire[j].Node[1]].Y+
          Nodes[ElEnqire[j].Node[3]].Y+Nodes[ElEnqire[j].Node[2]].Y)+1/2*(
          Nodes[ElEnqire[j].Node[4]].Y+Nodes[ElEnqire[j].Node[6]].Y+
          Nodes[ElEnqire[j].Node[7]].Y+Nodes[ElEnqire[j].Node[5]].Y);
        Nodes[Summen.nmax].Z:=-1/4*(
          Nodes[ElEnqire[j].Node[0]].Z+Nodes[ElEnqire[j].Node[1]].Z+
          Nodes[ElEnqire[j].Node[3]].Z+Nodes[ElEnqire[j].Node[2]].Z)+1/2*(
          Nodes[ElEnqire[j].Node[4]].Z+Nodes[ElEnqire[j].Node[6]].Z+
          Nodes[ElEnqire[j].Node[7]].Z+Nodes[ElEnqire[j].Node[5]].Z);
        ElEnqire[j].Node[8]:=Nodes[Summen.n].Number;
        Inc(Summen.n);
      end; // ecQuad8

    end;  // case
  end; // for
end;

procedure TInpFile.CalcScale;
var
  i: Integer;
begin
  Scale.XMax:=-10000000;
  Scale.XMin:=10000000;
  Scale.YMax:=-10000000;
  Scale.YMin:=10000000;
  Scale.ZMax:=-10000000;
  Scale.ZMin:=10000000;
  for i:=0 to Summen.n-1 do begin
    if Nodes[i].Flag<>0 then
      continue;
    Scale.XMax:=Max(Scale.XMax,Nodes[i].X);
    Scale.XMin:=Min(Scale.XMin,Nodes[i].X);
    Scale.YMax:=Max(Scale.YMax,Nodes[i].Y);
    Scale.YMin:=Min(Scale.YMin,Nodes[i].Y);
    Scale.ZMax:=Max(Scale.ZMax,Nodes[i].Z);
    Scale.ZMin:=Min(Scale.ZMin,Nodes[i].Z);
  end;

  Scale.X:=(Scale.XMax+Scale.XMin)/2;
  Scale.Y:=(Scale.YMax+Scale.YMin)/2;
  Scale.Z:=(Scale.ZMax+Scale.ZMin)/2;
  Scale.XMax:=Scale.XMax-Scale.X;
  Scale.YMax:=Scale.YMax-Scale.Y;
  Scale.ZMax:=Scale.ZMax-Scale.Z;
  Scale.XMin:=Scale.XMin-Scale.X;
  Scale.YMin:=Scale.YMin-Scale.Y;
  Scale.ZMin:=Scale.ZMin-Scale.Z;
  if Scale.XMax<-Scale.XMin then Scale.XMax:=-Scale.XMin;
  if Scale.YMax<-Scale.YMin then Scale.YMax:=-Scale.YMin;
  if Scale.ZMax<-Scale.ZMin then Scale.ZMax:=-Scale.ZMin;
  Scale.W:=Scale.XMax;
  if Scale.W<Scale.YMax then Scale.W:=Scale.YMax;
  if Scale.W<Scale.ZMax then Scale.W:=Scale.ZMax;

  Scale.W/=0.4;
  if Scale.W<=0.0 then Scale.W:=1;
end;

procedure TInpFile.ScaleNodes;
var
  i,j,n: Integer;
begin
  n:=0;
  for i:=0 to Summen.n-1 do begin
    j:=Nodes[i].Number;
    Nodes[j].X:=(Nodes[j].X-Scale.X)/Scale.W;
    Nodes[j].Y:=(Nodes[j].Y-Scale.Y)/Scale.W;
    Nodes[j].Z:=(Nodes[j].Z-Scale.Z)/Scale.W;
  end;
end;

procedure TInpFile.CalcStat;
var
  i,j,n: Integer;
  f: TElface;
begin
  // Correct nodes count
  n:=0;
  for i:=0 to Summen.n-1 do
    if Nodes[Nodes[i].Number].Flag=0 then
      Inc(n);
   Summen.n:=n;
  // calc faces count
  for i:=0 to Summen.sets-1 do begin
    for j:=0 to Sets[i].NumElfaces-1 do begin
      f:=Sets[i].Elfaces[j];
      if ElEnqire[f.e].Category in [ecTria3..ecQuad8] then begin
        if ElEnqire[f.e].Attr>3 then begin
          Inc(Sets[i].Elfaces[j].f);
          if ElEnqire[f.e].Category in [ecTria3,ecTria6] then begin
            if f.f>4 then
              Sets[i].Elfaces[j].f:=1;
          end else begin
            if f.f>5 then
              Sets[i].Elfaces[j].f:=1;
          end;
        end else begin
          Dec(Sets[i].Elfaces[j].f);
          if f.f<=0 then
            Sets[i].Elfaces[j].f:=1;
        end;
      end;
      if Faces[f.e].Index[f.f]=-1 then begin
        fLog.Add(sErrFaceNoExst,[Sets[i].Name,f.f+1,f.e]);
      end
      else
        Inc(Sets[i].NumFaces);
    end;
  end;
end;


function TInpFile.GetSetIndex(const s: string): Integer;
var
  i: integer;
begin
  if Summen.sets=0 then
    Exit(-1);
  for i:=0 to Summen.sets-1 do
    if AnsiSameStr(s,Sets[i].Name) then
      Exit(i);
  result:=-1; // not found
end;

function TInpFile.GetShapeIndex(const s: string): Integer;
begin
  result:=-1; // not found
end;

function TInpFile.AddToSet(const nset: Integer; const typ: string;
  const number: Integer): Integer;

  function Iinsert(var a: TIntegerDynArray; n, x0: Integer): Integer;
  var
    i,ii,m,n1,n2: Integer;
  begin
    i:=0;
    // if x0 is lower than the first elem
    if (n=0) or (x0<a[0]) then begin
      SetLength(a,n+1);
      for ii:=n downto 1 do
        a[ii]:=a[ii-1];
      a[0]:=x0;
      Inc(n);
    end
    // if x0 is higher than the last elem
    else if x0>a[n-1] then begin
      SetLength(a,n+1);
      a[n]:=x0;
      Inc(n);
    end
    // search the intersection
    else begin
      n1:=0;
      n2:=n;
      for ii:=0 to n-1 do begin
        m:=(n2+n1) div 2;
        if x0>=a[m] then
          n1:=m;
        if x0<a[m] then
          n2:=m;
        if (n2-n1)=1 then
          break;
      end;
      i:=n1;
      if x0<>a[i] then begin
        // extend array by x0
        SetLength(a,n+1);
        ii:=n;
        while ii>i+1 do begin
          a[ii]:=a[ii-1];
          Dec(ii);
        end;
        a[i+1]:=x0;
        Inc(n);
      end;
    end;
    result:=n;
  end;

begin
  if (nset<0) or (nset>=Summen.sets) then begin
    fLog.Add(sErrSetIndxOut,[nset,Summen.sets-1]);
    Exit(-1);
  end;
  if Sets[nset].Name='' then begin
    fLog.Add(sErrSetUndef,[nset]);
    Exit(-1);
  end;
  if IsStringStarted(typ,'r') then begin
    if number<0 then begin
      fLog.Add(sErrSetNoExst,[number]);
      Exit(-1);
    end;
    Sets[nset].NumSets:=Iinsert(Sets[nset].Sets,Sets[nset].NumSets, number);
    Sets[number].NumSets:=Iinsert(Sets[number].Sets,Sets[number].NumSets,nset);
  end
  else if IsStringStarted(typ,'sh') or IsStringStarted(typ,'hh') then begin
    if number<0 then begin
      fLog.Add(sErrShpNoExst,[number]);
      Exit(-1);
    end;
    Sets[nset].NumShapes:=Iinsert(Sets[nset].Shapes,Sets[nset].NumShapes,number);
  end
  else if IsStringStarted(typ,'n') then
    Sets[nset].NumNodes:=Iinsert(Sets[nset].Nodes,Sets[nset].NumNodes,number)
  else if IsStringStarted(typ,'e') then
    Sets[nset].NumElements:=Iinsert(Sets[nset].Elements,Sets[nset].NumElements,number)
  else if IsStringStarted(typ,'f') then
    Sets[nset].NumFaces:=Iinsert(Sets[nset].Faces,Sets[nset].NumFaces,number)
  else if IsStringStarted(typ,'j') then begin
    SetLength(Sets[nset].Elfaces,Sets[nset].NumElfaces+1);
    Inc(Sets[nset].NumElfaces);
    Exit(Sets[nset].NumElfaces-1);
  end else begin
    fLog.Add(sWrnSetNoRec,[typ]);
    Exit(-1);
  end;
  result:=1;
end;

function TInpFile.SetupSetIndex(const s, typ: string): Integer;
var
  nset,number: Integer;
begin
  if Length(Trim(s))=0 then
    Exit(-1);
  number:=0;
  nset:=GetSetIndex(s);
  if nset=-1 then begin
    nset:=Summen.sets;
    Inc(Summen.sets);
    SetLength(Sets,Summen.sets);
    FillChar(Sets[nset],SizeOf(TSet),0);
    SetLength(Sets[nset].Nodes,1);
    SetLength(Sets[nset].Elements,1);
    SetLength(Sets[nset].Faces,1);
    SetLength(Sets[nset].Sets,1);
    SetLength(Sets[nset].Shapes,1);
    SetLength(Sets[nset].Elfaces,1);
    Sets[nset].Name:=s;
  end;
  if IsStringStarted(typ,'se') then
    number:=GetSetIndex(s)
  else if IsStringStarted(typ,'sh') then
    number:=GetShapeIndex(s)
  else if IsStringStarted(typ,'n') then
    number:=StrToIntDef(s,-1)
  else if IsStringStarted(typ,'e') then
    number:=StrToIntDef(s,-1)
  else if IsStringStarted(typ,'f') then
    number:=StrToIntDef(s,-1)
  else if IsStringStarted(typ,'i') then
    Exit(nset)
  else
    Exit(-1);
  if AddToSet(nset,typ,number)<0 then
    result:=-1
  else
    result:=nset;
end;


function TInpFile.GetLine(out line: string): TReadResult;
label
  NextLine;
begin
  NextLine:
  if fReader=nil then
    Exit(rrEnd);
  repeat
    if fReader.Eof then begin
      if fFileStack.Count>0 then begin
        fReader:=TFileReader(fFileStack.Pop);
        FreeAndNil(fReader);
        fReader:=TFileReader(fFileStack.Peek);
        goto NextLine;
      end
      else
        Exit(rrEnd);
    end;
    line:=fReader.ReadLine;
  until not (IsStringStarted(line,'**'));

  if IsStringStarted(line,'*') then begin
    if IsStringStarted(line,'*INCLUDE') then begin
      SetCurrentReader(ExtractArgumentValue(line,'INPUT'));
      goto NextLine;
    end
    else
      Exit(rrNextIns);
  end;
  result:=rrNormal;
end;

function TInpFile.ExtractNodes(var s: string): Boolean;
var
  arg: string;
  i,j,n,m,p,nset: Integer;
  res: TReadResult;
  line_list: TStrArray;
begin
  nset:=-1;
  arg:=ExtractArgumentValue(s,'NSET');
  if arg<>'' then begin
    nset:=GetSetIndex(arg);
    if nset<0 then
      nset:=SetupSetIndex(arg,'i');
  end;
  repeat
    res:=GetLine(s);
    if res in [rrEnd,rrNextIns] then begin
      if (nset>=0) and (nset<Length(Sets)) then begin
        Qsort(@Sets[nset].Nodes[0],Sets[nset].NumNodes,SizeOf(integer),@CompareInt);
        // erase multiple entities
        n:=0;
        for j:=1 to Sets[nset].NumNodes-1 do
          if Sets[nset].Nodes[n]<>Sets[nset].Nodes[j] then begin
            Inc(n);
            Sets[nset].Nodes[n]:=Sets[nset].Nodes[j];
          end;
        if Sets[nset].NumNodes>0 then
          Sets[nset].NumNodes:=n+1;
      end;
      case res of
        rrEnd: Exit(false);
        rrNextIns: break;
      end;
    end;

    line_list:=SplitString(s,',');
    if Length(line_list)=0 then
      continue;
    n:=StrToIntDef(line_list[0],0);
    p:=Length(Nodes);
    if n>=p then begin
      m:=n-p+1;
      SetLength(Nodes,p+m);
      for i:=p to p+m-1 do begin
        FillChar(Nodes[i],SizeOf(TNode),0);
        Nodes[i].Index:=-1;
      end;
    end;
    p:=Length(line_list);
    if p>1 then
      Nodes[n].X:=ParseNumberString(line_list[1]);
    if p>2 then
      Nodes[n].Y:=ParseNumberString(line_list[2]);
    if p>3 then
      Nodes[n].Z:=ParseNumberString(line_list[3]);

    if Nodes[n].Index<0 then begin
      Nodes[Summen.n].Number:=n;
      Nodes[n].Index:=Summen.n;
      Summen.nmax:=Max(Summen.nmax,n);
      Summen.nmin:=Min(Summen.nmin,n);
      Inc(Summen.n);
      if (nset>=0) and (nset<Length(Sets)) then begin
        SetLength(Sets[nset].Nodes,Sets[nset].NumNodes+1);
        Sets[nset].Nodes[Sets[nset].NumNodes]:=n;
        Inc(Sets[nset].NumNodes);
      end;
    end;
  until false;
  result:=true;
end;

procedure TInpFile.ExtractElement(const category: TElementCategory;
  const number: Integer; buff: TElementBuffer);
var
  node: TElementBuffer;
  group: Integer;
  attr: Integer;

  procedure _write;
  var
    i,j,m,p: Integer;
  begin
    p:=Length(Elements);
    if Summen.e>=p then begin
      m:=Summen.e-p+1;
      SetLength(Elements,p+m);
      for i:=p to p+m-1 do begin
        FillChar(Elements[i],SizeOf(TElement),0);
      end;
    end;
    p:=Summen.e;
    Elements[p].Number:=number;
    Summen.emax:=Max(Summen.emax,number);
    Summen.emin:=Min(Summen.emin,number);
    Elements[p].Group:=group;
    Elements[p].Category:=category;
    Elements[p].Mat:=1;
    Elements[p].Attr:=attr;

    for j:=0 to INP_NUMBER_OF_NODES[category]-1 do
      Elements[p].Node[j]:=node[j];
    Inc(Summen.etype[category]);

    Inc(Summen.e);
  end;

var
  i: Integer;
begin
  attr:=0;
  FillChar(node,SizeOf(TElementBuffer),0);
  case category of

    ecTria3: begin
      for i:=0 to 2 do
        node[i]:=buff[i];
      group:=7;
      _write;
    end;

    ecTria6: begin
      for i:=0 to 5 do
        node[i]:=buff[i];
      group:=8;
      _write;
    end;

    ecQuad4: begin
      for i:=0 to 3 do
        node[i]:=buff[i];
      group:=9;
      _write;
    end;

    ecQuad8: begin
      for i:=0 to 7 do
        node[i]:=buff[i];
      group:=10;
      _write;
    end;

    ecTetra4: begin
      for i:=0 to 3 do
        node[i]:=buff[i];
      group:=3;
      _write;
    end;

    ecTetra10: begin
      for i:=0 to 9 do
        node[i]:=buff[i];
      group:=6;
      _write;
    end;

    ecHexa8: begin
      for i:=0 to 7 do
        node[i]:=buff[i];
      group:=1;
      _write;
    end;

    ecHexa20: begin
      for i:=0 to 11 do
        buff[i+20]:=buff[i];
      for i:=0 to 3 do
        buff[i+32]:=buff[i+16];
      for i:=0 to 3 do
        buff[i+36]:=buff[i+12];
      for i:=0 to 19 do
        node[i]:=buff[i+20];
      group:=4;
      _write;
    end;

    ecPenta6: begin
      for i:=0 to 5 do
        node[i]:=buff[i];
      group:=2;
      _write;
    end;

    ecPenta15: begin
      for i:=0 to 8 do
        buff[i+20]:=buff[i];
      for i:=0 to 2 do
        buff[i+29]:=buff[i+12];
      for i:=0 to 2 do
        buff[i+32]:=buff[i+9];
      for i:=0 to 19 do
        node[i]:=buff[i+20];
      group:=5;
      _write;
    end;

    ecSeg2: begin
      for i:=0 to 1 do
        node[i]:=buff[i];
      group:=11;
      _write;
    end;

    ecSeg3: begin
      for i:=0 to 2 do
        node[i]:=buff[i];
      group:=11;
      _write;
    end;

  end;
end;


function TInpFile.ExtractElements(var s: string): Boolean;
var
  arg,setname: string;
  res: TReadResult;
  elm_type: TElementType;
  elm_category: TElementCategory;
  elm_2nd_line,isset: Boolean;
  line_list: TStrArray;
  el,i,j,m,p,nset,se: Integer;
  elm_nd: TElementBuffer;
begin
  nset:=-1;
  se:=-1;
  elm_category:=ecUnknown;
  elm_2nd_line:=false;
  arg:=ExtractArgumentValue(s,'TYPE');
  if arg<>'' then begin
    elm_type:=ExtractElementType(arg);
    elm_category:=ElementTypeToCategory(elm_type);
    setname:=Format('+%s',[arg]);
  end;

  arg:=ExtractArgumentValue(s,'ELSET');
  if arg<>'' then begin
    nset:=GetSetIndex(arg);
    if nset<0 then
      nset:=SetupSetIndex(arg,'i');
  end;
  if elm_category<>ecUnknown then
    se:=SetupSetIndex(setname,'i');

  repeat
    res:=GetLine(s);
    case res of
      rrEnd: Exit(false);
      rrNextIns: break;
    end;

    if elm_category<>ecUnknown then begin
      line_list:=SplitString(s,',');
      if not elm_2nd_line then begin
        el:=StrToIntDef(line_list[0],0);
        FillChar(elm_nd,SizeOf(elm_nd),0);
        p:=1;
        j:=0;
      end else
        p:=0;
      m:=Length(line_list)-1;
      for i:=p to m do begin
        elm_nd[j]:=StrToIntDef(line_list[i],0);
        Inc(j);
      end;
      if j<INP_NUMBER_OF_NODES[elm_category] then
        elm_2nd_line:=true
      else begin
        ExtractElement(elm_category,el,elm_nd);
        if (nset>=0) and (nset<Length(Sets)) then
          AddToSet(nset,'e',el);
        AddToSet(se,'e', el);
        elm_2nd_line:=false;
      end;
    end;

  until false;
  result:=true;
end;

function TInpFile.ExtractElset(var s: string): Boolean;
var
  i,j,eset,nset,na: Integer;
  arg: string;
  res: TReadResult;
  el: TElementBuffer;
  generate: Boolean;
  line_list: TStrArray;
begin
  nset:=-1;
  arg:=ExtractArgumentValue(s,'ELSET');
  if arg<>'' then begin
    nset:=GetSetIndex(arg);
    if nset<0 then
      nset:=SetupSetIndex(arg,'i');
  end;
  generate:=ArgumentNameExists(s,'GENERATE');

  repeat
    res:=GetLine(s);
    case res of
      rrEnd: Exit(false);
      rrNextIns: break;
    end;

    if generate then begin
      FillChar(el,SizeOf(TElementBuffer),0);
      el[2]:=1;
      line_list:=SplitString(s,',');
      na:=Length(line_list);
      if na>0 then
        el[0]:=StrToIntDef(line_list[0],0);
      if na>1 then
        el[1]:=StrToIntDef(line_list[1],0);
      if na>2 then
        el[2]:=StrToIntDef(line_list[2],0);
      if el[2]<>0 then begin
        AddToSet(nset,'e',el[0]);
        AddToSet(nset,'e',el[1]);
      end else begin
        i:=el[0];
        while i<=el[1] do begin
          AddToSet(nset,'e',i);
          i+=el[2];
        end;
      end;
    end // if generate
    else begin
      FillChar(el,SizeOf(TElementBuffer),0);
      line_list:=SplitString(s,',');
      na:=Length(line_list);
      for i:=0 to na-1 do begin
        el[0]:=StrToIntDef(line_list[i],0);
        if el[0]<>0 then
          AddToSet(nset,'e',el[0])
        else begin
          eset:=GetSetIndex(line_list[i]);
          if eset<0 then begin
            fLog.Add(sErrElSetNotDef,[line_list[i]]);
            Exit(false);
          end;
          for j:=0 to Sets[eset].NumElements-1 do
            AddToSet(nset,'e',Sets[eset].Elements[j]);
        end;
      end;
    end;
  until false;
  result:=true;
end;

function TInpFile.ExtractNset(var s: string): Boolean;
var
  i,j,nset,n,na: Integer;
  arg: string;
  res: TReadResult;
  nd: TNodeBuffer;
  generate: Boolean;
  line_list: TStrArray;
begin
  nset:=-1;
  arg:=ExtractArgumentValue(s,'NSET');
  if arg<>'' then begin
    nset:=GetSetIndex(arg);
    if nset<0 then
      nset:=SetupSetIndex(arg,'i');
  end;
  generate:=ArgumentNameExists(s,'GENERATE');

  repeat
    res:=GetLine(s);
    case res of
      rrEnd: Exit(false);
      rrNextIns: break;
    end;

    if generate then begin
      FillChar(nd,SizeOf(TNodeBuffer),0);
      nd[2]:=1;
      line_list:=SplitString(s,',');
      na:=Length(line_list);
      if na>0 then
        nd[0]:=StrToIntDef(line_list[0],0);
      if na>1 then
        nd[1]:=StrToIntDef(line_list[1],0);
      if na>2 then
        nd[2]:=StrToIntDef(line_list[2],0);
      if nd[2]<>0 then begin
        AddToSet(nset,'n',nd[0]);
        AddToSet(nset,'n',nd[1]);
      end else begin
        i:=nd[0];
        while i<=nd[1] do begin
          AddToSet(nset,'n',i);
          i+=nd[2];
        end;
      end;
    end // if generate
    else begin
      FillChar(nd,SizeOf(TNodeBuffer),0);
      line_list:=SplitString(s,',');
      na:=Length(line_list);
      for i:=0 to na-1 do begin
        nd[0]:=StrToIntDef(line_list[i],0);
        if nd[0]<>0 then
          AddToSet(nset,'n',nd[0])
        else begin
          n:=GetSetIndex(line_list[i]);
          if n<0 then begin
            fLog.Add(sErrNSetNotDef,[line_list[i]]);
            Exit(false);
          end;
          for j:=0 to Sets[n].NumNodes-1 do
            AddToSet(nset,'n',Sets[n].Nodes[j]);
        end;
      end;
    end;

  until false;
  result:=true;
end;

function TInpFile.ExtractSurface(var s: string): Boolean;
var
  i,j,el,eset,nset,n,
  face,facei: Integer;
  arg,typ,z: string;
  line_list: TStrArray;
  res: TReadResult;
begin
  nset:=-1;
  arg:=ExtractArgumentValue(s,'NAME');
  if arg='' then
    arg:='+sur';
  nset:=GetSetIndex(arg);
  if nset<0 then
    nset:=SetupSetIndex(arg,'i');

  typ:=ExtractArgumentValue(s,'TYPE');
  if typ='' then
    typ:='ELEM';

  if AnsiSameText(typ,'NODE') then begin
    repeat
      res:=GetLine(s);
      case res of
        rrEnd: Exit(false);
        rrNextIns: break;
      end;
      line_list:=SplitString(s,',');
      el:=StrToIntDef(line_list[0],0);
      //check if the node-number is 0, then an eset is defined
      if el=0 then begin
        eset:=GetSetIndex(line_list[0]);
        if eset<0 then begin
          fLog.Add(sErrSurfNotDef,[line_list[0]]);
          Exit(false);
        end;
        for j:=0 to Sets[eset].NumNodes-1 do
          AddToSet(nset,'n',Sets[eset].Nodes[j]);
      end
      else
        AddToSet(nset,'n',el);
    until false;
  end
  else if AnsiSameText(typ,'ELEM') then begin
    repeat
      res:=GetLine(s);
      case res of
        rrEnd: Exit(false);
        rrNextIns: break;
      end;
      line_list:=SplitString(s,',');
      if Length(line_list)<>2 then begin
        if Length(s)>0 then
          fLog.Add(sErrSurfInvDat1,[s])
        else
          fLog.Add(sErrSurfInvDat2,[fReader.FileName,fReader.CurrentLine]);
        Exit(false);
      end;
      el:=StrToIntDef(line_list[0],0);
      z:=Copy(line_list[1],2,MaxInt);
      face:=StrToIntDef(z,0)-1;
      if face>=-1 then begin
        // check if the element-number is 0, then an eset is defined
        if el=0 then begin
          eset:=GetSetIndex(line_list[0]);
          if eset<0 then begin
            fLog.Add(sErrSurfNotDef,[line_list[0]]);
            Exit(false);
          end;
          for j:=0 to Sets[eset].NumElements-1 do begin
            facei:=face;
						el:=Sets[eset].Elements[j];
            if ElEnqire[el].Category in [ecTria3..ecQuad8] then begin
              if ElEnqire[el].Attr>3 then begin
                if ElEnqire[el].Category in [ecTria3,ecTria6] then begin
                  if facei<0 then begin
                    if line_list[1][1]='N' then
                      facei:=4
                    else
                      facei:=5;
                  end
                  else facei+=1;
                end else begin
                  if facei<0 then begin
                    if line_list[1][1]='N' then
                      facei:=5
                    else
                      facei:=6;
                  end
                  else facei+=1;
                end;
              end
              else begin
                if facei>1 then
                  facei+=1
                else begin
                  if line_list[1][1]='N' then
                    facei:=0
                  else
                    facei:=1;
                end
              end
            end; // ElEnqire[el].Category
            i:=AddToSet(nset,'j',0);
            if i>-1 then begin
              Sets[nset].Elfaces[i].e:=el;
              Sets[nset].Elfaces[i].f:=facei;
            end
          end; // for j
        end // if el=0
        else begin
          if el>=Length(ElEnqire) then
            Continue;
          if ElEnqire[el].Category in [ecTria3..ecQuad8] then begin
            if ElEnqire[el].Attr>3 then begin
              if ElEnqire[el].Category in [ecTria3,ecTria6] then begin
                if face<0 then begin
									if line_list[1][1]='N' then
										face:=4
									else
                    face:=5;
								end
                else
                  face+=1;
              end else begin
                if face<0  then begin
									if line_list[1][1]='N' then
										face:=5
                  else
                    face:=6;
								end
                else
                  face+=1;
              end
            end
            else begin
              if face>1 then
								face+=1
							else begin
								if line_list[1][1]='N' then
									face:=0
								else
									face:=1;
							end
            end
          end;
          i:=AddToSet(nset,'j',0);
          if i>-1 then begin
            Sets[nset].Elfaces[i].e:=el;
            Sets[nset].Elfaces[i].f:=face;
          end
        end;
      end; // if AnsiSameText(typ,'ELEM')

    until false;
  end
  else
    result:=GetLine(s)=rrNormal;
  result:=true;
end;


procedure TInpFile.SetCurrentReader(path: string);
var
  r: TFileReader;
begin
  if Assigned(fReader) and (not FilenameIsAbsolute(path)) then
    path:=CreateAbsoluteSearchPath(path,ExtractFilePath(fReader.FileName));
  if FileExists(path) then begin
    r:=TFileReader.Create;
    r.Parse(path);
    if r.CurrentLine<0 then
      r.Free
    else
      fReader:=TFileReader(fFileStack.Push(r));
  end else
    fLog.Add(sErrIncFileNoExst,[path]);
end;


////////////////////////////////////////////////////////////////////////////////


procedure TInpFile.Parse(lines: TStrings; const aFileName: string);
label
  next1, next2;
var
  process: boolean;
  line: string;
  i,sz: integer;
begin
  Clear;
  fFileName:=aFileName;
  fReader:=TFileReader.Create;
  fReader.Parse(lines,fFileName);
  fFileStack.Push(fReader);
  SetLength(Nodes,1); // add one node first
  FillChar(Nodes[0],SizeOf(TNode),0);
  Nodes[0].Index:=-1;
  SetLength(Elements,1); // add one element first
  FillChar(Elements[0],SizeOf(TElement),0);
  process:=true;

  // first pas
  repeat
    if GetLine(line)=rrEnd then
      break;

    next1:

    if IsStringStarted(line,'*NODE') then begin
      process:=ExtractNodes(line);
      goto next1;
    end

    else if IsStringStarted(line,'*ELEMENT') then begin
      process:=ExtractElements(line);
      goto next1;
    end

    else if IsStringStarted(line,'*ELSET') then begin
      process:=ExtractElset(line);
      goto next1;
    end

    else if IsStringStarted(line,'*NSET') then begin
      process:=ExtractNset(line);
      goto next1;
    end;

  until not process;

  if Summen.nmax=-MaxInt then begin
    Summen.nmax:=0;
    Summen.nmin:=0;
  end;

  if Summen.emax=-MaxInt then begin
    Summen.emax:=0;
    Summen.emin:=0;
  end;

  sz:=Summen.nmax+1;
  SetLength(Nodes,sz);
  Summen.n:=Summen.nmax;

  sz:=Summen.e+1;
  SetLength(Elements,sz);

  InitElements;

  // two pass
  process:=true;

  ResetReaders;
  fReader:=TFileReader.Create;
  fReader.Parse(lines,fFileName);
  fFileStack.Push(fReader);

  repeat
    if GetLine(line)=rrEnd then
      break;

    next2:
    if IsStringStarted(line,'*SURFACE') then begin
      process:=ExtractSurface(line);
      goto next2;
    end;

  until not process;

  // third pass - find LastProcessLine
  for i:=0 to lines.Count-1 do begin
    line:=TrimLeft(lines[i]);
    if line='' then
      continue;
    if line[1]<>'*' then
      continue;
    if IsStringStarted(line,'**') then
      continue;

    process:=IsStringStarted(line,'*INCLUDE')
      or IsStringStarted(line,'*NODE')
      or IsStringStarted(line,'*ELEMENT')
      or IsStringStarted(line,'*ELSET')
      or IsStringStarted(line,'*NSET');
    if not process then begin
      fLastProcessLine:=i+1;
      break;
    end;
  end;
  if fLastProcessLine=0 then
    fLastProcessLine:=lines.Count-1;


  GetElemNormales;
  MakeSurfaces;
  CalcScale;
  ScaleNodes;
  CalcStat;
end;


end.


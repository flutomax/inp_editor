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

unit uNodeTransform;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils;

type

  TNode3D = record
    N: Integer;
    X: Extended;
    Y: Extended;
    Z: Extended;
  end;

  TNodeList = array of TNode3D;

  TBoundBox = record
    Width: Extended;
    Height: Extended;
    Depth: Extended;
  end;

  TMatrix = array[0..3,0..3] of Extended;

  TOpType = (opNone, opTranslate, opScale, opRotateX, opRotateY, opRotateZ);

  TOpRot = record
    C1, C2, Alpha: Extended;
  end;

  TOpRec = record
    OpType: TOpType;
    Dx, Dy, Dz: Extended;
    Sx, Sy, Sz: Extended;
    RotX: TOpRot;
    RotY: TOpRot;
    RotZ: TOpRot;
  end;
  POpRec = ^TOpRec;

  TOpRecs = array[0..7] of TOpRec;

const

  OpTypes: array [0..5] of TOpType = (opNone, opTranslate, opScale, opRotateX,
    opRotateY, opRotateZ);

type

  { TNodeProcessor }

  TNodeProcessor = class
  private
    fNodes: TNodeList;
    fBoundBox: TBoundBox;
    fBlockBegin: TPoint;
    fBlockEnd: TPoint;
    function GetCount: Integer;
    function GetNode(const Index: Integer): TNode3D;
    procedure Reset;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(List: TStrings);
    property BoundBox: TBoundBox read fBoundBox;
    property BlockBegin: TPoint read fBlockBegin;
    property BlockEnd: TPoint read fBlockEnd;
    property Count: Integer read GetCount;
    property Node[const Index: Integer]: TNode3D read GetNode; default;
  end;

  { TAffineTransform }

  TAffineTransform = class
  private
    fMatrix: TMatrix;
  public
    constructor Create; virtual;
    procedure Transform(const Src: TNode3D; out Dst: TNode3D);
    procedure Translate(const Dx, Dy, Dz: Extended);
    procedure Scale(const Sx, Sy, Sz: Extended);
    procedure RotateZ(const Alpha, Cx, Cy: Extended);
    procedure RotateX(const Alpha, Cy, Cz: Extended);
    procedure RotateY(const Alpha, Cx, Cz: Extended);
  end;

implementation

uses
  Math, uEditorMisc;

const
  IdentityMatrix: TMatrix = (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));


function MultMatrix(const M1, M2: TMatrix): TMatrix;
var
  i,j: Integer;
begin
  for i:=0 to 3 do
    for j:=0 to 3 do
      Result[i,j]:=M1[0,j]*M2[i,0]+M1[1,j]*M2[i,1]+M1[2,j]*M2[i,2]+M1[3,j]*M2[i,3];
end;


function SplitString(s: string; delimiter: char): TStringDynArray;
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

function IsStringStarted(const line, pattern: string): Boolean;
begin
  result:=SameText(Copy(line,1,Length(pattern)),pattern);
end;

{ TNodeProcessor }

constructor TNodeProcessor.Create;
begin
  Reset;
end;

destructor TNodeProcessor.Destroy;
begin
  fNodes:=nil;
  inherited Destroy;
end;

procedure TNodeProcessor.Reset;
begin
  fNodes:=nil;
  FillChar(fBoundBox,SizeOf(TBoundBox),0);
  fBlockBegin.X:=1;
  fBlockBegin.Y:=1;
  fBlockEnd.X:=1;
  fBlockEnd.Y:=1;
end;

function TNodeProcessor.GetNode(const Index: Integer): TNode3D;
begin
  FillChar(result,SizeOf(TNode3D),0);
  if InRange(Index,0,Count-1) then
    result:=fNodes[Index];
end;

function TNodeProcessor.GetCount: Integer;
begin
  result:=Length(fNodes);
end;

procedure TNodeProcessor.Parse(List: TStrings);
var
  i,j,p: Integer;
  line: string;
  data: TStringDynArray;
  read,start: Boolean;
  xMin,xMax,yMin,yMax,
  zMin,zMax: Extended;
begin
  Reset;
  read:=false;
  start:=true;
  for i:=0 to List.Count-1 do begin
    line:=Trim(List[i]);
    if line='' then
      continue;
    if line[1]='*' then begin
      if IsStringStarted(line,'**') then
        continue;
      if read then
        break;
    end;
    if IsStringStarted(line,'*NODE') then
      read:=true
    else if read then begin
      if start then begin
        fBlockBegin.X:=1;
        fBlockBegin.Y:=i+1;
        start:=false;
      end;
      fBlockEnd.X:=Length(List[i])+1;
      fBlockEnd.Y:=i+1;
      data:=SplitString(line,',');
      p:=Length(data);
      if p=0 then
        continue;
      j:=Length(fNodes);
      SetLength(fNodes,j+1);
      FillChar(fNodes[j],SizeOf(TNode3D),0);
      with fNodes[j] do begin
        N:=StrToIntDef(data[0],0);
        if p>1 then
          X:=ParseValue(data[1]);
        if p>2 then
          Y:=ParseValue(data[2]);
        if p>3 then
          Z:=ParseValue(data[3]);
        if j=0 then begin
          xMin:=X; xMax:=X;
          yMin:=Y; yMax:=Y;
          zMin:=Z; zMax:=Z;
        end else begin
          xMin:=Min(xMin,X);
          xMax:=Max(xMax,X);
          yMin:=Min(yMin,Y);
          yMax:=Max(yMax,Y);
          zMin:=Min(zMin,Z);
          zMax:=Max(zMax,Z);
        end;
      end;
    end;
  end;
  fBoundBox.Width:=xMax-xMin;
  fBoundBox.Height:=yMax-yMin;
  fBoundBox.Depth:=zMax-zMin;
end;


{ TAffineTransform }

constructor TAffineTransform.Create;
begin
  fMatrix:=IdentityMatrix;
end;

procedure TAffineTransform.Transform(const Src: TNode3D; out Dst: TNode3D);
begin
  Dst.N:=Src.N;
  Dst.X:=Src.X*fMatrix[0,0]+Src.Y*fMatrix[1,0]+Src.Z*fMatrix[2,0]+fMatrix[3,0];
  Dst.Y:=Src.X*fMatrix[0,1]+Src.Y*fMatrix[1,1]+Src.Z*fMatrix[2,1]+fMatrix[3,1];
  Dst.Z:=Src.X*fMatrix[0,2]+Src.Y*fMatrix[1,2]+Src.Z*fMatrix[2,2]+fMatrix[3,2];
end;

procedure TAffineTransform.Translate(const Dx, Dy, Dz: Extended);
var
  M: TMatrix;
begin
  M:=IdentityMatrix;
  M[3,0]:=Dx;
  M[3,1]:=Dy;
  M[3,2]:=Dz;
  fMatrix:=MultMatrix(M,fMatrix);
end;

procedure TAffineTransform.Scale(const Sx, Sy, Sz: Extended);
var
  M: TMatrix;
begin
  M:=IdentityMatrix;
  M[0,0]:=Sx;
  M[1,1]:=Sy;
  M[2,2]:=Sz;
  fMatrix:=MultMatrix(M,fMatrix);
end;

procedure TAffineTransform.RotateZ(const Alpha, Cx, Cy: Extended);
var
  M: TMatrix;
  S,C: Extended;
begin
  if (Cx<>0) or (Cy<>0) then
    Translate(-Cx,-Cy,0);
  SinCos(DegToRad(Alpha),S,C);
  M:=IdentityMatrix;
  M[0,0]:=C;
  M[1,0]:=-S;
  M[0,1]:=S;
  M[1,1]:=C;
  fMatrix:=MultMatrix(M,fMatrix);
  if (Cx<>0) or (Cy<>0) then
    Translate(Cx,Cy,0);
end;

procedure TAffineTransform.RotateX(const Alpha, Cy, Cz: Extended);
var
  M: TMatrix;
  S,C: Extended;
begin
  if (Cy<>0) or (Cz<>0) then
    Translate(0,-Cy,-Cz);
  SinCos(DegToRad(Alpha),S,C);
  M:=IdentityMatrix;
  M[1,1]:=C;
  M[2,1]:=-S;
  M[1,2]:=S;
  M[2,2]:=C;
  fMatrix:=MultMatrix(M,fMatrix);
  if (Cy<>0) or (Cz<>0) then
    Translate(0,Cy,Cz);
end;

procedure TAffineTransform.RotateY(const Alpha, Cx, Cz: Extended);
var
  M: TMatrix;
  S,C: Extended;
begin
  if (Cx<>0) or (Cz<>0) then
    Translate(-Cx,0,-Cz);
  SinCos(DegToRad(Alpha),S,C);
  M:=IdentityMatrix;
  M[0,0]:=C;
  M[2,0]:=S;
  M[0,2]:=-S;
  M[2,2]:=C;
  fMatrix:=MultMatrix(M,fMatrix);
  if (Cx<>0) or (Cz<>0) then
    Translate(Cx,0,Cz);
end;

end.


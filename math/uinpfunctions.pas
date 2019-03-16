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

unit uInpFunctions;

{$mode objfpc}{$H+}
{$DEFINE TEX_MODE}

interface

uses
  Types, Classes, SysUtils, GL, uInpTypes;

type
  TQSortCompare = function(a, b: Pointer): Integer;
  TGlOne  = array[0..0] of GLfloat;
  TGlQuat = array[0..3] of GLfloat;

const

  Z_DEPTH = 2.0;
  TRACKBALLSIZE = 0.8;
  PT_ZERO: TPoint3D = (X:0;Y:0;Z:0);
  GL_MAX_EVAL_ORDER_VAL = 8;
  LMODEL_ONESIDE: TGlOne = (0.0);

var
  DrawMode: byte=0; // actual draw-function (Load=1, Light=2, Animate=3, Preprocessor=4, Vector=5)

function VSub(const v1, v2: TPoint3D): TPoint3D; overload;
function VSub(const v1, v2: TNode): TPoint3D; overload;
function VCross(const v1, v2: TPoint3D): TPoint3D; overload;
function VCross(const v1, v2: TQuat): TPoint3D; overload;
function VNormal(const a: TPoint3D; var v: TPoint3D): Double; overload;
procedure VNormal(var v: TPoint3D); overload;
procedure VdSort(a: PIntegerArray);
procedure QSort(base: Pointer; num, width: NativeInt; comp: TQSortCompare);
function CompareElnum1(a, b: Pointer): Integer;
function CompareFaces4(a, b: Pointer): Integer;
function CompareInt(a, b: Pointer): Integer;

procedure Trackball(var q: TQuat; const p1x, p1y, p2x, p2y: Double);
procedure AddQuats(const q1, q2: TQuat; var dest: TQuat);
procedure BuildRotMatrix(var m: TQuatMatrix; const q: TQuat);
procedure MatrixSub(var ms: TQuatMatrix; const m, s: TQuatMatrix);
procedure MatrixMul(var v: TQuat; const m: TQuatMatrix);
procedure InitLightAndMaterial;

procedure DrawFaces(const num: Integer; node: TNodeDynArray; face: TFaceDynArray;
  elem: TElementDynArray; const col: Integer; const _type: Char; const pickflag: Integer);
procedure DrawFacesEdge(const num: Integer; node: TNodeDynArray; face: TFaceDynArray;
  const col: Integer; const _type: Char; var ds: Double);
procedure DrawElemNodes(const num: Integer; node: TNodeDynArray;
  elem: TIntegerDynArray; const col: Integer; const _type: Char);
procedure DrawElements(const num: Integer; name: TIntegerDynArray;
  node: TNodeDynArray; elem: TElementDynArray; const col: Integer);



implementation

uses
  Math;

const
  CUTOFF = 8;
  STKSIZ = (8*SizeOf(Pointer)-2);

var
  OldQBuff: TQuat;


function ProjectHypToSphere(const r, x, y: Double): Double;
var
  d, t: Double;
begin
  d:=Sqrt(x*x+y*y);
  if (d<r*0.70710678118654752440) then   // Inside sphere
    result:=Sqrt(r*r-d*d)
  else begin // On hyperbola
    t:=r/1.41421356237309504880;
    result:=t*t/d;
  end;
end;

function VAdd(const v1, v2: TPoint3D): TPoint3D;
begin
  result.X:=v1.X+v2.X;
  result.Y:=v1.Y+v2.Y;
  result.Z:=v1.Z+v2.Z;
end;

function VSub(const v1, v2: TPoint3D): TPoint3D; overload;
begin
  result.X:=v1.X-v2.X;
  result.Y:=v1.Y-v2.Y;
  result.Z:=v1.Z-v2.Z;
end;

function VSub(const v1, v2: TNode): TPoint3D; overload;
begin
  result.X:=v1.X-v2.X;
  result.Y:=v1.Y-v2.Y;
  result.Z:=v1.Z-v2.Z;
end;

function VCross(const v1, v2: TPoint3D): TPoint3D; overload;
begin
  result.X:=(v1.Y*v2.Z)-(v1.Z*v2.Y);
  result.Y:=(v1.Z*v2.X)-(v1.X*v2.Z);
  result.Z:=(v1.X*v2.Y)-(v1.Y*v2.X);
end;

function VCross(const v1, v2: TQuat): TPoint3D; overload;
begin
  result.X:=(v1[1]*v2[2])-(v1[2]*v2[1]);
  result.Y:=(v1[2]*v2[0])-(v1[0]*v2[2]);
  result.Z:=(v1[0]*v2[1])-(v1[1]*v2[0]);
end;

function VDot(const v1, v2: TQuat): Double;
begin
  result:=v1[0]*v2[0]+v1[1]*v2[1]+v1[2]*v2[2];
end;

function VLength(const v: TPoint3D): Double;
begin
  result:=Sqrt(v.X*v.X+v.Y*v.Y+v.Z*v.Z);
end;

procedure VScale(var v: TPoint3D; const d: Double);
begin
  v.X*=d;
  v.Y*=d;
  v.Z*=d;
end;

procedure VNormal(var v: TPoint3D); overload;
begin
  VScale(v,1/VLength(v));
end;

function VNormal(const a: TPoint3D; var v: TPoint3D): Double; overload;
begin
  result:=VLength(a);
  if IsZero(result) then
    exit;
  v.X:=a.X/result;
  v.Y:=a.Y/result;
  v.Z:=a.Z/result;
end;

procedure AxisToQuat(const a: TPoint3D; const phi: Double; var q: TQuat);
var
  t: TPoint3D;
begin
  t:=a;
  VNormal(t);
  VScale(t,Sin(phi/2));
  q[0]:=t.X;
  q[1]:=t.Y;
  q[2]:=t.Z;
  q[3]:=Cos(phi/2);
end;

procedure Trackball(var q: TQuat; const p1x, p1y, p2x, p2y: Double);
var
  a, d, p1, p2: TPoint3D;
  t, phi: Double;
begin
  if (p1x=p2x) and (p1y=p2y) then begin
    // Zero rotation
    q[0]:=0;
    q[1]:=0;
    q[2]:=0;
    q[3]:=1;
    Exit;
  end;

  // First, figure out z-coordinates for projection of P1 and P2 to deformed sphere
  p1.X:=p1x;
  p1.Y:=p1y;
  p1.Z:=ProjectHypToSphere(TRACKBALLSIZE,p1x,p1y);
  p2.X:=p2x;
  p2.Y:=p2y;
  p2.Z:=ProjectHypToSphere(TRACKBALLSIZE,p2x,p2y);

  // Now, we want the cross product of P1 and P2
  a:=VCross(p2,p1);
  // Figure out how much to rotate around that axis.
  d:=VSub(p1,p2);
  t:=VLength(d)/(2.0*TRACKBALLSIZE);
  // Avoid problems with out-of-control values.
  t:=EnsureRange(t,-1,1);
  phi:=2*arcsin(t);
  AxisToQuat(a,phi,q);
end;

procedure NormalizeQuat(var q: TQuat);
var
  i: Integer;
  mag: Double;
begin
  mag:=Sqrt(q[0]*q[0]+q[1]*q[1]+q[2]*q[2]+q[3]*q[3]);
  if (mag>1.01) or (mag<0.99) then begin
    q:=OldQBuff;
    if (q[0]*q[0]+q[1]*q[1]+q[2]*q[2]=0.0) then
      q[0]:=1.0;
  end else begin
    for i:=0 to 3 do
      q[i]/=mag;
    OldQBuff:=q;
  end;
end;

procedure AddQuats(const q1, q2: TQuat; var dest: TQuat);
var
  t1, t2, t3, tf: TPoint3D;
begin
  t1.X:=q1[0];
  t1.Y:=q1[1];
  t1.Z:=q1[2];
  VScale(t1,q2[3]);
  t2.X:=q2[0];
  t2.Y:=q2[1];
  t2.Z:=q2[2];
  VScale(t2,q1[3]);
  t3:=VCross(q2,q1);
  tf:=Vadd(t1,t2);
  tf:=Vadd(t3,tf);
  dest[0]:=tf.X;
  dest[1]:=tf.Y;
  dest[2]:=tf.Z;
  dest[3]:=q1[3]*q2[3]-VDot(q1,q2);
  NormalizeQuat(dest);
end;

procedure BuildRotMatrix(var m: TQuatMatrix; const q: TQuat);
begin
  m[0,0]:=1.0-2.0*(q[1]*q[1]+q[2]*q[2]);
  m[0,1]:=2.0*(q[0]*q[1]-q[2]*q[3]);
  m[0,2]:=2.0*(q[2]*q[0]+q[1]*q[3]);
  m[0,3]:=0.0;
  m[1,0]:=2.0*(q[0]*q[1]+q[2]*q[3]);
  m[1,1]:=1.0-2.0*(q[2]*q[2]+q[0]*q[0]);
  m[1,2]:=2.0*(q[1]*q[2]-q[0]*q[3]);
  m[1,3]:=0.0;
  m[2,0]:=2.0*(q[2]*q[0]-q[1]*q[3]);
  m[2,1]:=2.0*(q[1]*q[2]+q[0]*q[3]);
  m[2,2]:=1.0-2.0*(q[1]*q[1]+q[0]*q[0]);
  m[2,3]:=0.0;
  m[3,0]:=0.0;
  m[3,1]:=0.0;
  m[3,2]:=0.0;
  m[3,3]:=1.0;
end;

procedure MatrixSub(var ms: TQuatMatrix; const m, s: TQuatMatrix);
var
  i,j: Integer;
begin
  for i:=0 to 3 do
    for j:=0 to 3 do
      ms[i,j]:=m[i,j]-s[i,j];
end;

procedure MatrixMul(var v: TQuat; const m: TQuatMatrix);
var
  b: TQuat;
  i: Integer;
begin
  for i:=0 to 3 do
    b[i]:=v[0]*m[0,i]+v[1]*m[1,i]+v[2]*m[2,i]+v[3]*m[3,i];
  v:=b;
end;

procedure VdSort(a: PIntegerArray);
var
  n0, n1, n2, n3: integer;
begin
  if a^[1]<a^[0] then begin
    n0:=a^[1];
    n1:=a^[0];
  end
  else begin
    n0:=a^[0];
    n1:=a^[1];
  end;
  if a^[3]<a^[2] then begin
    n2:=a^[3];
    n3:=a^[2];
  end
  else begin
    n2:=a^[2];
    n3:=a^[3];
  end;
  if n2<n0 then begin
    a^[0]:=n2;
    if n3<n0 then begin
      a^[1]:=n3;
      a^[2]:=n0;
      a^[3]:=n1;
    end
    else
    if n3<n1 then begin
      a^[1]:=n0;
      a^[2]:=n3;
      a^[3]:=n1;
    end
    else begin
      a^[1]:=n0;
      a^[2]:=n1;
      a^[3]:=n3;
    end;
  end
  else begin
    a^[0]:=n0;
    if n2<n1 then begin
      a^[1]:=n2;
      if n3<n1 then begin
        a^[2]:=n3;
        a^[3]:=n1;
      end
      else begin
        a^[2]:=n1;
        a^[3]:=n3;
      end;
    end
    else begin
      a^[1]:=n1;
      a^[2]:=n2;
      a^[3]:=n3;
    end;
  end;
end;

function CompareInt(a, b: Pointer): Integer;
begin
  if PInteger(a)^<PInteger(b)^ then
    Exit(-1)
  else if PInteger(a)^>PInteger(b)^ then
    Exit(1)
  else
    Exit(0);
end;

function CompareElnum1(a, b: Pointer): Integer;
begin
  if PIntegerArray(a)^[0]<PIntegerArray(b)^[0] then
    Exit(-1)
  else if PIntegerArray(a)^[0]>PIntegerArray(b)^[0] then
    Exit(1)
  else
    Exit(0);
end;

function CompareFaces4(a, b: Pointer): Integer;
begin
  if PInteger(a)^<PInteger(b)^ then
    Exit(-1)
  else if PInteger(a)^>PInteger(b)^ then
    Exit(1)
  else begin
    Inc(PInteger(a));
    Inc(PInteger(b));
    if PInteger(a)^<PInteger(b)^ then
      Exit(-1)
    else if PInteger(a)^>PInteger(b)^ then
      Exit(1)
    else begin
      Inc(PInteger(a));
      Inc(PInteger(b));
      if PInteger(a)^<PInteger(b)^ then
        Exit(-1)
      else if PInteger(a)^>PInteger(b)^ then
        Exit(1)
      else begin
        Inc(PInteger(a));
        Inc(PInteger(b));
        if PInteger(a)^<PInteger(b)^ then
          Exit(-1)
        else if PInteger(a)^>PInteger(b)^ then
          Exit(1)
        else
          Exit(0);
      end;
    end;
  end;
end;

procedure Swap(a, b: PShortInt; width: NativeInt);
var
  tmp: ShortInt;
begin
  if a<>b then begin
  // Do the swap one character at a time to avoid potential alignment problems.
    while width>0 do begin
      tmp:=a^;
      a^:=b^;
      Inc(a);
      b^:=tmp;
      Inc(b);
      Dec(width);
    end;
  end;
end;

procedure ShortSort(lo, hi: PShortInt; width: NativeInt; comp: TQSortCompare);
var
  p, max: PShortInt;
begin
  // Note: in assertions below, i and j are alway inside original bound of array to sort.
  while hi>lo do begin
    max:=lo;
    p:=lo+width;
    while p<=hi do begin
      if comp(p,max)>0 then
        max:=p;
      p+=width;
    end;
    Swap(max, hi, width);
    hi-=width;
  end;
end;

procedure QSort(base: Pointer; num, width: NativeInt; comp: TQSortCompare);
type
  TStack = array[0..STKSIZ] of PShortInt;
label
  recurse;
var
  lo, hi: PShortInt;              // ends of sub-array currently sorting
  mid: PShortInt;                 // points to middle of subarray
  loguy, higuy: PShortInt;        // traveling pointers for partition step
  size: NativeInt;                // size of the sub-array
  lostk, histk: TStack;
  stkptr: Integer;                // stack for saving sub-array to be processed
begin
  if (num<2) or (base=nil) then
    Exit;
  stkptr:=0; // initialize stack
  lo:=PShortInt(base);
  hi:=PShortInt(base+width*(num-1)); // initialize limits

recurse:
  size:=(hi-lo) div width + 1; // number of el's to sort

  // below a certain size, it is faster to use a O(n^2) sorting method */
  if size<=CUTOFF then
    ShortSort(lo,hi,width,comp)
  else begin

    mid:=lo+(size div 2)*width; // find middle element
    // Sort the first, middle, last elements into order
    if comp(lo,mid)>0 then
      Swap(lo,mid,width);
    if comp(lo,hi)>0 then
      Swap(lo,hi,width);
    if comp(mid,hi)>0 then
      Swap(mid, hi, width);

    loguy:=lo;
    higuy:=hi;

    while true do begin
      if mid>loguy then begin
        repeat
          loguy+=width;
        until not((loguy<mid) and (comp(loguy,mid)<=0));
      end;
      if mid<=loguy then begin
        repeat
          loguy+=width;
        until not((loguy<=hi) and (comp(loguy,mid)<=0));
      end;

      repeat
        higuy-=width;
      until not((higuy>mid) and (comp(higuy,mid)>0));

      if higuy<loguy then
        break;

      Swap(loguy,higuy,width);

      if mid=higuy then
        mid:=loguy;
    end;

    higuy+=width;
    if mid<higuy then begin
      repeat
        higuy-=width;
      until not((higuy>mid) and (comp(higuy,mid)=0));
    end;

    if mid>=higuy then begin
      repeat
        higuy-=width;
      until not((higuy>lo) and (comp(higuy,mid)=0));
    end;

    if (higuy-lo)>=(hi-loguy) then begin
      if lo<higuy then begin
        lostk[stkptr]:=lo;
        histk[stkptr]:=higuy;
        Inc(stkptr);
      end;  // save big recursion for later

      if loguy<hi then begin
        lo:=loguy;
        goto recurse; // do small recursion
      end;
    end else begin
      if loguy<hi then begin
        lostk[stkptr]:=loguy;
        histk[stkptr]:=hi;
        Inc(stkptr); // save big recursion for later
      end;

      if lo<higuy then begin
        hi:=higuy;
        goto recurse; // do small recursion
      end;
    end;
  end;

    Dec(stkptr);
    if stkptr>=0 then begin
      lo:=lostk[stkptr];
      hi:=histk[stkptr];
      goto recurse; // pop subarray from stack
    end
    else
      Exit; // all subarrays done
end;

const
  AMB = 1.0;
  DIFF = 0.5;
  MAT_SPEC = 0.0;
  MAT_DIFF = 0.6;
  MAT_SHININESS: TGlOne = (128);
  LMODEL_AMBIENT: TGlQuat = (0.5, 0.5, 0.5, 1.0);

  AMBIENT0: TGlQuat = (AMB, AMB, AMB, 1.0);
  DIFFUSE0: TGlQuat = (DIFF, DIFF, DIFF, 1.0);
  MAT_SPECULAR: TGlQuat = (MAT_SPEC,MAT_SPEC,MAT_SPEC, 1.0);
  MAT_DIFFUSE: TGlQuat = (MAT_DIFF,MAT_DIFF,MAT_DIFF, 1.0);
  POSITION0: TGlQuat = (0, 0, -1, 0);


procedure InitLightAndMaterial;
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT, AMBIENT0);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, DIFFUSE0);
  glLightfv(GL_LIGHT0, GL_SPECULAR, DIFFUSE0);
  glLightfv(GL_LIGHT0, GL_POSITION, POSITION0);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, MAT_SHININESS);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,   MAT_DIFFUSE);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR,  MAT_SPECULAR);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, LMODEL_AMBIENT);
  glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, LMODEL_ONESIDE);
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_FALSE);
  glEnable(GL_LIGHT0);
  glDisable(GL_COLOR_MATERIAL);
end;

procedure SetLightAndMaterial(const col: Integer);
var
  matdiffuse: TGlQuat;
begin
  matdiffuse[0]:=COLOR_ENTITY[col].r;
  matdiffuse[1]:=COLOR_ENTITY[col].g;
  matdiffuse[2]:=COLOR_ENTITY[col].b;
  matdiffuse[3]:=1;
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, MAT_SHININESS);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,   matdiffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR,  MAT_SPECULAR);
end;

procedure DrawFaces(const num: Integer; node: TNodeDynArray; face: TFaceDynArray;
  elem: TElementDynArray; const col: Integer; const _type: Char; const pickflag: Integer);
var
  i,a: Integer;
begin
  glLineWidth(1.0);
  SetLightAndMaterial(col);
  glEnable(GL_LIGHTING);

  for i:=0 to num-1 do begin
    a:=i;
    if ((elem[face[a].ElemNumber].Category in [ecHexa8,ecTetra4,ecHexa20..ecPenta15,ecTetra10])
    and (drawMode<4)) then
      glEnable(GL_CULL_FACE)
    else
      glDisable(GL_CULL_FACE);

    case face[a].Category of

      ecTria3: begin
        glNormal3dv(@face[a].Side[0].X);
        glBegin(GL_TRIANGLES);
        glVertex3dv(@node[face[a].Node[0]].X);
        glVertex3dv(@node[face[a].Node[1]].X);
        glNormal3dv(@face[a].Side[0].X);
        glVertex3dv(@node[face[a].Node[2]].X);
        glEnd;
      end;

      ecTria6: begin
        glNormal3dv (@face[a].Side[0].X);
        glBegin(GL_TRIANGLES);
        glVertex3dv(@node[face[a].Node[0]].X);
        glVertex3dv(@node[face[a].Node[3]].X);
        glNormal3dv(@face[a].Side[0].X);
        glVertex3dv(@node[face[a].Node[5]].X);
        glVertex3dv(@node[face[a].Node[2]].X);
        glVertex3dv(@node[face[a].Node[5]].X);
        glNormal3dv(@face[a].Side[1].X);
        glVertex3dv(@node[face[a].Node[4]].X);
        glVertex3dv(@node[face[a].Node[4]].X);
        glVertex3dv(@node[face[a].Node[5]].X);
        glNormal3dv(@face[a].Side[2].X);
        glVertex3dv(@node[face[a].Node[3]].X);
        glVertex3dv(@node[face[a].Node[3]].X);
        glVertex3dv(@node[face[a].Node[1]].X);
        glNormal3dv(@face[a].Side[3].X);
        glVertex3dv(@node[face[a].Node[4]].X);
        glEnd;
      end;

      ecQuad4: begin
        glNormal3dv(@face[a].Side[0].X);
        glBegin(GL_TRIANGLE_STRIP);
        glVertex3dv(@node[face[a].Node[0]].X);
        glVertex3dv(@node[face[a].Node[1]].X);
        glNormal3dv(@face[a].Side[0].X);
        glVertex3dv(@node[face[a].Node[3]].X);
        glVertex3dv(@node[face[a].Node[2]].X);
        glEnd;
      end;

      ecQuad8: begin
        glNormal3dv(@face[a].Side[0].X);
        glBegin(GL_TRIANGLE_FAN);
        glVertex3dv(@node[face[a].Node[8]].X);
        glVertex3dv(@node[face[a].Node[0]].X);
        glNormal3dv(@face[a].Side[0].X);
        glVertex3dv(@node[face[a].Node[4]].X);
        glNormal3dv(@face[a].Side[1].X);
        glVertex3dv(@node[face[a].Node[1]].X);
        glNormal3dv(@face[a].Side[2].X);
        glVertex3dv(@node[face[a].Node[5]].X);
        glNormal3dv(@face[a].Side[3].X);
        glVertex3dv(@node[face[a].Node[2]].X);
        glNormal3dv(@face[a].Side[4].X);
        glVertex3dv(@node[face[a].Node[6]].X);
        glNormal3dv(@face[a].Side[5].X);
        glVertex3dv(@node[face[a].Node[3]].X);
        glNormal3dv(@face[a].Side[6].X);
        glVertex3dv(@node[face[a].Node[7]].X);
        glNormal3dv(@face[a].Side[7].X);
        glVertex3dv(@node[face[a].Node[0]].X);
        glEnd;
      end;

      ecSeg2: begin
        glBegin(GL_LINES);
        glVertex3dv(@node[face[a].Node[0]].X);
        glVertex3dv(@node[face[a].Node[1]].X);
        glEnd;
      end;

      ecSeg3: begin
        glBegin(GL_LINE_STRIP);
        glVertex3dv(@node[face[a].Node[0]].X);
        glVertex3dv(@node[face[a].Node[2]].X);
        glVertex3dv(@node[face[a].Node[1]].X);
        glEnd;
      end;

    end;

  end; // for
  glDisable(GL_LIGHTING);
  glEnable(GL_CULL_FACE);
end;

procedure DrawFacesEdge(const num: Integer; node: TNodeDynArray; face: TFaceDynArray;
  const col: Integer; const _type: Char; var ds: Double);
var
  ipuf: array[0..1] of GLint;
  i,a: integer;
begin
  glGetIntegerv(GL_CULL_FACE_MODE,ipuf);
  if ipuf[0]=GL_FRONT then
    ds*=-1;
  glColor3d(col,col,col);
  glLineWidth(1.0);

  for i:=0 to num-1 do begin
    a:=i;
    case face[a].Category of

      ecTria3: begin
        glBegin(GL_LINE_LOOP);
        glVertex3d(node[face[a].Node[0]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[0]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[0]].Z+face[a].Side[0].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[1]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[1]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[1]].Z+face[a].Side[0].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[2]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[2]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[2]].Z+face[a].Side[0].Z*1e-3*ds);
        glEnd;
      end;

      ecTria6: begin
        glBegin(GL_LINE_LOOP);
        glVertex3d(node[face[a].Node[0]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[0]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[0]].Z+face[a].Side[0].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[3]].X+face[a].Side[2].X*1e-3*ds,
                   node[face[a].Node[3]].Y+face[a].Side[2].Y*1e-3*ds,
                   node[face[a].Node[3]].Z+face[a].Side[2].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[1]].X+face[a].Side[3].X*1e-3*ds,
                   node[face[a].Node[1]].Y+face[a].Side[3].Y*1e-3*ds,
                   node[face[a].Node[1]].Z+face[a].Side[3].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[4]].X+face[a].Side[2].X*1e-3*ds,
                   node[face[a].Node[4]].Y+face[a].Side[2].Y*1e-3*ds,
                   node[face[a].Node[4]].Z+face[a].Side[2].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[2]].X+face[a].Side[1].X*1e-3*ds,
                   node[face[a].Node[2]].Y+face[a].Side[1].Y*1e-3*ds,
                   node[face[a].Node[2]].Z+face[a].Side[1].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[5]].X+face[a].Side[2].X*1e-3*ds,
                   node[face[a].Node[5]].Y+face[a].Side[2].Y*1e-3*ds,
                   node[face[a].Node[5]].Z+face[a].Side[2].Z*1e-3*ds);
        glEnd;
      end;

      ecQuad4: begin
        glBegin(GL_LINE_LOOP);
        glVertex3d(node[face[a].Node[0]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[0]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[0]].Z+face[a].Side[0].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[1]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[1]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[1]].Z+face[a].Side[0].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[2]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[2]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[2]].Z+face[a].Side[0].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[3]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[3]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[3]].Z+face[a].Side[0].Z*1e-3*ds);
        glEnd;
      end;

      ecQuad8: begin
        glBegin(GL_LINE_LOOP);
        glVertex3d(node[face[a].Node[0]].X+face[a].Side[0].X*1e-3*ds,
                   node[face[a].Node[0]].Y+face[a].Side[0].Y*1e-3*ds,
                   node[face[a].Node[0]].Z+face[a].Side[0].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[4]].X+face[a].Side[1].X*1e-3*ds,
                   node[face[a].Node[4]].Y+face[a].Side[1].Y*1e-3*ds,
                   node[face[a].Node[4]].Z+face[a].Side[1].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[1]].X+face[a].Side[5].X*1e-3*ds,
                   node[face[a].Node[1]].Y+face[a].Side[5].Y*1e-3*ds,
                   node[face[a].Node[1]].Z+face[a].Side[5].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[5]].X+face[a].Side[4].X*1e-3*ds,
                   node[face[a].Node[5]].Y+face[a].Side[4].Y*1e-3*ds,
                   node[face[a].Node[5]].Z+face[a].Side[4].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[2]].X+face[a].Side[3].X*1e-3*ds,
                   node[face[a].Node[2]].Y+face[a].Side[3].Y*1e-3*ds,
                   node[face[a].Node[2]].Z+face[a].Side[3].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[6]].X+face[a].Side[4].X*1e-3*ds,
                   node[face[a].Node[6]].Y+face[a].Side[4].Y*1e-3*ds,
                   node[face[a].Node[6]].Z+face[a].Side[4].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[3]].X+face[a].Side[2].X*1e-3*ds,
                   node[face[a].Node[3]].Y+face[a].Side[2].Y*1e-3*ds,
                   node[face[a].Node[3]].Z+face[a].Side[2].Z*1e-3*ds);
        glVertex3d(node[face[a].Node[7]].X+face[a].Side[1].X*1e-3*ds,
                   node[face[a].Node[7]].Y+face[a].Side[1].Y*1e-3*ds,
                   node[face[a].Node[7]].Z+face[a].Side[1].Z*1e-3*ds);
        glEnd;
      end;

    end; // case
  end; // for

  glGetIntegerv(GL_CULL_FACE_MODE,ipuf);
  if ipuf[0]=GL_FRONT then
    ds*=-1;
end;

procedure DrawElemNodes(const num: Integer; node: TNodeDynArray;
  elem: TIntegerDynArray; const col: Integer; const _type: Char);
var
  i: Integer;
begin
  glLineWidth(1);
  glPointSize(4);
  with COLOR_ENTITY[col] do
    glColor3f(r,g,b);
  for i:=0 to num-1 do begin
    glBegin(GL_POINTS);
    glVertex3d(node[elem[i]].x,node[elem[i]].y,node[elem[i]].z);
    glEnd;
  end;
end;

var
  ipuf: array[0..1] of GLint;

procedure DrawElements(const num: Integer; name: TIntegerDynArray;
  node: TNodeDynArray; elem: TElementDynArray; const col: Integer);
var
  i,a: Integer;
begin
  glLineWidth(1.0);
  glGetIntegerv(GL_POLYGON_MODE,ipuf);
  SetLightAndMaterial(col);
  glEnable(GL_LIGHTING);
  for i:=0 to num-1 do begin
    a:=name[i];
    if ((elem[a].Category in [ecHexa8,ecTetra4,ecHexa20..ecPenta15,ecTetra10])
    and (ipuf[1]=GL_FILL)) then
      glEnable(GL_CULL_FACE)
    else
      glDisable(GL_CULL_FACE);
    case elem[a].Category of
      ecHexa8: begin
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
          glVertex3dv ( @node[elem[a].Node[6]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
          glVertex3dv ( @node[elem[a].Node[6]].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
          glVertex3dv ( @node[elem[a].Node[7]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
          glVertex3dv ( @node[elem[a].Node[7]].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[4].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[5].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glVertex3dv ( @node[elem[a].Node[6]].X );
          glVertex3dv ( @node[elem[a].Node[7]].X );
        glEnd();
      end;

      ecPenta6: begin
        glBegin ( GL_TRIANGLES      );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
        glEnd();
        glBegin ( GL_TRIANGLES      );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[4].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
        glEnd();
      end;

      ecTetra4: begin
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
        glEnd();
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
        glEnd();
      end;

      ecHexa20: begin
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[21]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glNormal3dv ( @elem[a].Side[4].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glNormal3dv ( @elem[a].Side[5].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glNormal3dv ( @elem[a].Side[6].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glNormal3dv ( @elem[a].Side[7].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[8].X );
          glVertex3dv ( @node[elem[a].Node[22]].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glNormal3dv ( @elem[a].Side[9].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[10].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glNormal3dv ( @elem[a].Side[11].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glNormal3dv ( @elem[a].Side[12].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glNormal3dv ( @elem[a].Side[13].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glNormal3dv ( @elem[a].Side[14].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glNormal3dv ( @elem[a].Side[15].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[16].X );
          glVertex3dv ( @node[elem[a].Node[23]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glNormal3dv ( @elem[a].Side[17].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glNormal3dv ( @elem[a].Side[18].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glNormal3dv ( @elem[a].Side[19].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glNormal3dv ( @elem[a].Side[20].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glNormal3dv ( @elem[a].Side[21].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[22].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glNormal3dv ( @elem[a].Side[23].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[24].X );
          glVertex3dv ( @node[elem[a].Node[24]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glNormal3dv ( @elem[a].Side[25].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glNormal3dv ( @elem[a].Side[26].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glNormal3dv ( @elem[a].Side[27].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[28].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glNormal3dv ( @elem[a].Side[29].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glNormal3dv ( @elem[a].Side[30].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glNormal3dv ( @elem[a].Side[31].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[32].X );
          glVertex3dv ( @node[elem[a].Node[25]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glNormal3dv ( @elem[a].Side[33].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glNormal3dv ( @elem[a].Side[34].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glNormal3dv ( @elem[a].Side[35].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glNormal3dv ( @elem[a].Side[36].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glNormal3dv ( @elem[a].Side[37].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glNormal3dv ( @elem[a].Side[38].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glNormal3dv ( @elem[a].Side[39].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
        glEnd();
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[40].X );
          glVertex3dv ( @node[elem[a].Node[20]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glNormal3dv ( @elem[a].Side[41].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glNormal3dv ( @elem[a].Side[42].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glNormal3dv ( @elem[a].Side[43].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glNormal3dv ( @elem[a].Side[44].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glNormal3dv ( @elem[a].Side[45].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glNormal3dv ( @elem[a].Side[46].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glNormal3dv ( @elem[a].Side[47].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
        glEnd();
      end;

      ecPenta15: begin
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glNormal3dv ( @elem[a].Side[4].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glNormal3dv ( @elem[a].Side[5].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glNormal3dv ( @elem[a].Side[6].X );
          glVertex3dv ( @node[elem[a].Node[15]].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glNormal3dv ( @elem[a].Side[7].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[8].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glNormal3dv ( @elem[a].Side[9].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glNormal3dv ( @elem[a].Side[10].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glNormal3dv ( @elem[a].Side[11].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glNormal3dv ( @elem[a].Side[12].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glNormal3dv ( @elem[a].Side[13].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glNormal3dv ( @elem[a].Side[14].X );
          glVertex3dv ( @node[elem[a].Node[16]].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glNormal3dv ( @elem[a].Side[15].X );
          glVertex3dv ( @node[elem[a].Node[10]].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glNormal3dv ( @elem[a].Side[16].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glNormal3dv ( @elem[a].Side[17].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glNormal3dv ( @elem[a].Side[18].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glNormal3dv ( @elem[a].Side[19].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glNormal3dv ( @elem[a].Side[20].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glNormal3dv ( @elem[a].Side[21].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glNormal3dv ( @elem[a].Side[22].X );
          glVertex3dv ( @node[elem[a].Node[17]].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glNormal3dv ( @elem[a].Side[23].X );
          glVertex3dv ( @node[elem[a].Node[11]].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glNormal3dv ( @elem[a].Side[24].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glNormal3dv ( @elem[a].Side[25].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glNormal3dv ( @elem[a].Side[26].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glNormal3dv ( @elem[a].Side[27].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glNormal3dv ( @elem[a].Side[28].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glNormal3dv ( @elem[a].Side[29].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glNormal3dv ( @elem[a].Side[30].X );
          glVertex3dv ( @node[elem[a].Node[18]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glNormal3dv ( @elem[a].Side[31].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glNormal3dv ( @elem[a].Side[32].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glNormal3dv ( @elem[a].Side[33].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glNormal3dv ( @elem[a].Side[34].X );
          glVertex3dv ( @node[elem[a].Node[12]].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[35].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[36].X );
          glVertex3dv ( @node[elem[a].Node[13]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glNormal3dv ( @elem[a].Side[37].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glNormal3dv ( @elem[a].Side[38].X );
          glVertex3dv ( @node[elem[a].Node[19]].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[39].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glVertex3dv ( @node[elem[a].Node[14]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
        glEnd();
      end;

      ecTetra10: begin
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[4].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glNormal3dv ( @elem[a].Side[5].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glNormal3dv ( @elem[a].Side[6].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glNormal3dv ( @elem[a].Side[7].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[8].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glNormal3dv ( @elem[a].Side[9].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glNormal3dv ( @elem[a].Side[10].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glNormal3dv ( @elem[a].Side[11].X );
          glVertex3dv ( @node[elem[a].Node[ 9]].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[12].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glNormal3dv ( @elem[a].Side[13].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glNormal3dv ( @elem[a].Side[14].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glNormal3dv ( @elem[a].Side[15].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
        glEnd();
      end;

      ecTria3: begin
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
        glEnd();
      end;

      ecTria6: begin
        glBegin ( GL_TRIANGLES );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
          glVertex3dv ( @node[elem[a].Node[5]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[4]].X );
        glEnd();
      end;

      ecQuad4: begin
        glBegin ( GL_TRIANGLE_STRIP );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
          glVertex3dv ( @node[elem[a].Node[3]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
        glEnd();
      end;

      ecQuad8: begin
        glBegin ( GL_TRIANGLE_FAN );
          glNormal3dv ( @elem[a].Side[0].X );
          glVertex3dv ( @node[elem[a].Node[ 8]].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
          glVertex3dv ( @node[elem[a].Node[ 4]].X );
          glNormal3dv ( @elem[a].Side[1].X );
          glVertex3dv ( @node[elem[a].Node[ 1]].X );
          glNormal3dv ( @elem[a].Side[2].X );
          glVertex3dv ( @node[elem[a].Node[ 5]].X );
          glNormal3dv ( @elem[a].Side[3].X );
          glVertex3dv ( @node[elem[a].Node[ 2]].X );
          glNormal3dv ( @elem[a].Side[4].X );
          glVertex3dv ( @node[elem[a].Node[ 6]].X );
          glNormal3dv ( @elem[a].Side[5].X );
          glVertex3dv ( @node[elem[a].Node[ 3]].X );
          glNormal3dv ( @elem[a].Side[6].X );
          glVertex3dv ( @node[elem[a].Node[ 7]].X );
          glNormal3dv ( @elem[a].Side[7].X );
          glVertex3dv ( @node[elem[a].Node[ 0]].X );
        glEnd();
      end;

      ecSeg2: begin
        glBegin ( GL_LINES );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
        glEnd();
      end;

      ecSeg3: begin
        glBegin ( GL_LINE_STRIP );
          glVertex3dv ( @node[elem[a].Node[0]].X );
          glVertex3dv ( @node[elem[a].Node[2]].X );
          glVertex3dv ( @node[elem[a].Node[1]].X );
        glEnd();
      end;

    end; // case
  end; // for
  glDisable(GL_LIGHTING);
end;

end.


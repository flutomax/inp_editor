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

unit uAnimateBackgound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Forms, Graphics, LCLType, FPImage,
  GraphType, IntfGraphics;

type

  TMatrix = array of array of Double;

  { TMap }

  TMap = class(TFPPalette)
  public
    function ColorSmooth(const Value: Double): TFPColor;
  end;

  TBackgoundStage = (bgsNone,bgsFadeIn,bgsFadeOut);

  { TAnimateBackgound }

  TAnimateBackgound = class(TComponent)
  private
    fRuleStr: string;
    fMatrix: TMatrix;
    fBuffer: TBitmap;
    fIntf: TLazIntfImage;
    fMap: TMap;
    fValMin: Double;
    fValMax: Double;
    fStep: Integer;
    fCount: Integer;
    fVariant: Integer;
    fAlpha: Word;
    fStage: TBackgoundStage;
    fOnDraw: TNotifyEvent;
    function CalcRule(const x, y: Double): Double;
    procedure DoNextStep(Sender: TObject; var Done: Boolean);
    procedure NextVariant;
    procedure MakeMap;
    procedure MakeFrame;
    procedure MakeMatrix;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Buffer: TBitmap read fBuffer;
    property OnDraw: TNotifyEvent read fOnDraw write fOnDraw;
  end;

implementation

uses
  Math, StrUtils;

const
  S_RULES: array[0..29] of string = (
  'BqWLRFPw','eyGdLaR','aQpObesKc','BwJc','TMOYaVslmGN','GIpmfmvplp','jGrpN',
  'KA','RtI','xXYXOFHHYQW','pMRtmAe','wnXPVnBcQ','XN','hnGOVPGewe','Me',
  'yVLHnQYw','NuTGXhl','Bg','Mel','ebsXCqyTf','mNDHBB','fhXNggmvDxn',
  'FLmJeh','PmfsbtF','uJxUeQgQSYt','ffQ','WCEmOOYUb','Nt','LbMtBX','QMsAH');



function FMod(const a, b: Double): Double; inline;
begin
  result:=a-b*Int(a/b);
end;

{ TMap }

function TMap.ColorSmooth(const Value: Double): TFPColor;
var
  n1,n2: Byte;
  r: Double;
  c1,c2: TFPColor;
begin
  r:=Frac(Value);
  n1:=EnsureRange(Trunc(Value),0,Count-1);
	n2:=EnsureRange(n1+1,0,Count-1);
  c1:=Color[n1];
  c2:=Color[n2];
  result.red:=Round(c1.red+((c2.red-c1.red)*r));
  result.green:=Round(c1.green+((c2.green-c1.green)*r));
  result.blue:=Round(c1.blue+((c2.blue-c1.blue)*r));
  result.alpha:=alphaOpaque;
end;

{ TAnimateBackgound }


constructor TAnimateBackgound.Create(AOwner: TComponent);
var
  c: TWinControl;
begin
  inherited Create(AOwner);
  if not (AOwner is TWinControl) then
    raise Exception.Create('Owner must be TWinControl only!');
  c:=TWinControl(AOwner);
  fVariant:=-1;
  fCount:=0;
  fStep:=0;
  fStage:=bgsFadeIn;
  fAlpha:=alphaTransparent;
  SetLength(fMatrix,c.Width,c.Height);
  fBuffer:=TBitmap.Create;
  fBuffer.SetSize(c.Width,c.Height);
  fIntf:=fBuffer.CreateIntfImage;
  fIntf.FillPixels(colWhite);
  fMap:=TMap.Create(256);
  MakeMap;
  NextVariant;
  MakeFrame;
end;

destructor TAnimateBackgound.Destroy;
begin
  fMatrix:=nil;
  fBuffer.Free;
  fIntf.Free;
  fMap.Free;
  inherited Destroy;
end;

procedure TAnimateBackgound.Start;
begin
  Application.OnIdle:=@DoNextStep;
end;

procedure TAnimateBackgound.Stop;
begin
  Application.OnIdle:=nil;
end;

function TAnimateBackgound.CalcRule(const x, y: Double): Double;

  function CheckZero(const a: Double): Double;
  begin
    if IsZero(a) then
      result:=1E-7*IfThen(a<0,-1,1)
    else
      result:=a;
  end;

var
  i: integer;
begin
  result:=0.0;
  for i:=1 to Length(fRuleStr) do begin
    case fRuleStr[i] of
      'A': result:=result+sin(x*x+y*y);
      'B': result:=result+sin(x*x)*cos(y*y);
      'C': result:=result+sin(x/CheckZero(y))*cos(x/CheckZero(y));
      'D': result:=result+cos(x/CheckZero(y));
      'E': result:=result+sin(y/CheckZero(x));
      'F': result:=result+abs(y)-x;
      'G': result:=result+x+abs(y);
      'H': result:=result+abs(x);
      'I': result:=result+abs(y);
      'J': result:=result+abs(x)*abs(y);
      'K': result:=result+sin(x)*cos(y);
      'L': result:=result+sin(x*y)*cos(x*y);
      'M': result:=result+sin(sqr(abs(x)))-cos(sqr(abs(y)));
      'N': result:=result+sin(x*x-y*y);
      'O': result:=result+y-abs(x);
      'P': result:=result+abs(x)+y;
      'Q': result:=result+cos(x)*sin(y)*cos(x*y);
      'R': result:=result+sin(cos(x)*abs(y)*abs(y));
      'S': result:=result+sin(x*x*x-y*y*y);
      'T': result:=result+sin(y*y*y)+sin(x*x*x);
      'U': result:=result+cos(y*y*y+x*x*x);
      'V': result:=result+cos(y*y*y)+cos(x*x*x);
      'W': result:=result+abs(y*3);
      'X': result:=result+abs(x*3);
      'Y': result:=result+sin(x*x/CheckZero(y)-y*y/CheckZero(x));
      'Z': result:=result+cos(x*x/CheckZero(y))+sin(y*y/CheckZero(x));
      'a': result:=result+sin(x)+sin(x)+cos(y)+cos(y);
      'b': result:=result+cos(x)+cos(x)+sin(y)+sin(y);
      'c': result:=result+sin(x)+cos(x)+sin(y)+cos(y);
      'd': result:=result*cos(y)+sin(y)+cos(x)+sin(x);
      'e': result:=result-tan(cos(sqrt(x*y*x*y)));
      'f': result:=result*y-sin(x);
      'g': result:=result*x-cos(y);
      'h': result:=result*sqrt(abs(x)+abs(y));
      'i': result:=result*sin(x*y*x)+cos(y*x*y);
      'j': result:=result+sin(x*x);
      'k': result:=result+sqr(cos(x)+sqr(x)*sin(y)+sqr(y));
      'l': result:=result*sin(result)*cos(x)*sin(x*y);
      'm': result:=result*sin(result)*cos(y)*sin(x*y);
      'n': result:=result+sin(x+y*x*y+x*x);
      'o': result:=result+sin(y+x*y*x+y*y);
      'p': result:=result+abs(x*y+x*x+y*y);
      'q': result:=result+((x+y)*y*x*sin(x)*cos(y));
      'r': result:=result+((x+y*x)+sin(x*y)+cos(y/CheckZero(x)));
      's': result:=result+sin(x*y+x)+cos(y*x+y);
      't': result:=result*cos(x+y)*sin(x+y)/2;
      'u': result:=result+cos(sqr(x+y))*y+sqr(cos(y)*sin(x));
      'v': result:=result+sin(sqr(y+x))*x+sqr(sin(x)*cos(y));
      'w': result:=result+cos(x)*sin(x)+cos(y)*sin(y);
      'x': result:=result+sqr(sin(sqr(x)/CheckZero(sqr(y))));
      'y': result:=result+sin(abs(cos(x))+abs(sin(y)));
      'z': result:=result+sin(abs(cos(x+y))+abs(cos(y*x*y)));
    end;
  end;
end;

procedure TAnimateBackgound.DoNextStep(Sender: TObject; var Done: Boolean);
const
  AlphaStep = 8192;
  AlphaStep2 = 4096;
  AlphaHi = alphaOpaque-AlphaStep;
begin
  case fStage of
    bgsFadeIn: begin
      Inc(fAlpha,AlphaStep);
      if fAlpha>=AlphaHi then begin
        fAlpha:=alphaOpaque;
        fStage:=bgsNone;
      end
    end;
    bgsFadeOut: begin
      if fAlpha>AlphaStep then
        Dec(fAlpha,AlphaStep)
      else
        Dec(fAlpha,AlphaStep2);
      if fAlpha<=AlphaStep2 then begin
        fAlpha:=alphaTransparent;
        fStage:=bgsFadeIn;
        NextVariant;
      end
    end
  end;
  MakeFrame;
  inc(fStep);
  if fStep>=256 then begin
    fStep:=0;
    Inc(fCount);
  end;
  if fCount>1 then begin
    fCount:=0;
    fStage:=bgsFadeOut;
  end;
  Done:=false;
end;

procedure TAnimateBackgound.NextVariant;
var
  n: Integer;
begin
  // Always new variant! :^)
  repeat
  n:=Random(High(S_RULES)+1);
  until n<>fVariant;
  fRuleStr:=S_RULES[n];
  fVariant:=n;
  MakeMatrix;
end;

procedure TAnimateBackgound.MakeMap;
const
  k = 360/255;
  eHigh = 255;
  eLow = 239;
  repe = 7;
var
  i,x,h: Integer;
  v: Double;
begin
  h:=eHigh-eLow;
  fMap.Clear;
  for i:=0 to 255 do begin
    v:=repe*degtorad(k*i);
    v:=0.5+Cos(v)*0.5;
    x:=eLow+Round(v*h);
    fMap.Add(TColorToFPColor(RGBToColor(x,x,x)));
  end;
end;

procedure TAnimateBackgound.MakeFrame;
var
  i,j: Integer;
  r,v,w: Double;
  c: TFPColor;
begin
  if fValMax-fValMin=0 then
    Exit;
  r:=256/(fValMax-fValMin);
  for j:=0 to fBuffer.Height-1 do begin
    for i:=0 to fBuffer.Width-1 do begin
      w:=FMod(fStep+(fMatrix[i,j]-fValMin)*r,256);
      c:=fMap.ColorSmooth(w);
      c.alpha:=fAlpha;
      fIntf.Colors[i,j]:=AlphaBlend(colWhite,c);
    end;
  end;

  fBuffer.LoadFromIntfImage(fIntf);
  if Assigned(fOnDraw) then
    fOnDraw(self);
end;

procedure TAnimateBackgound.MakeMatrix;
const
  edScale = 3.75;
var
  xMin,xMax,yMin,yMax,
  xDelta,yDelta,
  x,y: double;
  i,j: Integer;
begin
  fValMin:=MaxInt;
  fValMax:=-MaxInt;
  xMin:=-edScale;
  xMax:=edScale;
  yMin:=-edScale;
  yMax:=edScale;
  xDelta:=(xMax-xMin)/fBuffer.Width;
  yDelta:=(yMax-yMin)/fBuffer.Height;
  for j:=0 to fBuffer.Height-1 do begin
    y:=yMax-j*yDelta;
     for i:=0 to fBuffer.Width-1 do begin
       x:=xMin+i*xDelta;
       fMatrix[i,j]:=CalcRule(x,y);
       fValMin:=Min(fValMin,fMatrix[i,j]);
       fValMax:=Max(fValMax,fMatrix[i,j]);
     end;
  end;
end;


end.


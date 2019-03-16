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

unit uFrmModelViewer;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  IntfGraphics, ExtCtrls, StdCtrls, ComCtrls, ActnList, ButtonPanel, GL,
  FPimage, OpenGLContext, Menus, uInpTypes, uInpTranslator, uInpEditor;

type

  TViewTool = (vtHand, vtRota, vtZoom);

  { TFrmModelViewer }

  TFrmModelViewer = class(TForm)
    pnBottom: TButtonPanel;
    cmdViewToolZoom: TAction;
    cmdViewToolRotation: TAction;
    cmdViewToolHand: TAction;
    cmdViewElementEdges: TAction;
    cmdViewSolid: TAction;
    alMain: TActionList;
    ilMain: TImageList;
    lvStat: TListView;
    mmLog: TMemo;
    pnErrorLog: TPanel;
    pnGroupsTitle: TPanel;
    pnGrpView: TPanel;
    pnGlWnd: TPanel;
    pnLogTitle: TPanel;
    pnRight: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ToolBar2: TToolBar;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    pmView: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    procedure cmdViewElementEdgesExecute(Sender: TObject);
    procedure cmdViewToolHandExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvStatSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    fGlWnd: TOpenGLControl;
    fGlInitialized: Boolean;
    fTool: TViewTool;
    fMouseSpX: Double;
    fMouseSpY: Double;
    fCurQuat: TQuat;
    fLastQuat: TQuat;
    fVec: TQuat;
    fVecMem: TQuat;
    fRotMatrix: TQuatMatrix;
    fDifMatrix: TQuatMatrix;
    fMemMatrix: TQuatMatrix;
    fRatio: Double;
    fScale: Double;
    fDtPt: TPoint3D;
    fBgColor: TQuat;
    fFgColor: TQuat;
    fInpFile: TInpFile;
    fListSurfLight: GLuint;
    fListSurfLoad: GLuint;
    fListSurfEdges: GLuint;
    fListModelEdges: GLuint;
    procedure GlWndPaint(Sender: TObject);
    procedure GlWndResize(Sender: TObject);
    procedure GlWndMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GlWndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GlWndMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GlWndMouseWhell(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MoveScene;
    procedure UpdateDispLists;
    procedure DrawDispList(const list: GLuint);
    procedure DrawModelEdges(const list: GLuint);
    procedure DrawDispListEdges(const list: GLuint);
    procedure DrawGraficLight;
    procedure Redraw;
    procedure Render;
    procedure UpdateStat;
  public
    procedure Parse(Data: PtrInt);
    property InpFile: TInpFile read fInpFile;
  end;

var
  FrmModelViewer: TFrmModelViewer;

  procedure ShowModelViewer(aEditor: TInpEditor);

implementation

{$R *.lfm}
{$R cursors.res}

uses Math, uInpFunctions;

const
  crZoom = TCursor(250);
  crRota = TCursor(251);
  crPanHand = TCursor(252);
  crPanGrab = TCursor(253);

procedure ShowModelViewer(aEditor: TInpEditor);
begin
  with TFrmModelViewer.Create(Application) do
    try
      Application.QueueAsyncCall(@Parse,{%H-}PtrInt(aEditor));
      ShowModal;
    finally
      Release;
    end;
end;

function LoadCursorFromRes(Cursor: PtrInt): THandle;
var
  Cur: TCursorImage;
begin
  Cur:=TCursorImage.Create;
  try
    Cur.LoadFromResourceID(HInstance,Cursor);
    result:=Cur.ReleaseHandle;
  finally
    Cur.Free;
  end;
end;

{ TFrmModelViewer }

procedure TFrmModelViewer.FormCreate(Sender: TObject);
begin
  pnBottom.Color:=clForm; // correct button panel color
  fMouseSpX:=0;
  fMouseSpY:=0;
  fTool:=vtHand;
  FillChar(fCurQuat,SizeOf(TQuat),0);
  FillChar(fLastQuat,SizeOf(TQuat),0);
  FillChar(fVec,SizeOf(TQuat),0);
  FillChar(fVecMem,SizeOf(TQuat),0);
  FillChar(fRotMatrix,SizeOf(TQuatMatrix),0);
  FillChar(fDifMatrix,SizeOf(TQuatMatrix),0);
  FillChar(fMemMatrix,SizeOf(TQuatMatrix),0);
  FillChar(fDtPt,SizeOf(TPoint3D),0);
  fBgColor[0]:=1.0;
  fBgColor[1]:=1.0;
  fBgColor[2]:=1.0;
  fBgColor[3]:=1.0;
  fFgColor[0]:=0.0;
  fFgColor[1]:=0.0;
  fFgColor[2]:=0.0;
  fFgColor[3]:=1.0;
  fScale:=0.5;
  fGlInitialized:=false;
  Screen.Cursors[crZoom]:=LoadCursorFromRes(crZoom);
  Screen.Cursors[crRota]:=LoadCursorFromRes(crRota);
  Screen.Cursors[crPanHand]:=LoadCursorFromRes(crPanHand);
  Screen.Cursors[crPanGrab]:=LoadCursorFromRes(crPanGrab);
  fInpFile:=TInpFile.Create;
  fGlWnd:=TOpenGLControl.Create(Self);
  fGlWnd.Name:='GlWnd';
  fGlWnd.AutoResizeViewport:=true;
  fGlWnd.Parent:=pnGlWnd;
  fGlWnd.PopupMenu:=pmView;
  fGlWnd.Align:=alClient;
  fGlWnd.MultiSampling:=4;
  fGlWnd.Cursor:=crPanHand;
  fGlWnd.OnPaint:=@GlWndPaint;
  fGlWnd.OnResize:=@GlWndResize;
  fGlWnd.OnMouseDown:=@GlWndMouseDown;
  fGlWnd.OnMouseUp:=@GlWndMouseUp;
  fGlWnd.OnMouseMove:=@GlWndMouseMove;
  fGlWnd.OnMouseWheel:=@GlWndMouseWhell;
end;

procedure TFrmModelViewer.FormDestroy(Sender: TObject);
begin
  fInpFile.Free;
end;

procedure TFrmModelViewer.Parse(Data: PtrInt);
var
  aEditor: TInpEditor;
  gl_max_eval_order_val: Integer;
begin
  Screen.Cursor:=crHourGlass;
  try
    Application.ProcessMessages;
    aEditor:=TInpEditor(Data);
    gl_max_eval_order_val:=8;
    fScale:=0.5;
    InpFile.Parse(aEditor.Lines,aEditor.FileName);
    Trackball(fLastQuat,-0.2,-0.7,0.2,0.7);
    BuildRotMatrix(fRotMatrix,fLastQuat);
    glDisable(GL_DEPTH_TEST);
    glMatrixMode(GL_MODELVIEW);
    glDisable(GL_DITHER);
    fListSurfLight:=glGenLists(1);
    fListSurfLoad:=glGenLists(1);
    fListSurfEdges:=glGenLists(1);
    fListModelEdges:=glGenLists(1);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    glFrontFace(GL_CCW);
    glGetIntegerv(GL_MAX_EVAL_ORDER,@gl_max_eval_order_val);
    glDisable(GL_DITHER);
    glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE,LMODEL_ONESIDE);
    glCullFace(GL_BACK);
    InitLightAndMaterial;
    glDisable(GL_DITHER);
    drawMode:=2;
    Redraw;
    UpdateStat;
    mmLog.Lines.Assign(InpFile.Log);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFrmModelViewer.cmdViewElementEdgesExecute(Sender: TObject);
begin
  Redraw;
end;

procedure TFrmModelViewer.GlWndPaint(Sender: TObject);
begin
  if fGlWnd.MakeCurrent then begin
    Redraw;
  end;
end;

procedure TFrmModelViewer.GlWndResize(Sender: TObject);
begin
  fRatio:=fGlWnd.Width/Max(1,fGlWnd.Height);
end;

procedure TFrmModelViewer.GlWndMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if fTool=vtHand then
    fGlWnd.Cursor:=crPanGrab;
end;

procedure TFrmModelViewer.GlWndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if fTool=vtHand then
    fGlWnd.Cursor:=crPanHand;
end;

procedure TFrmModelViewer.GlWndMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  dx, dy: Double;
begin
  dx:=-2*fRatio*(fGlWnd.Width/2-x)/Max(1,fGlWnd.Width);
  dy:=2*(fGlWnd.Height/2-y)/Max(1,fGlWnd.Height);
  if (ssLeft in Shift) then begin
    case fTool of
      vtHand: begin
        fDtPt.X+=(dx-fMouseSpX);
        fDtPt.Y+=(dy-fMouseSpY);
        Redraw;
      end;

      vtRota: begin
        Trackball(fCurQuat,fMouseSpX,fMouseSpY,dx,dy);
        AddQuats(fCurQuat,fLastQuat,fLastQuat);
        BuildRotMatrix(fRotMatrix,fLastQuat);
        Redraw;
      end;

      vtZoom: begin
        fDtPt.X*=fScale;
        fDtPt.Y*=fScale;
        fScale-=(fMouseSpY-dy)*fScale;
        fDtPt.X/=fScale;
        fDtPt.Y/=fScale;
        Redraw;
      end;
    end;
  end;

  fMouseSpX:=dx;
  fMouseSpY:=dy;
end;

procedure TFrmModelViewer.GlWndMouseWhell(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  fDtPt.X*=fScale;
  fDtPt.Y*=fScale;
  if WheelDelta>0 then
    fScale-=0.05*fScale
  else
    fScale+=0.05*fScale;
  fDtPt.X/=fScale;
  fDtPt.Y/=fScale;
  Handled:=true;
  Redraw;
end;


procedure TFrmModelViewer.cmdViewToolHandExecute(Sender: TObject);
begin
  fTool:=TViewTool(TAction(Sender).Tag);
  case fTool of
    vtHand: fGlWnd.Cursor:=crPanHand;
    vtRota: fGlWnd.Cursor:=crRota;
    vtZoom: fGlWnd.Cursor:=crZoom;
  end;
end;

procedure TFrmModelViewer.MoveScene;
begin
  if fRatio>=1 then
    glOrtho(-fScale*fRatio,fScale*fRatio,-fScale,fScale,-Z_DEPTH,Z_DEPTH)
  else
    glOrtho(-fScale,fScale,-fScale/fRatio,fScale/fRatio,-Z_DEPTH,Z_DEPTH);

  fVec[0]:=0;
  fVec[1]:=0;
  fVec[2]:=0;
  fVec[3]:=1.0;
  MatrixSub(fDifMatrix,fRotMatrix,fMemMatrix);
  MatrixMul(fVec,fDifMatrix);
  glTranslated(-fVec[0]-fVecMem[0],-fVec[1]-fVecMem[1],-fVec[2]-fVecMem[2]);
  glTranslated(fDtPt.X*fScale,fDtPt.Y*fScale,fDtPt.Z);
  glMultMatrixd(@fRotMatrix[0,0]);
end;

procedure TFrmModelViewer.UpdateDispLists;
begin
  case DrawMode of
    1: DrawDispList(fListSurfLoad);
    2: DrawDispList(fListSurfLight);
  end;
    DrawModelEdges(fListModelEdges);
    DrawDispListEdges(fListSurfEdges);
end;

procedure TFrmModelViewer.DrawDispList(const list: GLuint);
begin
  glNewList(list,GL_COMPILE);
  DrawFaces(InpFile.Summen.f,InpFile.Nodes,InpFile.Faces,InpFile.ElEnqire,2,'f',0);
  glEndList;
end;

procedure TFrmModelViewer.DrawModelEdges(const list: GLuint);
var
  i: Integer;
begin
  glNewList(list,GL_COMPILE);
  glColor3d(fFgColor[0],fFgColor[1],fFgColor[2]);
  glLineWidth(2.0);
  glBegin(GL_LINES);
  for i:=0 to InpFile.Summen.g-1 do begin
    glVertex3dv(@InpFile.Nodes[InpFile.Edges[i].p1].X);
    glVertex3dv(@InpFile.Nodes[InpFile.Edges[i].p2].X);
  end;
  glEnd;
  glEndList;
end;

procedure TFrmModelViewer.DrawDispListEdges(const list: GLuint);
begin
  glNewList(list,GL_COMPILE);
  DrawFacesEdge(InpFile.Summen.f,InpFile.Nodes,InpFile.Faces,0,'f',fScale);
  glEndList;
end;

procedure TFrmModelViewer.DrawGraficLight;
begin
  glCallList(fListSurfLight);
  glCallList(fListModelEdges);
end;

procedure TFrmModelViewer.Render;
begin
  drawMode:=4;
  UpdateDispLists;
  DrawFaces(InpFile.Summen.f,InpFile.Nodes,InpFile.Faces,InpFile.ElEnqire,2,'f',0);
  glCallList(fListModelEdges);
end;

procedure TFrmModelViewer.Redraw;
var
  i: Integer;
begin

  glClearColor(fBgColor[0],fBgColor[1],fBgColor[2],fBgColor[3]);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  InitLightAndMaterial;
  MoveScene;

  if DrawMode=1 then begin
    UpdateDispLists;
    glCallList(fListModelEdges);
    for i:=1 to lvStat.Items.Count-1 do begin
      if not lvStat.Items[i].Selected then
        continue;
      if InpFile.Sets[i-1].NumNodes>0 then begin
        DrawElemNodes(InpFile.Sets[i-1].NumNodes,InpFile.Nodes,
          InpFile.Sets[i-1].Nodes,3,'n');
      end
      else if InpFile.Sets[i-1].NumElements>0 then begin
        DrawElements(InpFile.Sets[i-1].NumElements,InpFile.Sets[i-1].Elements,
          InpFile.Nodes,InpFile.ElEnqire,4);
      end;

    end;
  end
  else
  if DrawMode=2 then begin
    Render;
    DrawMode:=2;
    DrawGraficLight;
  end;
  if cmdViewElementEdges.Checked then begin
    DrawFacesEdge(InpFile.Summen.f,InpFile.Nodes,InpFile.Faces,0,'f',fScale);
  end;
  fGlWnd.SwapBuffers;
end;


procedure TFrmModelViewer.UpdateStat;
var
  i: integer;
  t: TSet;
  m: TListItem;
begin
  lvStat.Items.BeginUpdate;
  try
    lvStat.Items.Clear;
    m:=lvStat.Items.Add;
    m.Caption:='1';
    m.SubItems.Add('All');
    m.SubItems.Add('%d',[InpFile.Summen.n]);
    m.SubItems.Add('%d',[InpFile.Summen.e]);
    m.SubItems.Add('%d',[InpFile.Summen.f]);
    for i:=0 to InpFile.Summen.sets-1 do begin
      t:=InpFile.Sets[i];
      if AnsiSameText(t.Name,'all') then // ignore all set
        continue;
      m:=lvStat.Items.Add;
      m.Caption:=IntToStr(m.Index+1);
      m.SubItems.Add(t.Name);
      m.SubItems.Add('%d',[t.NumNodes]);
      m.SubItems.Add('%d',[t.NumElements]);
      m.SubItems.Add('%d',[t.NumFaces]);
    end;
  finally
    lvStat.Items.EndUpdate;
  end;
end;

procedure TFrmModelViewer.lvStatSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Selected then
    Exit;
  if Assigned(Item) then begin
    if (lvStat.SelCount=1) and (Item.Index=0) then
      drawMode:=2
    else
      drawMode:=1;
    Redraw;
  end;
end;


end.




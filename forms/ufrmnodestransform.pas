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

unit uFrmNodesTransform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, LCLType, uInpEditor, uNodeTransform, Types;

type

  { TFrmNodesTransform }

  TFrmNodesTransform = class(TForm)
    BtnClearAll: TButton;
    pnBottom: TButtonPanel;
    cbType: TComboBox;
    edDx: TEdit;
    edDy: TEdit;
    edDz: TEdit;
    edRxAlpha: TEdit;
    edRxCy: TEdit;
    edRxCz: TEdit;
    edRyAlpha: TEdit;
    edRyCx: TEdit;
    edRyCz: TEdit;
    edRzAlpha: TEdit;
    edRzCx: TEdit;
    edRzCy: TEdit;
    edSx: TEdit;
    edSy: TEdit;
    edSz: TEdit;
    gbInfo: TGroupBox;
    gbAffineTransform: TGroupBox;
    gbOperation: TGroupBox;
    lbOpNum: TListBox;
    lbAlphaInfo: TLabel;
    lbAlphaInfo1: TLabel;
    lbAlphaInfo2: TLabel;
    lbDx: TLabel;
    lbDy: TLabel;
    lbDz: TLabel;
    LblNoOperation1: TLabel;
    LblType: TLabel;
    lbRxAlpha: TLabel;
    lbRxCy: TLabel;
    lbRxCz: TLabel;
    lbRyAlpha: TLabel;
    lbRyCx: TLabel;
    lbRyCz: TLabel;
    lbRzAlpha: TLabel;
    lbRzCx: TLabel;
    lbRzCy: TLabel;
    lbSx: TLabel;
    lbSy: TLabel;
    lbSz: TLabel;
    pcType: TNotebook;
    pgNone: TPage;
    pgTranslate: TPage;
    pgScale: TPage;
    pgRotateX: TPage;
    pgRotateY: TPage;
    pgRotateZ: TPage;
    mmInfo: TMemo;
    procedure BtnClearAllClick(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure edDxChange(Sender: TObject);
    procedure edDxKeyPress(Sender: TObject; var Key: char);
    procedure edRxAlphaChange(Sender: TObject);
    procedure edRyAlphaChange(Sender: TObject);
    procedure edRzAlphaChange(Sender: TObject);
    procedure edSxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbOpNumClick(Sender: TObject);
    procedure lbOpNumDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OKButtonClick(Sender: TObject);
  private
    fEditor: TInpEditor;
    fNodes: TNodeProcessor;
    fOperation: TOpRecs;
    fCurrent: POpRec;
    fLoading: Boolean;
    procedure ClearTransforms;
    procedure ShowSettings(const OperationNum: Integer);
    procedure DoTransform;
  public
    procedure Parse(Data: PtrInt);
  end;

var
  FrmNodesTransform: TFrmNodesTransform;

  function ApplyNodesTransform(Editor: TInpEditor): Boolean;

implementation

{$R *.lfm}

uses
  uEditorMisc, uConsts;

function ApplyNodesTransform(Editor: TInpEditor): Boolean;
begin
  with TFrmNodesTransform.Create(Application) do
    try
      fEditor:=Editor;
      Application.QueueAsyncCall(@Parse,0);
      Result:=ShowModal=mrOK;
    finally
      Release;
    end;
end;

{ TFrmNodesTransform }

procedure TFrmNodesTransform.FormCreate(Sender: TObject);
begin
  fNodes:=TNodeProcessor.Create;
  pnBottom.Color:=clForm; // correct button panel color
  CentredLabels(self);
  ClearTransforms;
  ShowSettings(0);
end;

procedure TFrmNodesTransform.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fNodes);
end;

procedure TFrmNodesTransform.Parse(Data: PtrInt);
begin
  Screen.Cursor:=crHourGlass;
  try
    fNodes.Parse(fEditor.Lines);
    mmInfo.Lines.BeginUpdate;
    try
      mmInfo.Lines.Clear;
      mmInfo.Lines.Add('Nodes Count %d, Max Node # %d',
        [fNodes.Count,fNodes[fNodes.Count-1].N]);
      mmInfo.Lines.Add('Bound Box: Width %s, Height %s, Depth %s',[
        ExtendedToStr(fNodes.BoundBox.Width),
        ExtendedToStr(fNodes.BoundBox.Height),
        ExtendedToStr(fNodes.BoundBox.Depth)]);
    finally
      mmInfo.Lines.EndUpdate;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFrmNodesTransform.lbOpNumClick(Sender: TObject);
begin
  ShowSettings(lbOpNum.ItemIndex);
end;

procedure TFrmNodesTransform.lbOpNumDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  s: string;
  h: Integer;
begin
  if odSelected in State then begin
    lbOpNum.Canvas.Brush.Color:=clHighlight;
    lbOpNum.Canvas.Font.Color:=clHighlightText;
  end else begin
    lbOpNum.Canvas.Brush.Color:=Control.Color;
    lbOpNum.Canvas.Font.Color:=clWindowText;
  end;
  lbOpNum.Canvas.FillRect(ARect);
  s:=lbOpNum.Items[Index];
  h:=(ARect.Bottom-ARect.Top-lbOpNum.Canvas.TextHeight(s)) div 2;
  lbOpNum.Canvas.TextOut(ARect.Left+3,ARect.Top+h,s);
end;

procedure TFrmNodesTransform.OKButtonClick(Sender: TObject);
begin
  DoTransform;
end;

procedure TFrmNodesTransform.cbTypeChange(Sender: TObject);
begin
  fCurrent^.OpType:=OpTypes[cbType.ItemIndex];
  ShowSettings(lbOpNum.ItemIndex);
end;

procedure TFrmNodesTransform.ClearTransforms;
var
  i: Integer;
begin
  FillChar(fOperation[0],SizeOf(TOpRecs),0);
  for i:=0 to 7 do
    with fOperation[i] do begin
      Sx:=1;
      Sy:=1;
      Sz:=1;
    end;
end;

procedure TFrmNodesTransform.ShowSettings(const OperationNum: Integer);
begin
  fLoading:=true;
  try
    lbOpNum.ItemIndex:=OperationNum;
    fCurrent:=@fOperation[OperationNum];
    cbType.ItemIndex:=Ord(fCurrent^.OpType);
    pcType.PageIndex:=cbType.ItemIndex;
    edDx.Text:=ExtendedToStr(fCurrent^.Dx);
    edDy.Text:=ExtendedToStr(fCurrent^.Dy);
    edDz.Text:=ExtendedToStr(fCurrent^.Dz);
    edSx.Text:=ExtendedToStr(fCurrent^.Sx);
    edSy.Text:=ExtendedToStr(fCurrent^.Sy);
    edSz.Text:=ExtendedToStr(fCurrent^.Sz);
    edRxAlpha.Text:=ExtendedToStr(fCurrent^.RotX.Alpha);
    edRxCy.Text:=ExtendedToStr(fCurrent^.RotX.C1);
    edRxCz.Text:=ExtendedToStr(fCurrent^.RotX.C2);
    edRyAlpha.Text:=ExtendedToStr(fCurrent^.RotY.Alpha);
    edRyCx.Text:=ExtendedToStr(fCurrent^.RotY.C1);
    edRyCz.Text:=ExtendedToStr(fCurrent^.RotY.C2);
    edRzAlpha.Text:=ExtendedToStr(fCurrent^.RotZ.Alpha);
    edRzCx.Text:=ExtendedToStr(fCurrent^.RotZ.C1);
    edRzCy.Text:=ExtendedToStr(fCurrent^.RotZ.C2);
  finally
    fLoading:=false;
  end;
end;

procedure TFrmNodesTransform.DoTransform;
var
  a: TAffineTransform;
  i: Integer;
  s,nl: string;
  n: TNode3D;
begin
  Screen.Cursor:=crHourGlass;
  try
    a:=TAffineTransform.Create;
    try
      for i:=0 to 7 do
        with fOperation[i] do begin
          case OpType of
            opTranslate: a.Translate(Dx,Dy,Dz);
            opScale: a.Scale(Sx,Sy,Sz);
            opRotateX: a.RotateX(RotX.Alpha,RotX.C1,RotX.C2);
            opRotateY: a.RotateY(RotY.Alpha,RotY.C1,RotY.C2);
            opRotateZ: a.RotateZ(RotZ.Alpha,RotZ.C1,RotZ.C2);
          end;
        end;

      s:='';
      case fEditor.TextLineBreakStyle of
        tlbsLF   : nl:=#10;
        tlbsCRLF : nl:=#13#10;
        tlbsCR   : nl:=#13;
      end;
      for i:=0 to fNodes.Count-1 do begin
        a.Transform(fNodes[i],n);
        s:=Format('%s%d, %s, %s, %s%s',
          [s,n.N,ExtendedToStrExp(n.X),ExtendedToStrExp(n.Y),
          ExtendedToStrExp(n.Z),nl]);
      end;
    finally
      a.Free;
    end;
    fEditor.BlockBegin:=fNodes.BlockBegin;
    fEditor.BlockEnd:=fNodes.BlockEnd;
    fEditor.SelText:=TrimRight(s);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFrmNodesTransform.BtnClearAllClick(Sender: TObject);
begin
  ClearTransforms;
  ShowSettings(lbOpNum.ItemIndex);
end;

procedure TFrmNodesTransform.edDxKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet(Key,AllowEditNumChars) then
    Key:=#0;
end;

procedure TFrmNodesTransform.edDxChange(Sender: TObject);
begin
  if fLoading then
    Exit;
  fCurrent^.Dx:=ParseValue(edDx.Text);
  fCurrent^.Dy:=ParseValue(edDy.Text);
  fCurrent^.Dz:=ParseValue(edDz.Text);
end;

procedure TFrmNodesTransform.edSxChange(Sender: TObject);
begin
  if fLoading then
    Exit;
  fCurrent^.Sx:=ParseValue(edSx.Text);
  fCurrent^.Sy:=ParseValue(edSy.Text);
  fCurrent^.Sz:=ParseValue(edSz.Text);
end;


procedure TFrmNodesTransform.edRxAlphaChange(Sender: TObject);
begin
  if fLoading then
    Exit;
  fCurrent^.RotX.Alpha:=ParseValue(edRxAlpha.Text);
  fCurrent^.RotX.C1:=ParseValue(edRxCy.Text);
  fCurrent^.RotX.C2:=ParseValue(edRxCz.Text);
end;

procedure TFrmNodesTransform.edRyAlphaChange(Sender: TObject);
begin
  if fLoading then
    Exit;
  fCurrent^.RotY.Alpha:=ParseValue(edRyAlpha.Text);
  fCurrent^.RotY.C1:=ParseValue(edRyCx.Text);
  fCurrent^.RotY.C2:=ParseValue(edRyCz.Text);
end;

procedure TFrmNodesTransform.edRzAlphaChange(Sender: TObject);
begin
  if fLoading then
    Exit;
  fCurrent^.RotZ.Alpha:=ParseValue(edRzAlpha.Text);
  fCurrent^.RotZ.C1:=ParseValue(edRzCx.Text);
  fCurrent^.RotZ.C2:=ParseValue(edRzCy.Text);
end;

end.


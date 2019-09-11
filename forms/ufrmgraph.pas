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

unit uFrmGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ExtCtrls, StdCtrls, Spin, ActnList, uMathParser, uRender, Grids, ExtDlgs,
  Menus, LCLType, ComCtrls;

type

  { TFrmGraph }

  TFrmGraph = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    cmdFileClose: TAction;
    cmdFileNew: TAction;
    cmdHelp: TAction;
    cmdSaveGraph: TAction;
    cmdCheckList: TAction;
    cmdRenderGraph: TAction;
    cmdFileLoad: TAction;
    cmdFileSave: TAction;
    cmdCalc: TAction;
    AlMain: TActionList;
    EdExpression: TEdit;
    EdMaxTimeValue: TEdit;
    EdNumPts: TSpinEdit;
    gbExpression: TGroupBox;
    DlgLoad: TOpenDialog;
    gbParams: TGroupBox;
    LbNumTimePoints: TLabel;
    lbVariable: TLabel;
    MainMenu1: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pnControls: TPanel;
    pbGraph: TPaintBox;
    pnMain: TPanel;
    DlgSave: TSaveDialog;
    DlgSaveGraph: TSavePictureDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    VLEGraph: TValueListEditor;
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure cmdCalcExecute(Sender: TObject);
    procedure cmdCalcUpdate(Sender: TObject);
    procedure cmdCheckListExecute(Sender: TObject);
    procedure cmdFileCloseExecute(Sender: TObject);
    procedure cmdFileNewExecute(Sender: TObject);
    procedure cmdFileSaveExecute(Sender: TObject);
    procedure cmdFileSaveUpdate(Sender: TObject);
    procedure cmdFileLoadExecute(Sender: TObject);
    procedure cmdHelpExecute(Sender: TObject);
    procedure cmdRenderGraphExecute(Sender: TObject);
    procedure cmdSaveGraphExecute(Sender: TObject);
    procedure EdExpressionKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure EdMaxTimeValueKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure EdNumPtsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure pbGraphPaint(Sender: TObject);
    procedure VLEGraphKeyPress(Sender: TObject; var Key: char);
  private
    fOffScreen: TBitmap;
    function GraphNotEmpty: boolean;
    function CheckList: boolean;
    procedure SelectEditor(const index: integer; const col: integer = 0);
    procedure DrawEmptyGraph;
  public
    { public declarations }
  end;

var
  FrmGraph: TFrmGraph;

procedure ShowPlotGraph;

implementation

{$R *.lfm}

uses
  DOM, XMLRead, XMLWrite, uEditorMisc, uConsts;

type
  TGridAccess = class(TValueListEditor);

procedure ShowPlotGraph;
begin
  FrmGraph := TFrmGraph.Create(Application);
  try
    FrmGraph.ShowModal;
  finally
    FrmGraph.Release;
  end;
end;

{ TFrmGraph }

procedure TFrmGraph.FormCreate(Sender: TObject);
begin
  fOffScreen := TBitmap.Create;
  fOffScreen.PixelFormat := pf24Bit;
  CentredLabels(self);
end;

procedure TFrmGraph.FormDestroy(Sender: TObject);
begin
  fOffScreen.Free;
end;

procedure TFrmGraph.FormResize(Sender: TObject);
begin
  fOffScreen.Width := pbGraph.Width;
  fOffScreen.Height := pbGraph.Height;
  DrawEmptyGraph;
  cmdRenderGraph.Execute;
end;

procedure TFrmGraph.FormShow(Sender: TObject);
begin
  SelectEditor(0);
end;

procedure TFrmGraph.MenuItem1Click(Sender: TObject);
begin

end;

procedure TFrmGraph.cmdHelpExecute(Sender: TObject);
begin
  MessageDlg(sHelp, mtInformation, [mbOK], 0);
end;

procedure TFrmGraph.pbGraphPaint(Sender: TObject);
begin
  pbGraph.Canvas.Draw(0, 0, fOffScreen);
end;

procedure TFrmGraph.SelectEditor(const index: integer; const col: integer);
begin
  TGridAccess(VLEGraph).MoveNextSelectable(False, col, index + 1);
  if VLEGraph.Editor is TStringCellEditor then
  begin
    TStringCelleditor(VLEGraph.Editor).SelectAll;
    VLEGraph.Editor.SetFocus;
  end;
end;

procedure TFrmGraph.DrawEmptyGraph;
begin
  with fOffScreen.Canvas do
  begin
    Font.Size := 8;
    Brush.Color := clWhite;
    Pen.Color := clWindowFrame;
    Rectangle(0, 0, fOffScreen.Width, fOffScreen.Height);
  end;
end;

function TFrmGraph.CheckList: boolean;
var
  i, j, e: integer;
  d1, d2: TDouble;
begin
  Result := False;
  with VLEGraph.Strings do
  begin
    // pass1 check valid number & empty field
    for i := 0 to Count - 1 do
    begin
      if Length(Names[i]) = 0 then
      begin
        MessageDlg(sVLEEmptyField, mtWarning, [mbOK], 0);
        SelectEditor(i);
        exit;
      end;
      if Length(ValueFromIndex[i]) = 0 then
      begin
        MessageDlg(sVLEEmptyField, mtWarning, [mbOK], 0);
        SelectEditor(i, 1);
        exit;
      end;

      Val(Names[i], d1, e);
      if e <> 0 then
      begin
        MessageDlg(Format(sVLEIllegalValue, [Names[i]]), mtWarning, [mbOK], 0);
        SelectEditor(i);
        exit;
      end;
      Val(ValueFromIndex[i], d1, e);
      if e <> 0 then
      begin
        MessageDlg(Format(sVLEIllegalValue, [ValueFromIndex[i]]), mtWarning, [mbOK], 0);
        SelectEditor(i, 1);
        exit;
      end;
    end;
    // pass2 find duplicates
    for i := 0 to Count - 1 do
    begin
      for j := Count - 1 downto i + 1 do
      begin
        if CompareText(Names[j], Names[i]) = 0 then
        begin
          MessageDlg(Format(sVLEDuplicateKey, [Names[j]]), mtWarning, [mbOK], 0);
          SelectEditor(j);
          exit;
        end;
      end;
    end;
    // pass3 find not ascending order
    Val(Names[0], d1, e);
    for i := 1 to Count - 1 do
    begin
      Val(Names[i], d2, e);
      if d2 <= d1 then
      begin
        MessageDlg(sVLETimeNotAsc, mtWarning, [mbOK], 0);
        SelectEditor(i);
        exit;
      end;
      d1 := d2;
    end;
  end;
  Result := True;
end;

procedure TFrmGraph.cmdCheckListExecute(Sender: TObject);
begin
  if CheckList then
    MessageDlg(sCheckListSucc, mtInformation, [mbOK], 0);
end;

procedure TFrmGraph.cmdFileCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmGraph.cmdFileNewExecute(Sender: TObject);
begin
  VLEGraph.Strings.Clear;
  EdExpression.Text := 'sin(t*2*pi/1.0)';
  EdMaxTimeValue.Text := '1';
  EdNumPts.Value := 2;
  fOffScreen.Canvas.Clear;
  DrawEmptyGraph;
  pbGraph.Invalidate;
end;

procedure TFrmGraph.VLEGraphKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', '.', '+', '-', 'E', 'e', #8]) then
    Key := #0;
end;

procedure TFrmGraph.cmdCalcUpdate(Sender: TObject);
begin
  cmdCalc.Enabled := (Trim(EdExpression.Text) <> '') and
    (Trim(EdMaxTimeValue.Text) <> '') and (EdNumPts.Value > 1);
end;

procedure TFrmGraph.cmdCalcExecute(Sender: TObject);
var
  p: TMathParser;
  i, e: integer;
  t, d1, d2: TDouble;
  v: TVector;
begin
  // calc times range
  Val(EdMaxTimeValue.Text, d1, e);
  if e <> 0 then
  begin
    MessageDlg(Format(sVLEIllegalValue, [EdMaxTimeValue.Text]), mtWarning, [mbOK], 0);
    EdMaxTimeValue.SelectAll;
    EdMaxTimeValue.SetFocus;
    exit;
  end;

  Val(VLEGraph.Strings.Names[0], d2, e);
  if e <> 0 then
    d2 := 0;
  t := (d1 - d2) / (VLEGraph.Strings.Count - 1);
  p := TMathParser.Create;
  try
    p.Translate(EdExpression.Text, 't');
    SetLength(v, 2);
    for i := 0 to VLEGraph.Strings.Count - 1 do
    begin
      v[1] := d2;
      d1 := p.Get(v);
      VLEGraph.Strings[i] := Format(
        '%s=%s', [FormatFloat('0.#######', d2),
        FormatFloat('0.0000000E+00', d1)]);
      d2 := d2 + t;
    end;
    SetLength(v, 0);
  finally
    p.Free;
  end;
end;

procedure TFrmGraph.Button6Click(Sender: TObject);
begin

end;

procedure TFrmGraph.Button7Click(Sender: TObject);
begin

end;

function TFrmGraph.GraphNotEmpty: boolean;
begin
  with VLEGraph.Strings do
    Result := (Count > 0) and (Names[0] <> '');
end;

procedure TFrmGraph.cmdFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := GraphNotEmpty;
end;

procedure TFrmGraph.cmdFileSaveExecute(Sender: TObject);

  procedure WriteXml(const FileName: string);
  var
    xml: TXMLDocument;
    root, item: TDOMNode;
    i: integer;
  begin
    xml := TXMLDocument.Create;
    try
      root := xml.CreateElement('graph');
      xml.AppendChild(root);
      (root as TDOMElement)['count'] := IntToStr(VLEGraph.Strings.Count);
      for i := 0 to VLEGraph.Strings.Count - 1 do
      begin
        item := xml.CreateElement('item');
        (item as TDOMElement)['time'] := VLEGraph.Strings.Names[i];
        (item as TDOMElement)['value'] := VLEGraph.Strings.ValueFromIndex[i];
        root.AppendChild(item);
      end;
      WriteXMLFile(xml, FileName);
    finally
      xml.Free;
    end;
  end;

label
  again;
var
  FileName: string;
  List: TStringList;
  i: integer;
begin
  again:
    if not DlgSave.Execute then
      exit;
  case DlgSave.FilterIndex of
    1: FileName := ChangeFileExt(DlgSave.FileName, '.txt');
    2: FileName := ChangeFileExt(DlgSave.FileName, '.csv');
    3: FileName := ChangeFileExt(DlgSave.FileName, '.xml');
    else
      FileName := DlgSave.FileName;
  end;
  if FileExists(FileName) then
    case MessageDlg(Format(sOverwriteMsg, [FileName]), mtWarning, mbYesNoCancel, 0) of
      mrCancel: exit;
      mrNo: goto again;
    end;
  if SameText(ExtractFileExt(FileName), '.csv') then
  begin
    List := TStringList.Create;
    try
      for i := 0 to VLEGraph.Strings.Count - 1 do
        List.Add(StringReplace(VLEGraph.Strings[i], '=', ',', []));
      List.SaveToFile(FileName);
    finally
      List.Free;
    end;
  end
  else if SameText(ExtractFileExt(FileName), '.xml') then
  begin
    WriteXml(FileName);
  end
  else
  begin
    VLEGraph.Strings.SaveToFile(FileName);
  end;
end;

procedure TFrmGraph.cmdFileLoadExecute(Sender: TObject);

  procedure ReadXml(const FileName: string);
  var
    xml: TXMLDocument;
    root, item: TDOMNode;
    e: TDOMElement;
    i: integer;
  begin
    xml := TXMLDocument.Create;
    try
      ReadXMLFile(xml, FileName);
      root := xml.ChildNodes.Item[0];
      if (root = nil) or (root.ChildNodes.Count = 0) then
        raise Exception.Create(sInvalidXML);
      for i := 0 to root.ChildNodes.Count - 1 do
      begin
        item := root.ChildNodes.Item[i];
        if item = nil then
          raise Exception.Create(sInvalidXML);
        e := item as TDOMElement;
        if (Length(e['time']) = 0) or (Length(e['value']) = 0) then
          raise Exception.Create(sInvalidXML);
        VLEGraph.Strings.Add(Format('%s=%s', [e['time'], e['value']]));
      end;
    finally
      xml.Free;
    end;
  end;

var
  List: TStringList;
  i: integer;
begin
  if not DlgLoad.Execute then
    exit;
  VLEGraph.Strings.Clear;
  if SameText(ExtractFileExt(DlgLoad.FileName), '.csv') then
  begin
    List := TStringList.Create;
    try
      List.LoadFromFile(DlgLoad.FileName);
      for i := 0 to List.Count - 1 do
        VLEGraph.Strings.Add(StringReplace(List[i], ',', '=', []));
    finally
      List.Free;
    end;
  end
  else if SameText(ExtractFileExt(DlgLoad.FileName), '.xml') then
  begin
    VLEGraph.Strings.BeginUpdate;
    try
      VLEGraph.Strings.Clear;
      ReadXml(DlgLoad.FileName);
    finally
      VLEGraph.Strings.EndUpdate;
    end;
  end
  else
  begin
    VLEGraph.Strings.LoadFromFile(DlgLoad.FileName);
  end;
  EdNumPts.Value := VLEGraph.Strings.Count;
  EdMaxTimeValue.Text := VLEGraph.Strings.Names[VLEGraph.Strings.Count - 1];
  CheckList;
end;


procedure TFrmGraph.cmdRenderGraphExecute(Sender: TObject);
begin
  CheckList;
  RenderGraph(fOffScreen, VLEGraph.Strings);
  pbGraph.Invalidate;
  cmdSaveGraph.Enabled := True;
end;

procedure TFrmGraph.cmdSaveGraphExecute(Sender: TObject);
label
  again;
var
  FileName, ext: string;
  Png: TPortableNetworkGraphic;
begin
  again:
    if not DlgSaveGraph.Execute then
      exit;

  case DlgSaveGraph.FilterIndex of
    1: FileName := ChangeFileExt(DlgSaveGraph.FileName, '.png');
    2: FileName := ChangeFileExt(DlgSaveGraph.FileName, '.bmp');
    else
      FileName := DlgSaveGraph.FileName;
  end;
  if FileExists(FileName) then
    case MessageDlg(Format(sOverwriteMsg, [FileName]), mtWarning, mbYesNoCancel, 0) of
      mrCancel: exit;
      mrNo: goto again;
    end;
  Screen.Cursor := crHourGlass;
  try
    ext := ExtractFileExt(FileName);
    if SameText(ext, '.bmp') then
      fOffScreen.SaveToFile(FileName)
    else
    if SameText(ext, '.png') then
    begin
      Png := TPortableNetworkGraphic.Create;
      try
        Png.LoadFromBitmapHandles(fOffScreen.Handle, 0);
        Png.SaveToFile(FileName);
      finally
        Png.Free;
      end;
    end
    else
      MessageDlg(Format(sUnknownGraphFileExt, [ext]),
        mtInformation, [mbOK], 0);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmGraph.EdExpressionKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if key = VK_Return then
  begin
    cmdCalc.Execute;
    cmdRenderGraph.Execute;
  end;
end;

procedure TFrmGraph.EdMaxTimeValueKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if key = VK_Return then
  begin
    cmdCalc.Execute;
    cmdRenderGraph.Execute;
  end;
end;

procedure TFrmGraph.EdNumPtsChange(Sender: TObject);
begin
  VLEGraph.RowCount := EdNumPts.Value + 1;
end;



end.

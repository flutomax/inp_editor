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

unit ufrmFileImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, uDerivedClasses, uUnicalConv;

type

  { TFrmPopupNotifier }

  TFrmPopupNotifier = class(TForm)
    pnBottom: TButtonPanel;
    lbInfo: TListBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fImporter: TImporter;
    fJobList: TStrings;
    procedure ImportSuccess(Sender: TObject; const aFileName: string; aText: TStringStreamEx);
    procedure InportMessage(Sender: TObject; const aMsg: string);
    procedure InportStart(Sender: TObject; const aFileName: string);
    procedure InportFinish(Sender: TObject);
    procedure DoJob(Data: PtrInt);
  public
    property JobList: TStrings read fJobList write fJobList;
  end;

var
  FrmPopupNotifier: TFrmPopupNotifier;

implementation

uses uInpEditor, uFrmMain;

{$R *.lfm}

{ TFrmPopupNotifier }

procedure TFrmPopupNotifier.FormCreate(Sender: TObject);
begin
  fImporter:=TImporter.Create(self);
  fImporter.OnSuccessfully:=@ImportSuccess;
  fImporter.OnMessage:=@InportMessage;
  fImporter.OnStart:=@InportStart;
  fImporter.OnFinish:=@InportFinish;
  pnBottom.Color:=clForm; // correct button panel color
end;

procedure TFrmPopupNotifier.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoJob,0);
end;

procedure TFrmPopupNotifier.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose:=true;
  if not fImporter.Started then
    Exit;
  fImporter.Stop;
  while fImporter.Busy do
    Application.ProcessMessages;
end;

procedure TFrmPopupNotifier.ImportSuccess(Sender: TObject;
  const aFileName: string; aText: TStringStreamEx);
var
  Editor: TInpEditor;
  NewFileName: string;
begin
  NewFileName:=ChangeFileExt(aFileName,'.inp');
  Editor:=FrmMain.Pager.Open(NewFileName,true);
  Editor.Lines.LoadFromStream(aText);
  Editor.Sheet.Title:=ExtractFileName(NewFileName);
  Editor.Modified:=true;
  Editor.CaretToEOF;
end;

procedure TFrmPopupNotifier.InportMessage(Sender: TObject; const aMsg: string);
begin
  lbInfo.ItemIndex:=lbInfo.Items.Add(aMsg);
end;

procedure TFrmPopupNotifier.InportStart(Sender: TObject; const aFileName: string);
begin
  Caption:=Format('Importing %s',[ExtractFileName(aFileName)]);
  if lbInfo.Items.Count>0 then
    lbInfo.Items.Add('');
  lbInfo.ItemIndex:=lbInfo.Items.Add(Caption);
end;

procedure TFrmPopupNotifier.InportFinish(Sender: TObject);
begin
  lbInfo.ItemIndex:=lbInfo.Items.Add('All job is done');
  Sleep(500);
  Close;
end;

procedure TFrmPopupNotifier.DoJob(Data: PtrInt);
begin
  fImporter.AddJob(fJobList);
  fImporter.Start;
end;

end.


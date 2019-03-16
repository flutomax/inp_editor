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

unit uFrmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LMessages, uAnimateBackgound;

type



  { TFrmAbout }

  TFrmAbout = class(TForm)
    btClose: TButton;
    DonateLink: TImage;
    Image: TImage;
    Label1: TLabel;
    lblBuildV: TLabel;
    lblCopy: TLabel;
    lblBuildC: TLabel;
    lblFreePascalVerC: TLabel;
    lblFreePascalVerV: TLabel;
    lblHomePage: TLabel;
    lblHomePageAddress: TLabel;
    lblLazarusVerC: TLabel;
    lblLazarusVerV: TLabel;
    lblOperatingSystemC: TLabel;
    lblOperatingSystemV: TLabel;
    lblPlatformC: TLabel;
    lblPlatformV: TLabel;
    lblTitle: TLabel;
    lblVersionC: TLabel;
    lblVersionV: TLabel;
    procedure DonateLinkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AnimateBackgoundDraw(Sender: TObject);
    procedure lblHomePageAddressClick(Sender: TObject);
    procedure lblHomePageAddressMouseEnter(Sender: TObject);
    procedure lblHomePageAddressMouseLeave(Sender: TObject);
  private
    fAnimateBackgound: TAnimateBackgound;
  public

  end;

var
  FrmAbout: TFrmAbout;

  procedure ShowAbout;

implementation

uses
  LCLIntf, uSysInfo;

{$R *.lfm}


procedure ShowAbout;
begin
  with TFrmAbout.Create(Application) do
    try
      ShowModal;
    finally
      Release;
    end;
end;



{ TFrmAbout }

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
  DoubleBuffered:=true;
  // Windows: without DoubleBuffered - Bilnking if not theme service running
  fAnimateBackgound:=TAnimateBackgound.Create(self);
  fAnimateBackgound.OnDraw:=@AnimateBackgoundDraw;
  lblVersionV.Caption:=GetProgramVersion;
  lblBuildV.Caption:=GetBuildDate;
  lblLazarusVerV.Caption:=GetLazarusVersion;
  lblFreePascalVerV.Caption:=GetFPCVersion;
  lblPlatformV.Caption:=GetPlatform;
  lblOperatingSystemV.Caption:=GetOSVersion;
end;

procedure TFrmAbout.FormShow(Sender: TObject);
begin
  fAnimateBackgound.Start;
end;

procedure TFrmAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fAnimateBackgound.Stop;
end;

procedure TFrmAbout.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0,0,fAnimateBackgound.Buffer);
end;

procedure TFrmAbout.AnimateBackgoundDraw(Sender: TObject);
begin
  Invalidate;
end;

procedure TFrmAbout.lblHomePageAddressMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style:=[fsBold,fsUnderLine];
end;

procedure TFrmAbout.lblHomePageAddressMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style:=[fsBold];
end;

procedure TFrmAbout.lblHomePageAddressClick(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/?lang=en');
end;


procedure TFrmAbout.DonateLinkClick(Sender: TObject);
begin
  OpenURL('http://stone-voices.ru/donation?lang=en')
end;

end.


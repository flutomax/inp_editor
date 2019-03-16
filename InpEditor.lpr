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

program InpEditor;

{$mode objfpc}{$H+}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, lazcontrols, lazopenglcontext, uFrmMain, uInpEditor,
  uConsts, uFrmGotoLine, uFrmOptions, uConfig, uFrmTextSearch, uFrmTextReplace,
  SynExportRTF, uEditorMisc, uSynEditorOptions, uHighliter, uFrmExportText,
  uFrmAbout, uSysInfo, uAnimateBackgound, uDerivedClasses, uFrmEditorKeystrokes,
  ufileutils, uMRUList, uencodingFunc, uFrmNodesTransform, uFrmModelViewer,
  uCalculix, uFrmViewGroup, uInpTranslator, uFrmGroupSelector, uAddBCFunctions,
  uDialogs, uUnicalConv, ufrmFileImport;

{$R *.res}

{$IFDEF Windows}
  {$R InpDocument.res}
{$ENDIF}

begin
  Randomize;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.


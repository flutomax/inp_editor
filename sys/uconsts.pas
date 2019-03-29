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

unit uConsts;

{$mode objfpc}{$H+}

interface

uses SysUtils;

const

  AllowEditNumChars: TSysCharSet = ['0'..'9','+','-',#8,'.','e','E'];

resourcestring
  sAppTitle = 'Inp Editor';
  sDescription = 'Abaqus and CCX Inp Files Editor';
  sReload = 'Reload';
  sAskFileCreation = 'Required file'+LineEnding+'"%s"'+LineEnding+
    'does not exists. Do you want to create it?';
  sCannotCreate = 'Can''t create file'+LineEnding+'"%s".';
  sSaveChanges = 'Do you want to save the changes in the file'+LineEnding+'"%s"?';
  sCannotSave = 'Can not save changes to the file'+LineEnding+'"%s"!';
  sSaveWanted = 'For completion of this operation it is required to save the current file.'+LineEnding+
                'Do you want to save "%s"?';
  sOutTabIndex = 'Tab index (%d) out of range!';
  sFileNotFound = '"%s"'+LineEnding+LineEnding+
    'This file was not found at the specified path.';
  sReloadSimple = '"%s"'+LineEnding+LineEnding+
    'This file has been modified by another application.'+LineEnding+
    'Do you want to reload it?';
  sReloadModified = '"%s"'+LineEnding+LineEnding+
    'This file has been modified by another application.'+LineEnding+
    'Do you want to reload it and lose changes?';
  sKeepDeleted = '"%s"'+LineEnding+LineEnding+
    'This file has been deleted by another application.'+LineEnding+
    'Do you want to keep it in the editor?';
  sStatusBarInsMode = 'Insert';
  sStatusBarOvrMode = 'Overwrite';
  sStatusBarReadonly = 'ReadOnly';
  sStatusBarModified = 'Modified';
  sSearchEmpty = 'Can''t search for empty text!';
  sSearchNotFound = 'Not found';
  sSearchNotFoundMsg = 'Search Text "%s" not found!';
  sSearchNotFoundContBegin = 'Search Text "%s" not found more.'+LineEnding+
    'Continue search from the beginning?';
  sSearchNotFoundContEnd = 'Search Text "%s" not found more.'+LineEnding+
    'Continue search from the end?';
  sReplaceEmpty = 'Can''t replace an empty text!';
  sReplaceNotReplace = 'Search Text "%s" could not be replaced!';
  sInclude = '*INCLUDE, INPUT=%s';
  sAppError1 = 'Unhandled exception "%s" has occurred this application!'+LineEnding+LineEnding+'%s';
  sAppError2 = 'Unhandled exception has occurred this application!'+LineEnding+LineEnding+'%s';
  sAppError3 = 'If you click Ignore, the application will ignore this error and attempt to continue.'+LineEnding+
    'If you click Abort, the application will close immediately.';
  sClearMRU = 'The list of recent files will be empty.'+LineEnding+'Are you sure you want to clear the list?';

  sCalculixNotExists1 = 'The %s program path is not set!'+LineEnding+'Set the correct path in the program settings.';
  sCalculixNotExists2 = 'The path "%s" to the %s program is invalid!'+LineEnding+'Set the correct path in the program settings.';

  {$IfDef Windows}
  sFilterProgram = 'Programs (*.exe)|*.exe|All Files (*.*)|*.*';
  sDefExtProgram = '.exe';
  {$Else}
  sFilterProgram = 'Programs (*.*)|';
  sDefExtProgram = '';
  {$EndIf}
  sWarnAddFaces = 'New items will be added to the model description'+LineEnding+'Continue action?';
  sOverwriteConfirm = 'Overwrite Confirmation';
  sOverwriteMsg = 'File "%s" already exists.'+
    LineEnding+'Do you want to overwrite the existing file?'+
    LineEnding+'Click Yes to overwrite the existing file.'+
    LineEnding+'Click No to create a file with other name.'+
    LineEnding+'Click Cancel to discard the changes.';

  sNewFileName = 'New file name';
  sEnterNewFileName = 'Enter a new file name';
  sWarnBeforeRunCalculix = 'Required to save changes before running Calculix.';
  sWarnBeforeRunAddBC = 'Required to save changes before running Add BC.';
  sWarnEmptyValue = 'The requested value should not be empty.';

implementation

end.


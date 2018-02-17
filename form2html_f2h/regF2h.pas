unit regf2h; 

{*******************************************************}
{                                                       }
{       Form to HTML converter "F2H"                    }
{       Register component to Delphi palette            }
{                                                       }
{       Copyright (c) 1998-2018 HREF Tools Corp.        }
{       http://www.href.com/f2h                         }
{                                                       }
{       This file is licensed under a Creative Commons  }
{       Share-Alike 3.0 License.                        }
{       http://creativecommons.org/licenses/by-sa/3.0/  }
{       If you use this file, please keep this notice   }
{       intact.                                         }
{                                                       }
{       Developed by HREF Tools Corp. 1998-2011         }
{       First author: Philippe Maquet                   }
{                                                       }
{*******************************************************}

interface

{$I hrefdefines.inc}  

uses
   SysUtils, Controls;

procedure Register;

implementation

uses
  Classes,
  DesignIntf,
  f2h, f2hDsgnt;

procedure Register;
begin
  RegisterComponentEditor(TwhForm2HTML, TwhForm2HTMLEditor);

  RegisterPropertyEditor(TypeInfo(THFormElement), TWHForm2HTML,
   'Attributes', THAttributesEditor);
  RegisterPropertyEditor(TypeInfo(TFileName), TWHForm2HTML,
   'ExportFName', THExportFNameEditor);
  RegisterPropertyEditor(TypeInfo(TFileName), TWHForm2HTML,
   'ExportWHChunkFName', THExportWHChunkFNameEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWHForm2HTML,
   'BrowseIt', THBrowseItEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TWHForm2HTML,
   'BrowseWHChunk', THBrowseWHChunkEditor);
  RegisterPropertyEditor(TypeInfo(TWinControl), TWHForm2HTML,
   'MainContainer', THMainContainerEditor);
  RegisterPropertyEditor(TypeInfo(string), TWHForm2HTML,
   'BGColor', THBGColorEditor);

  RegisterComponents('HREF Tools', [TwhForm2HTML]);
end;

end.

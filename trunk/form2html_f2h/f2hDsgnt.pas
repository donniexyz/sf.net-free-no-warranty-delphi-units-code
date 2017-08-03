unit f2hDsgnt; 

{*******************************************************}
{                                                       }
{       Form to HTML converter "F2H"                    }
{       Interface to Delphi IDE                         }
{                                                       }
{       Copyright (c) 1998-2016 HREF Tools Corp.        }
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

uses
  Classes, Controls,
  {$IFDEF VER130}dsgnintf{$ELSE}DesignIntf, DesignEditors{$ENDIF},
  F2H;

type
   THAttributesEditor = class(TPropertyEditor)
      public
         procedure Edit; override;
         function GetAttributes: TPropertyAttributes; override;
         function GetValue: string; override;
   end;

   THExportFNameEditor = class(TPropertyEditor)
      public
         procedure Edit; override;
         function GetAttributes: TPropertyAttributes; override;
         function GetName : string; override;
         function GetValue: string; override;
         procedure SetValue(const Value: string); override;
   end;

   THExportWHChunkFNameEditor = class(TPropertyEditor)
      public
         procedure Edit; override;
         function GetAttributes: TPropertyAttributes; override;
         function GetName : string; override;
         function GetValue: string; override;
         procedure SetValue(const Value: string); override;
   end;

   THBrowseItEditor = class (TEnumProperty)
      public
         function GetName : string; override;
   end;

   THBrowseWHChunkEditor = class (TEnumProperty)
      public
         function GetName : string; override;
   end;

   THBGColorEditor = class(TStringProperty)
      public
         procedure Edit; override;
         function GetAttributes: TPropertyAttributes; override;
         procedure GetValues(Proc: TGetStrProc); override;
   end;

   THMainContainerEditor = class(TComponentProperty)
      private
         _proc: TGetStrProc;
         procedure GetValuesFilterProc(const S: string);
      public
         procedure GetValues(Proc: TGetStrProc); override;
   end;

   TWHForm2HTMLEditor = class(TComponentEditor)
      public
         procedure ExecuteVerb(Index: Integer); override;
         function GetVerb(Index: Integer): string; override;
         function GetVerbCount: Integer; override;
   end;

implementation

uses TypInfo;

type
  TWHForm2HTMLHack = class(TWHForm2HTML);
  
procedure THAttributesEditor.Edit;
begin
   if TWHForm2HTMLHack(GetComponent(0)).Edit then
      modified;
end;

function THAttributesEditor.GetAttributes : TPropertyAttributes;
begin
   result := [paReadOnly, paDialog];
end;

function THAttributesEditor.GetValue : string;
begin
   result := '...';
end;

function THExportWHChunkFNameEditor.GetValue : string;
begin
   result := TWHForm2HTML(GetComponent(0)).ExportWHChunkFName;
end;

procedure THExportWHChunkFNameEditor.SetValue(const Value: string);
begin
   setStrValue(value);
   with TWHForm2HTMLHack(GetComponent(0)) do begin
      _bWebHubMode := True;
      doExport;
      _bWebHubMode := False;
   end;
end;

procedure THExportWHChunkFNameEditor.Edit;
begin
   with TWHForm2HTMLHack(GetComponent(0)) do begin
      _bWebHubMode := True;
      editExport;
      _bWebHubMode := False;
   end;
end;

function THExportWHChunkFNameEditor.GetAttributes: TPropertyAttributes;
begin
   result := [paDialog];
end;

function THExportWHChunkFNameEditor.GetName : string;
begin
   result := 'Export WebHub droplet...';
end;

function THExportFNameEditor.GetValue : string;
begin
   result := TWHForm2HTML(GetComponent(0)).ExportFName;
end;

procedure THExportFNameEditor.SetValue(const Value: string);
begin
   setStrValue(value);
   with TWHForm2HTMLHack(GetComponent(0)) do begin
      _bWebHubMode := False;
      doExport;
   end;
end;

procedure THExportFNameEditor.Edit;
begin
   with TWHForm2HTMLHack(GetComponent(0)) do begin
      _bWebHubMode := False;
      editExport;
   end;
end;

function THExportFNameEditor.GetAttributes: TPropertyAttributes;
begin
   result := [paDialog];
end;

function THExportFNameEditor.GetName : string;
begin
   result := 'Export HTML...';
end;

function THBGColorEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paMultiSelect, paValueList, paRevertable];
end;

procedure THBGColorEditor.GetValues(Proc: TGetStrProc);
begin
   proc('[None]');
   proc('[Same]');
   proc('Aqua');
   proc('Black');
   proc('Blue');
   proc('DkGray');
   proc('Fuschia');
   proc('Gray');
   proc('Green');
   proc('Lime');
   proc('LtGray');
   proc('Lime');
   proc('Maroon');
   proc('Navy');
   proc('Olive');
   proc('Purple');
   proc('Red');
   proc('Silver');
   proc('Teal');
   proc('White');
   proc('Yellow');
end;

procedure THBGColorEditor.Edit;
var
   sColor : string;
begin
   sColor := getStrValue;
   if editBGColor(sColor) then
      setStrValue(sColor);
end;

function THBrowseItEditor.GetName : string;
begin
   result := 'Browse it !';
end;

function THBrowseWHChunkEditor.GetName : string;
begin
   result := 'Browse WebHub droplet !';
end;

procedure THMainContainerEditor.GetValuesFilterProc(const S: string);  // Containers only !
var
   testComp : TComponent;
begin
   testComp := Designer.GetComponent(S);
   if (TWHForm2HTML.getHContainerClass(TControl(testComp)) <> nil) then
      _proc(S);
end;

procedure THMainContainerEditor.GetValues(Proc: TGetStrProc);
begin
   _proc := Proc;
   Proc(TComponent(GetComponent(0)).owner.Name); // the form
   Designer.GetComponentNames(GetTypeData(GetPropType), GetValuesFilterProc);
end;

procedure TWHForm2HTMLEditor.ExecuteVerb(Index: Integer);
begin
   with TWHForm2HTMLHack(Component) do begin
      case index of
         0:
            if Edit then
               designer.modified;
         1:
            browseHTMLDialog;
         2:
            exportHTMLDialog;
         3:
            browseWHChunkDialog;
         4:
            exportWHChunkDialog;
      end;
   end;
end;

function TWHForm2HTMLEditor.GetVerb(Index: Integer): string;
begin
   case index of
      0:
         result := '&Edit HTML attributes...';
      1:
         result := '&Browse it !';
      2:
         result := 'E&xport HTML...';
      3:
         result := 'B&rowse WebHub droplet !';
      4:
         result := 'Export &WebHub droplet...';
      else
         result := '';
   end;
end;

function TWHForm2HTMLEditor.GetVerbCount: Integer;
begin
   result := 5;
end;

end.

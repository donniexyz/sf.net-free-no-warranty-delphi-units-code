unit f2h;

{*******************************************************}
{                                                       }
{       Form to HTML converter "F2H"                    }
{                                                       }
{       Copyright (c) 1998-2017 HREF Tools Corp.        }
{       http://www.href.com/f2h                         }
{                                                       }
{       This file is licensed under a Creative Commons  }
{       Share-Alike 3.0 License.                        }
{       http://creativecommons.org/licenses/by-sa/3.0/  }
{       If you use this file, please keep this notice   }
{       intact.                                         }
{                                                       }
{       Developed by HREF Tools Corp. 1998-2012         }
{       First author: Philippe Maquet                   }
{                                                       }
{*******************************************************}

interface

{$DEFINE IN_WEB_APPLICATION}

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   tpIntegerTypes;

type
   THBrowserFamily = (bfDefault, bfIE3, bfIE4, bfNS3, bfNS4);
   THBrowserRunFamily = (brfIE3, brfIE4, brfNS3, brfNS4);
   THAttrType = (atString, atBoolean, atInteger, atList);
   THAttrOption = (aoEditable, aoHTML, aoIgnore, aoWebHub, aoDialog, aoNoName);
   THAttrOptions = set of THAttrOption;
   THAttribute = class;
   THADialogProc = procedure (attr : THAttribute; var bEdited : Boolean) of object;
   THAGetDefaultProc = procedure (var defaultString : string) of object;
   THAValidateProc = procedure (var aValue : string; var bOK : Boolean) of object;
   THAValueChangedProc = procedure (changedAttr : THAttribute) of object;
   THAttribute = class(TObject)    // an HTML attribute
      private
         _attrType : THAttrType;
         _bChanged : Boolean;
         _bIsDefault : Boolean;
         _defValue : string;
         _dialogProc : THADialogProc;
         _getDefaultProc : THAGetDefaultProc;
         _hint : string;
         _name : string;
         _options : THAttrOptions;
         _savedValue : string;
         _stdAttrName : string;
         _validateProc : THAValidateProc;
         _value : string;
         _valueChangedProc : THAValueChangedProc;
         _valuesList : TStringList;
         function ValueEquals(compObj : THAttribute) : Boolean;
         function getBChanged : Boolean;
         function getBooleanValue : Boolean;
         function getDefValue : string;
         function getHTML : string;
         function getNextValue : string;
         function getValue : string;
         procedure setBooleanValue(b : Boolean);
         procedure setDefValue(const aValue : string);
         procedure setOptions(options : THAttrOptions);
         procedure setValue(const aValue : string);
         procedure writeData(writer: TWriter);
      public
         constructor create(const aName : string; aType : THAttrType;
                       const aHint : string);
         destructor Destroy; override;
         class function boolean2String(b : Boolean) : string;
         function isValid(var aValue : string) : Boolean;
         procedure restore;
         procedure save;
         class function string2Boolean(const s : string) : Boolean;
         //properties
         property AttrType : THAttrType read _attrType;
         property BooleanValue : Boolean read getBooleanValue write setBooleanValue;
         property Changed : Boolean read getBChanged; //  information for the editor
         property DefValue : string read getDefValue write setDefValue;
         property DialogProc : THADialogProc read _dialogProc write _dialogProc;
         property GetDefaultProc : THAGetDefaultProc read _GetDefaultProc write _GetDefaultProc;
         property Hint : string read _hint;
         property HTML : string read getHTML;
         property Name : string read _name;
         property NextValue : string read getNextValue;
         property Options : THAttrOptions read _options write setOptions;
         property StdAttrName : string read _stdAttrName write _stdAttrName;
         property ValidateProc : THAValidateProc read _validateProc write _validateProc;
         property Value : string read getValue write setValue;
         property ValueChangedProc : THAValueChangedProc read _valueChangedProc write _valueChangedProc;
         property ValuesList : TStringList read _valuesList;
   end;

   THAttributes = class(TObject)    // an HTML object's attributes list
      private
         _attributes : TList;
         _optionsFilter : THAttrOptions;
         function ValueEquals(compObj : THAttributes) : Boolean;
         function getAttribute(const attrName : string) : THAttribute;
         function getAttributeN(index : integer) : THAttribute;
         function getCount : integer;
         function getHTML : string;
         procedure writeData(writer: TWriter);
      public
         destructor Destroy; override;
         procedure add(anAttr : THAttribute);
         procedure remove(const attrName : string);
         procedure save;
         procedure restore;
         // Properties
         property Attribute[const attrName : string] : THAttribute read getAttribute; default;
         property AttributeN[index : integer] : THAttribute read getAttributeN;
         property Count : integer read getCount;
         property HTML : string read getHTML;
         property OptionsFilter : THAttrOptions read _optionsFilter write _optionsFilter;
   end;

   TWHForm2HTML = class;

   THAttrObject = class(TObject)
      private
         _attributes : THAttributes;
         _form2HTML : TWHForm2HTML;
         procedure copyAttributesFrom(fromAO : THAttrObject);
         function ValueEquals(compObj : THAttrObject) : Boolean; virtual;
         function getName : string; virtual; abstract;
         procedure setOptionsFilter(optionsFilter : THAttrOptions); virtual;
         procedure setWebHubMode(b : Boolean); virtual;
         procedure writeData(writer: TWriter); virtual;
      public
         constructor createHAttrObject; virtual;
         destructor Destroy; override;
         property Attributes : THAttributes read _attributes;
         property Form2HTML : TWHForm2HTML read _form2HTML write _form2HTML;
         property Name : string read getName;
         property OptionsFilter : THAttrOptions write setOptionsFilter;
         property WebHubMode : Boolean write setWebHubMode;
   end;

   THContainer = class;

   THElement = class(THAttrObject)     // Any element. Owns the BeforeElement/After attributes
      private
         procedure addBeforeAndAfterElements(var toS : string);
         function getAfterElement : string;
         function getBeforeElement : string;
      public
         constructor createHAttrObject; override;
   end;

   THControl = class(THElement)     // An abstract HTML control, container or not
      private
         _bRightAligned : Boolean;
         _bRendered : Boolean;
         _bSynch : Boolean;
         _control : TControl;
         _col, _colspan, _row, _rowspan : integer;
         _hContainer : THContainer;
         procedure addAnchorsAttributes;
         procedure asAnchorChanged(attr : THAttribute);
         procedure asWHAnchorChanged(attr : THAttribute);
         procedure getDefaultWHAnchorMacro(var defWHAnchorMacro : string);
         function getEnabled : Boolean;
         function getHTML : string; virtual;
         function getHTMLBeginTag : string; virtual;
         function getHTMLEndTag : string; virtual;
         function getHTMLText: string; virtual;
         function getName : string; override;
         function getNamePath : string;
         function getVisible : Boolean;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; virtual;
         procedure setSynchronized(bOnOff : Boolean); virtual;
      public
         constructor create(aControl : TControl); virtual;
         function getHDims(var hWidth, hHeight : integer) : Boolean; virtual;
         property Control : TControl read _control;
         property Enabled : Boolean read getEnabled;
         property HTML : string read getHTML;
         property RightAligned : Boolean read _bRightAligned write _bRightAligned;
         property Synchronized : Boolean read _bSynch write setSynchronized;
         property Visible : Boolean read getVisible;
   end;

   THControlClass = class of THControl;

   THLabel = class(THControl)     // A label
      private
         function getHTML: string; override;
         function getHTMLText: string; override;
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THWinControl = class(THControl)
      private
         function getHTML : string; override;
         function getHTMLText : string; override;
      public
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THNamed = class(THWinControl)   // Any control with a "name" attribute
      private
         procedure getDefaultName (var defaultName : string); virtual;
      public
         constructor create(aControl : TControl); override;
   end;

   THMemo = class(THNamed)   // Any control of type <TEXTAREA>
      private
         procedure getDefaultCols(var defCols : string);
         procedure getDefaultText(var defText : string);
         procedure getDefaultRows(var defRows : string);
         function getHTMLBeginTag : string; override;
         function getHTMLText : string; override;
         function getHTMLEndTag : string; override;
         function getHTML : string; override;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; override;
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THInput = class(THNamed)   // Any control of type <INPUT>
      private
         function getHTMLBeginTag : string; override;
   end;

   THButton = class(THInput)
      private
         procedure asImageChanged(attr : THAttribute);
         procedure getDefaultAlt(var defAlt : string);
         procedure getDefaultValue(var defValue : string);
         function getHTML : string; override;
         function getHWidth(const sCaption : string) : integer;
         function getPureCaption : string;
         procedure validateType(var sType : string; var bOK : Boolean);
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THImage = class(THNamed)
      private
         procedure asButtonChanged(attr : THAttribute);
         procedure getDefaultAlt(var defAlt : string);
         procedure getDefaultHeight(var defHeight : string);
         procedure getDefaultWidth(var defWidth : string);
         function getHTMLBeginTag : string; override;
         function getHTMLEndTag : string; override;
         function getHTML : string; override;
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THEdit = class(THInput)    // <INPUT type="text">
      private
         procedure getDefaultMaxLength(var defMaxLength : string);
         procedure getDefaultSize(var defSize : string);
         procedure getDefaultType(var defType : string);
         procedure getDefaultValue(var defValue : string);
         function getHTML : string; override;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; override;
         procedure validateMaxLength(var sMaxLength : string; var bOK : Boolean);
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THCheck = class(THInput)  // Any <INPUT> control with a "Checked" attribute
      private
         procedure getDefaultChecked(var defChecked : string);
         procedure getDefaultName (var defaultName : string); override;
         function getHTMLText: string; override;
         function isRadio : Boolean;
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THCheckBox = class(THCheck)     // <INPUT type="checkbox">
      private
         function getHTML : string; override;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; override;
      public
         constructor create(aControl : TControl); override;
   end;

   THRadio = class(THCheck)     // <INPUT type="radio">
      private
         procedure getDefaultValue(var defValue : string);
         function getHTML : string; override;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; override;
      public
         constructor create(aControl : TControl); override;
   end;

   THList = class(THNamed)
      private
         function getHTML : string; override;
         function getHTMLBeginTag : string; override;
         function getHTMLEndTag : string; override;
         function getHTMLText : string; override;
         function getItemsMaxLength(var index : integer) : integer;
         function getItemText(index : integer) : string;
         function getItemValue(index : integer) : string;
         function getStdHWidth : integer;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; override;
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THComboBox = class(THList)
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THListBox = class(THList)
      private
         procedure getDefaultMultiple(var defMultiple : string);
         procedure getDefaultSize(var defSize : string);
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THPlacer = class(TObject)
      private
         _hContainer : THContainer;
         _form2HTML : TWHForm2HTML;
         _rows : TList;
         _xPrec, _yPrec : integer;
         procedure addRows(nRows : integer);
         procedure addCols(nCols : integer);
         function colCount : integer;
         function getCell(col, row : integer) : THControl;
         function getHTML : string;
         function rowCount : integer;
         procedure setCell(col, row : integer; hControl : THControl);
         property Cell[col, row : integer] : THControl read getCell write setCell;
      public
         constructor create(hControl : THControl; width, height, xPrec, yPrec : integer);
         destructor Destroy; override;
         procedure getHDims(var hWidth, hHeight : integer);
         procedure placeControl(hControl : THControl);
         property HTML : string read getHTML;
   end;

   THContainer = class(THWinControl)
      private
         _children : TList;
         _hPlacer : THPlacer;
         procedure createPlacer;
         function ValueEquals(compObj : THAttrObject) : Boolean; override;
         procedure freePlacer;
         function getChild(index : integer) : THControl;
         function getCount : integer;
         function getHTMLText: string; override;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; override;
         procedure setChild(index : integer; anHControl : THControl);
         procedure setOptionsFilter(optionsFilter : THAttrOptions); override;
         procedure setSynchronized(bOnOff : Boolean); override;
         procedure setWebHubMode(b : Boolean); override;
         procedure writeData(writer: TWriter); override;
      public
         constructor create(aControl : TControl); override;
         destructor Destroy; override;
         function createNewChild(fromCtl : TControl) : THControl;
         function FindHObjectByName(const objName : string) : THAttrObject;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
         procedure synchronize;
         procedure synchronizeDone;

         property Children[index : integer] : THControl read getChild write setChild;
         property Count : integer read getCount;
   end;

   THFormElement = class(THElement) // <FORM>..</FORM> element
      private
         function getName : string; override;
         procedure getDefaultWHAction(var defWHAction : string);
         procedure getDefaultWHTargetPageID(var defWHTargetPageID : string);
         procedure updateWHAction(attr : THAttribute);
      public
         constructor create;
   end;

   THForm = class(THContainer)        // A form container
      private
         procedure CaptionColorDlg(attr : THAttribute; var bEdited : Boolean);
         function getHTMLText: string; override;
         procedure ShowCaptionChanged(attr : THAttribute);
      public
         constructor create(aControl : TControl); override;
   end;

   THPanel = class(THContainer)        // A panel container
      private
         procedure getDefaultBorder(var defBorder : string);
         function getHTMLText: string; override;
         procedure validateBorder(var sBorder : string; var bOK : Boolean);
      public
         constructor create(aControl : TControl); override;
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THGroupBox = class(THContainer)
      private
         function getHTMLText: string; override;
      public
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THRadioGroup = class(THNamed)
      private
         function getHTMLText: string; override;
         function getHTML : string; override;
         function itemsHTML : string;
         function getWHResetToDefaultMacro(clearList : TStringList) : string; override;
      public
         function getHDims(var hWidth, hHeight : integer) : Boolean; override;
   end;

   THExtStored = class(TObject)
      private
         _bIsAttr : Boolean;
         _name : string;
         _value : string;
      public
         constructor create(bIsAttr : Boolean);
         procedure readData(Reader : TReader);
         property IsAttr : Boolean read _bIsAttr;
         property Name : string read _name;
         property Value : string read _value;
   end;

   THExtStoredList = class(TObject)
      private
         _list : TList;
      public
         constructor create;
         destructor Destroy; override;
         procedure readData(Reader : TReader);
         property List : TList read _list;
      end;

   TWHChunkOutputScope = (cosAll, cosForm, cosReset);

   TBARenderingEvent = procedure (ctl : TControl; hctl : THControl) of object;
   TRenderingEvent = procedure (ctl : TControl; hctl : THControl; var output : string) of object;

   TWHForm2HTML = class(TComponent)
      private
         _alternateFonts : string;
         _bChunkOnly : Boolean;
         _bmp : TBitmap;
         _bSetFonts : Boolean;
         _BGColor : string;
         _browserFamily : THBrowserFamily;
         _bQualifiedNames : Boolean;
         _bWHChunkDeclaration : Boolean;
         _exportFName : TFileName;
         _exportWHChunkFName : TFileName;
         _extStoredList : THExtStoredList;
         _mainContainer : TWinControl;
         _hForm : THFormElement;
         _hMainContainer : THContainer;
         _WHChunkOutputScope : TWHChunkOutputScope;

         // Events
         _onAfterRendering : TBARenderingEvent;
         _onBeforeRendering : TBARenderingEvent;
         _onRendering : TRenderingEvent;
         {_onAfterRenderingControl : TNotifyEvent;
         _onBeforeRenderingControl : TNotifyEvent;}
         procedure doBrowse(bWH : Boolean);
         function ValueEquals(compObj : TWHForm2HTML) : Boolean;
         function fontSize2HFontSize(fSize : integer) : integer;
         function getBrowseIt : Boolean;
         function getFontAttr(hctl : THControl; const sColor : string) : string;
         function getFontOfHCtl(hctl : THControl) : TFont;
         function getFormBeginTag : string;
         function getFormEndTag : string;
         function getHDimsOfText(hctl : THControl; const s : string; var hWidth, hHeight : integer) : Boolean;
         function getHTML : string;
         function getHTMLString : string;
         function getVersion : string;
         function getWHChunk : string;
         function hFontSize2FontSize(hfSize : integer) : integer;
         procedure ReadData(Reader: TReader);
         procedure setBrowseIt(b : Boolean);
         procedure setBrowseWHChunk(b : Boolean);
         procedure setCGIUserAgent(const cua : string);
         procedure setMainContainer(mainContainer : TWinControl);
         procedure setVersion(const s : string);
         procedure writeData(Writer: TWriter);
      protected
         _bWebHubMode : Boolean;
         procedure DefineProperties(Filer: TFiler); override;
         procedure Loaded; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         //For TWHForm2HTMLHack
         function edit : Boolean;
         function doExport : Boolean;
         function editExport : Boolean;
      public
         constructor Create(owner : TComponent); override;
         destructor Destroy; override;
         function BGColorAttr : string;
         procedure browseHTMLDialog;
         procedure browseWHChunkDialog;
         function exportHTML : Boolean;
         function exportHTMLDialog : Boolean;
         function exportWHChunk : Boolean;
         function exportWHChunkDialog : Boolean;
         function getHControlClass(fromCtl : TControl) : THControlClass;
         class function getHContainerClass(fromCtl : TControl) : THControlClass;
         procedure Synchronize;

         property CGIUserAgent : string write setCGIUserAgent;
         property HForm : THFormElement read _hForm;
         property HTML : string read getHTML;
         property HMainContainer : THContainer read _hMainContainer;
         property WebHubMode : Boolean read _bWebHubMode;
         property WHChunk : string read getWHChunk;
      published
         property AlternateFonts : string read _alternateFonts write _alternateFonts;
         property Attributes : THFormElement read _hForm;  // or any other type !
         property BGColor : string read _BGColor write _BGColor;
         property BrowseIt : Boolean read getBrowseIt write setBrowseIt stored false;
         property BrowseWHChunk : Boolean read getBrowseIt write setBrowseWHChunk stored false;
         property BrowserFamily : THBrowserFamily read _BrowserFamily write _BrowserFamily;
         property ChunkOnly : Boolean read _bChunkOnly write _bChunkOnly;
         property ExportFName : TFileName read _exportFName write _exportFName;
         property ExportWHChunkFName : TFileName read _exportWHChunkFName write _exportWHChunkFName;
         property MainContainer : TWinControl read _mainContainer write setMainContainer;
         property SetFonts : Boolean read _bSetFonts write _bSetFonts;
         property QualifiedNames : Boolean read _bQualifiedNames write _bQualifiedNames default False;
         property Version : string read getVersion write setVersion;
         property WHChunkDeclaration : Boolean read _bWHChunkDeclaration write _bWHChunkDeclaration default true;
         property WHChunkOutputScope : TWHChunkOutputScope read _WHChunkOutputScope write _WHChunkOutputScope;
         // Events
         property OnAfterRendering : TBARenderingEvent read _OnAfterRendering write _OnAfterRendering;
         property OnBeforeRendering : TBARenderingEvent read _OnBeforeRendering write _OnBeforeRendering;
         property OnRendering : TRenderingEvent read _OnRendering write _OnRendering;
         {property OnAfterRenderingControl : TNotifyEvent read _OnAfterRenderingControl write _OnAfterRenderingControl;
         property OnBeforeRenderingControl : TNotifyEvent read _OnBeforeRenderingControl write _OnBeforeRenderingControl;}

   end;

   function editBGColor(var sColor : string) : Boolean;

// See regf2h.pas for registration of component onto Delphi palette.

implementation

uses
   {$IFDEF CodeSite}CodeSiteLogging,{$ENDIF}
   StdCtrls, shellapi, TypInfo, ExtCtrls,
   {$IFDEF IN_WEB_APPLICATION}ZM_CodeSiteInterface,{$ENDIF}
   f2hf;

var
  MacroStart: string = '(~'; {signals beginning of dynamic WebHub content}
  MacroEnd: string = '~)'; {signals end of dynamic content}

const
   VERSION_NUMBER = '1.10';

   DEF_BRF : THBrowserRunFamily = brfIE4;

   EDIT_HEIGHT : array[THBrowserRunFamily] of integer = (23, 24, 24, 24);
   EDIT_MIN_WIDTH : array[THBrowserRunFamily] of integer = (25, 20, 16, 16);
   EDIT_MIN_WIDTH_CHARS : array[THBrowserRunFamily] of integer = (1, 1, 1, 1);
   EDIT_WIDTH_PER_CHAR : array[THBrowserRunFamily] of integer = (5, 7, 8, 8);

   BUT_HEIGHT : array[THBrowserRunFamily] of integer = (23, 24, 24, 24);
   BUT_MIN_TW : array[THBrowserRunFamily] of integer = (61, 0, 0, 0);
   BUT_ADD_WPCT : array[THBrowserRunFamily] of double = (0.0, 0.5, 0.5, 0.5);
   BUT_ADD_W : array[THBrowserRunFamily] of integer = (11, 4, 4, 4);
   BUT_FNT_MOD_SIZE : array[THBrowserRunFamily] of integer = (9, 12, 11, 11);

   CHECK_MIN_HEIGHT : array[THBrowserRunFamily] of integer = (19, 19, 19, 19);
   CHECK_MIN_WIDTH : array[THBrowserRunFamily] of integer = (19, 19, 19, 19);

   COMBO_HEIGHT : array[THBrowserRunFamily] of integer = (21, 22, 22, 22);
   LIST_MIN_WIDTH : array[THBrowserRunFamily] of integer = (75, 34, 41, 41);
   LIST_MIN_WIDTH_CHARS : array[THBrowserRunFamily] of integer = (10, 1, 1, 1);
   LIST_WIDTH_PER_CHARS : array[THBrowserRunFamily] of double = (5, 6.7, 6, 6);
   LIST_BASE_HEIGHT : array[THBrowserRunFamily] of integer = (6, 6, 6, 6);
   LIST_ITEM_HEIGHT : array[THBrowserRunFamily] of integer = (13, 16, 16, 16);

   MEMO_BASE_HEIGHT : array[THBrowserRunFamily] of integer = (26, 8, 32, 32);
   MEMO_ITEM_HEIGHT : array[THBrowserRunFamily] of integer = (13, 16, 16, 16);
   MEMO_BASE_WIDTH : array[THBrowserRunFamily] of integer = (0, 22, 16, 16);
   MEMO_COL_WIDTH : array[THBrowserRunFamily] of integer = (5, 8, 8, 8);


{$R *.RES}

// Utilities

function HColor2Color(const sColor : string; var color : TColor) : Boolean;
var
   s : string;
begin
   s := UpperCase(sColor);
   if s = 'LTGRAY' then
      s := 'SILVER'
   else if s = 'DKGRAY' then
      s := 'GRAY';
   s := 'cl' + s;
   if IdentToColor(s, Int32(Color)) then
      result := True
   else if (copy(sColor, 1, 1) <> '#') or (length(sColor) <> 7) then
      result := false
   else begin
      s := '$00' + copy(sColor, 6, 2) + copy(sColor, 4, 2) + copy(sColor, 2, 2);
      try
         color := integer(strToInt(s));
         result := true;
      except
         result := false;
      end;
   end;
end;

function Color2HColor(color : TColor) : string;
var
   s : string;
begin
   s := intToHex(ColorToRGB(color), 8);
   result := '#'+ copy(s, 7, 2) +
                  copy(s, 5, 2) +
                  copy(s, 3, 2);
end;

function editBGColor(var sColor : string) : Boolean;
var
   colorDlg : TColorDialog;
   col : TColor;
begin
   colorDlg := nil;
   result := false;
   try
      colorDlg := TColorDialog.create(application);
      with colorDlg do begin
         options := [cdSolidColor];
         if HColor2Color(sColor, col) then
            Color := col;
         if execute then begin
            sColor := Color2HColor(color);
            result := true;
         end;
      end;
   finally
      colorDlg.free;
   end;
end;

function left(const s : string; n : integer) : string;
begin
   result := copy(s, 1, n);
end;

function right(const s : string; n : integer) : string;
begin
   result := copy(s, length(s) - 1, n);
end;

function bf2brf(bf : THBrowserFamily) : THBrowserRunFamily;
begin
   if bf = bfDefault then
      result := DEF_BRF
   else
      result := THBrowserRunFamily(integer(bf) - 1);
end;

function removeChars(const s, cars : string) : string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
    if pos(s[i],cars) = 0 then Result := Result + s[i];
end;

function LTrim(const aString : string) : string;
var
   i, len : integer;
   testCar : char;
begin
   result := '';
   len := length(aString);
   for i := 1 to len do begin
      testCar := aString[i];
      if testCar <> ' ' then begin
         result := copy(aString, i , len - i + 1);
         exit;
      end;
   end;
end;

function RTrim(const aString : string) : string;
var
   i, nSpaces, len : integer;
   testCar : char;
begin
   result := '';
   nSpaces := 0;
   len := length(aString);
   for i := len downto 1 do begin
      testCar := aString[i];
      if testCar <> ' ' then begin
         result := copy(aString, 1 , len - nSpaces);
         exit;
      end;
      inc(nSpaces);
   end;
end;

function AllTrim(const aString : string) : string;
begin
   result := RTrim(LTrim(aString));
end;
// End of utilities

// Hacker classes
type
   THackLabel = class(TCustomLabel) end;
   THackEdit = class(TCustomEdit) end;
   THackCheckBox = class(TCustomCheckBox) end;
   THackListBox = class(TCustomListBox) end;
   THackRadioGroup = class(TCustomRadioGroup) end;
   THackGroupBox = class(TCustomGroupBox) end;
   THackPanel = class(TCustomPanel) end;


constructor THMemo.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Cols', atInteger,
         'HTML <TEXTAREA> Cols. Sets the element''s width in characters.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultCols;
         Options := Options - [aoEditable];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Rows', atInteger,
         'HTML <TEXTAREA> Rows. Sets the number of rows that will be displayed.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultRows;
         Options := Options - [aoEditable];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Text', atString,
         'HTML <TEXTAREA> initially displayed text.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultText;
         Options := Options - [aoHTML];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Wrap', atList,
         'HTML <TEXTAREA> Wrap attribute. The possible values are "Off", "Virtual" and "Physical".');
      with tmpAttr.ValuesList do begin
         add('Off');
         add('Virtual');
         add('Physical');
      end;
      with tmpAttr do begin
         DefValue := 'Physical';
         Value := DefValue;
      end;
      add(tmpAttr);
   end;
end;

// Ann also changed this function to make the textareas work.
function THMemo.getHTML : string;
var
   s : string;
   aName, aText : string;
   aWrap : string;
begin
   if (not Form2HTML.WebHubMode) or (not enabled) then
      result := inherited getHTML
   else begin
      //
      aName:=Attributes['Name'].Value;
      aText:=Attributes['Text'].Value;
      aWrap:=Attributes['Wrap'].Value;
      //
      if aText<>aName then begin
        result:='<textarea rows='+Attributes['Rows'].Value+
          ' cols='+Attributes['Cols'].Value+
          ' name='+aName;
        // not sure where the extra attributes go. -ann
        if aWrap<>'' then
          result:=result+' wrap='+aWrap;
        result:=result+'>'+aText+'</textarea>';
        end
      else begin
        result := string(MacroStart) + 'INPUTTEXTAREA|' +
          Attributes['Name'].Value +
                  ',' + Attributes['Rows'].Value +
                  ',' + Attributes['Cols'].Value;
        result := result + '|Wrap=' + Attributes['Wrap'].Value;
        s := Attributes['ExtraAttributes'].Value;
        if s <> '' then
           result := result + ' ' + s;
        result := result + string(MacroEnd);
        end;
      addBeforeAndAfterElements(result);
      with _form2HTML do begin
         if assigned(_OnAfterRendering) then
            _OnAfterRendering(_control, self);
      end;
   end;
end;

function THMemo.getHTMLText : string;
begin
   result := Attributes['Text'].Value;
end;

procedure THMemo.getDefaultText(var defText : string);
begin
   defText := TCustomMemo(_control).Lines.Text;
end;

procedure THMemo.getDefaultCols(var defCols : string);
var
   nWidth, nCols : integer;
   brf : THBrowserRunFamily;
begin
   nWidth := _control.Width;
   brf := bf2brf(Form2HTML.BrowserFamily);
   if nWidth < MEMO_BASE_WIDTH[brf] then
      nWidth := MEMO_BASE_WIDTH[brf];  // minimum width
   nCols := (nWidth - MEMO_BASE_WIDTH[brf]) div MEMO_COL_WIDTH[brf];
   if ((nWidth - MEMO_BASE_WIDTH[brf]) mod MEMO_COL_WIDTH[brf]) <> 0 then
      inc(nCols);
   defCols := intToStr(nCols);
end;

procedure THMemo.getDefaultRows(var defRows : string);
var
   nHeight, nRows : integer;
   brf : THBrowserRunFamily;
begin
   nHeight := _control.height;
   brf := bf2brf(Form2HTML.BrowserFamily);
   if nHeight < MEMO_BASE_HEIGHT[brf] then
      nHeight := MEMO_BASE_HEIGHT[brf];  // minimum height
   nRows := (nHeight - MEMO_BASE_HEIGHT[brf]) div MEMO_ITEM_HEIGHT[brf];
   if ((nHeight - MEMO_BASE_HEIGHT[brf]) mod MEMO_ITEM_HEIGHT[brf]) <> 0 then
      inc(nRows);
   defRows := intToStr(nRows);
end;

function THMemo.getHTMLBeginTag : string;
begin
   result := '<TEXTAREA';
end;

function THMemo.getHTMLEndTag : string;
begin
   result := '</TEXTAREA>';
end;

function THMemo.getHDims(var hWidth, hHeight : integer) : Boolean;
var
   nCols, nRows : integer;
   brf : THBrowserRunFamily;
begin
   result := true;
   if inherited getHDims(hWidth, hHeight) then
      exit;
   brf := bf2brf(Form2HTML.BrowserFamily);
   nRows := StrToInt(_attributes['Rows'].Value);
   hHeight := MEMO_BASE_HEIGHT[brf] + nRows * MEMO_ITEM_HEIGHT[brf];
   nCols := StrToInt(_attributes['Cols'].Value);
   hWidth := MEMO_BASE_WIDTH[brf] + nCols * MEMO_COL_WIDTH[brf];
end;

constructor THEdit.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Maxlength', atInteger,
         'HTML <INPUT> Maxlength. Sets the number of characters which may be typed. Unlimited if empty.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultMaxLength;
         ValidateProc := validateMaxLength;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Size', atInteger,
         'HTML <INPUT> Size. Sets the element''s width in characters.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultSize;
         Options := Options - [aoEditable];
      end;
      add(tmpAttr);

      tmpAttr := THAttribute.create('Type', atString,
         'HTML <INPUT> Type, "text" or "password" depending on property ''PasswordChar''.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultType;
         options := options - [aoEditable];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Value', atString,
         'HTML <INPUT> Value. Sets the initial displayed value.');
      with tmpAttr do
         GetDefaultProc := getDefaultValue;
      add(tmpAttr);
   end;
end;

function THEdit.getHTML : string;
var
   s : string;
   aName, aValue : string;
   aMaxLength : string;
begin
   if (not Form2HTML.WebHubMode) or (not enabled) then
      result := inherited getHTML
   else begin
      {if assigned(_form2HTML.OnBeforeRenderingControl) then
         _form2HTML.OnBeforeRenderingControl(self);}
      aName:=Attributes['Name'].Value;
      aValue:=Attributes['Value'].Value;
      aMaxLength:=Attributes['MaxLength'].value;
      if aValue<>aName then begin
        result:='<input type=text '+
          ' name='+aName+
          ' size='+Attributes['Size'].Value;
        if aMaxLength<>'' then
          result:=result+' maxlength='+aMaxLength;
        result:=result+' value="'+aValue+'">';
        end
      else begin
        if Attributes['Type'].Value = 'text' then
           result := MacroStart + 'INPUTTEXT|'
        else
           result := MacroStart + 'INPUTPASSWORD|';
        result := result + aName +
                  ',' + Attributes['Size'].Value;
        //s := Attributes['Maxlength'].Value;
        if aMaxLength <> '' then
           result := result + ',' + aMaxLength;
        s := Attributes['ExtraAttributes'].Value;
        if s <> '' then
           result := result + '|' + s;
        result := result + MacroEnd;
        end;
      addBeforeAndAfterElements(result);
      {if assigned(_form2HTML.OnAfterRenderingControl) then
         _form2HTML.OnAfterRenderingControl(self);}
   end;
end;

procedure THEdit.getDefaultType(var defType : string);
begin
   if THackEdit(_control).PasswordChar = #0 then
      defType := 'text'
   else
      defType := 'password';
end;

function THEdit.getHDims(var hWidth, hHeight : integer) : Boolean;
var
   nSize : integer;
   brf : THBrowserRunFamily;
begin
   result := true;
   if inherited getHDims(hWidth, hHeight) then
      exit;
   brf := bf2brf(Form2HTML.BrowserFamily);
   hHeight := EDIT_HEIGHT[brf];
   nSize := StrToInt(_attributes['Size'].Value);
   if nSize < EDIT_MIN_WIDTH_CHARS[brf] then
      nSize := EDIT_MIN_WIDTH_CHARS[brf];
   hWidth := EDIT_MIN_WIDTH[brf] + EDIT_WIDTH_PER_CHAR[brf] * (nSize - EDIT_MIN_WIDTH_CHARS[brf]);
end;

procedure THEdit.getDefaultSize(var defSize : string);
var
   nSize : integer;
   brf : THBrowserRunFamily;
   w : integer;
begin
   brf := bf2brf(Form2HTML.BrowserFamily);
   nSize := EDIT_MIN_WIDTH_CHARS[brf];
   w := TCustomEdit(_control).width - EDIT_MIN_WIDTH[brf];
   if w > 0 then
      inc(nSize, w div EDIT_WIDTH_PER_CHAR[brf]);
   if (w mod EDIT_WIDTH_PER_CHAR[brf]) <> 0 then
      inc(nSize);
   defSize := intToStr(nSize);
end;

procedure THEdit.getDefaultMaxLength(var defMaxLength : string);
begin
   with THackEdit(_control) do begin
      if MaxLength <> 0 then
         defMaxLength := intToStr(MaxLength)
      else
         defMaxLength := '';
   end;
end;

procedure THEdit.validateMaxLength(var sMaxLength : string; var bOK : Boolean);
var
   n : integer;
begin
   if sMaxLength <> '' then begin
      try
         n := strToInt(sMaxLength);
         if n <= 0 then
            bOK := false;
      except
         bOK := false;
      end;
   end;
end;

procedure THEdit.getDefaultValue(var defValue : string);
begin
   defValue := TCustomEdit(_control).text;
end;

constructor THList.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Size', atInteger,
         'HTML <SELECT> Size. Specifies the number of visible items.');
      with tmpAttr do
         Options := Options - [aoEditable];
      add(tmpAttr);
   end;
end;

function THList.getHTMLBeginTag : string;
begin
   if Form2HTML.WebHubMode then
      result := sLineBreak + MacroStart 
   else
      result := '';
   result := result + '<SELECT';
end;

function THList.getHTMLEndTag : string;
begin
   result := '</SELECT>';
end;

function THList.getHTMLText : string;
var
   i, j : integer;
   nIncreaseHWidth, iLongestItem, nCarsToAdd : integer;
   brf : THBrowserRunFamily;
   tmpDouble : double;
   sText, sValue : string;
begin
   getItemsMaxLength(iLongestItem);
   nIncreaseHWidth := _control.Width - getStdHWidth;
   if nIncreaseHWidth > 0 then
      begin
         brf := bf2brf(Form2HTML.BrowserFamily);
         tmpDouble := nIncreaseHWidth;
         tmpDouble := tmpDouble / LIST_WIDTH_PER_CHARS[brf];
         nCarsToAdd := round(tmpDouble) + 1;
      end
   else
      nCarsToAdd := 0; // Just to avoid a compiler warning
   if not Form2HTML.WebHubMode then
      result := sLineBreak;
   if (_control is TCustomComboBox) then
   begin
     with TCustomComboBox(_control) do
     begin
        for i := 0 to items.count - 1 do begin
           if not Form2HTML.WebHubMode then
              begin
                 result := result + '<OPTION';
                 if i = itemIndex then
                    result := result + ' Selected';
                 result := result + ' Value="' + getItemValue(i) + '"';
                 result := result + '>' + getItemText(i);
              end
           else begin
              if i > 0 then
                 result := result + ',';
              sValue := getItemValue(i);
              sText := getItemText(i);
              if sText = sValue then
                result := result + sText
              else
                result := result + sValue + '-' + sText;
           end;
           if (nIncreaseHWidth > 0) and (i = iLongestItem) then begin
              for j := 1 to round(1.2 * nCarsToAdd) do
                 result := result + '&nbsp;';
           end;
           if not Form2HTML.WebHubMode then
              result := result + sLineBreak;
        end;
     end;
  end;
end;

function THList.getHTML : string;
var
   s : string;
begin
   if (not Form2HTML.WebHubMode) or (not enabled) then
      result := inherited getHTML
   else begin
      {if assigned(_form2HTML.OnBeforeRenderingControl) then
         _form2HTML.OnBeforeRenderingControl(self);}
      //INPUTSELECT|name,valuename,size[,MULTIPLE][|other tags]
      result:=
          MacroStart + 'SET|list.dyn0='+getHTMLText+MacroEnd+sLineBreak
         +MacroStart + 'INPUTSELECT|' + Attributes['Name'].Value
         +',dyn0,' + Attributes['Size'].Value;
      if (self is THListBox) then begin
         if Attributes['Multiple'].BooleanValue then
            //result := result + ',MULTIPLE';
            result := result + ',YES';
      end;
      s := Attributes['ExtraAttributes'].Value;
      if s <> '' then
         result := result + '|' + s;
      result := result + MacroEnd+sLineBreak+MacroStart + 'CLEAR|dyn0' + MacroEnd;
      addBeforeAndAfterElements(result);
      {if assigned(_form2HTML.OnAfterRenderingControl) then
         _form2HTML.OnAfterRenderingControl(self);}
   end;
end;

constructor THComboBox.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   tmpAttr := _attributes['Size'];
   with tmpAttr do begin
      DefValue := '1';
      Value := DefValue;
   end;
end;

function THList.getItemText(index : integer) : string;
var
   s : string;
   i : integer;
begin
  if (_control is TCustomComboBox) then
    s := TCustomComboBox(_control).items[index]
  else
  if (_control is TListBox) then
    s := TListBox(_control).items[index];

  i := Pos('=', s);
  if i = 0 then
    Result := s
  else
    Result := AllTrim(Copy(s, 1, i - 1));
end;

function THList.getItemValue(index : integer) : string;
var
   s : string;
   i : integer;
begin
  if (_control is TCustomComboBox) then
    s := TCustomComboBox(_control).items[index]
  else
  if (_control is TListBox) then
    s := TListBox(_control).items[index];


   i := Pos('=', s);
   if i = 0 then
      result := s
   else
      result := AllTrim(Copy(s, i + 1, MAXINT));
end;

function THList.getItemsMaxLength(var index : integer) : integer;
var
   i, l : integer;
begin
   result := 0; index := -1;
   if (_control is TListBox) then
   begin
     with TListBox(_control) do
     begin
        for i := 0 to items.count - 1 do
        begin
           l := length(getItemText(i));
           if l > result then
           begin
              result := l;
              index := i;
           end;
        end;
     end;
   end
   else
   if (_control is TCustomComboBox) then
   begin
     with TCustomComboBox(_control) do
     begin
        for i := 0 to items.count - 1 do
        begin
           l := length(getItemText(i));
           if l > result then
           begin
              result := l;
              index := i;
           end;
        end;
     end;
   end;
end;

function THList.getStdHWidth : integer;
var
   brf : THBrowserRunFamily;
   nMax, dummy : integer;
   tmpDouble : double;
begin
   nMax := getItemsMaxLength(dummy);
   brf := bf2brf(Form2HTML.BrowserFamily);
   result := LIST_MIN_WIDTH[brf];
   if nMax > LIST_MIN_WIDTH_CHARS[brf] then begin
      tmpDouble := LIST_WIDTH_PER_CHARS[brf];
      inc(result, round(tmpDouble * (nMax - LIST_MIN_WIDTH_CHARS[brf])));
   end;
end;

function THList.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   result := true;
   if inherited getHDims(hWidth, hHeight) then exit;
   // hWidth only (common to THComboBox and THListBox)
   hWidth := getStdHWidth;
   with _control do begin
      if hWidth < Width then
         hWidth := Width;
   end;
end;

function THComboBox.getHDims(var hWidth, hHeight : integer) : Boolean;
var
   brf : THBrowserRunFamily;
begin
   result := inherited getHDims(hWidth, hHeight); // hWidth computed in THList
   if Enabled then begin
      brf := bf2brf(Form2HTML.BrowserFamily);
      hHeight := COMBO_HEIGHT[brf];
   end;
end;

constructor THListBox.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Multiple', atBoolean,
         'HTML <SELECT> Multiple. If True, the user can select more than one option.');
      tmpAttr.GetDefaultProc := getDefaultMultiple;
      add(tmpAttr);
      tmpAttr := Attribute['Size'];
      with tmpAttr do
         GetDefaultProc := getDefaultSize;
   end;
end;

procedure THListBox.getDefaultMultiple(var defMultiple : string);
var
   bMultiple : Boolean;
begin
   bMultiple := THackListBox(_control).MultiSelect;
   defMultiple := THAttribute.boolean2String(bMultiple);
end;

procedure THListBox.getDefaultSize(var defSize : string);
var
   nItems, hi : integer;
   brf : THBrowserRunFamily;
begin
   brf := bf2brf(Form2HTML.BrowserFamily);
   hi := _control.Height - LIST_BASE_HEIGHT[brf];
   nItems := hi div LIST_ITEM_HEIGHT[brf];
   if (hi mod LIST_ITEM_HEIGHT[brf]) <> 0 then
      inc(nItems);
   defSize := intToStr(nItems);
end;

function THListBox.getHDims(var hWidth, hHeight : integer) : Boolean;
var
   nSize : integer;
   brf : THBrowserRunFamily;
begin
   result := inherited getHDims(hWidth, hHeight); // hWidth computed in THList
   if not enabled then
      exit;
   brf := bf2brf(Form2HTML.BrowserFamily);
   nSize := strToInt(_attributes['Size'].Value);
   hHeight := LIST_BASE_HEIGHT[brf] + LIST_ITEM_HEIGHT[brf] * nSize;
end;

function THInput.getHTMLBeginTag : string;
begin
   result := '<INPUT';
end;

constructor THCheck.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Checked', atBoolean,
         'HTML <INPUT> Checked. Determines the initial state of the checkbox or radio button.');
      tmpAttr.GetDefaultProc := getDefaultChecked;
      add(tmpAttr);
   end;
   addAnchorsAttributes;  // In THControl
end;

procedure THCheck.getDefaultName (var defaultName : string);
begin
   if isRadio then
      begin
         if _form2HTML.QualifiedNames then
            defaultName := _hContainer.getNamePath + '_Radio'
         else
            defaultName := 'Radio';
      end
   else
      inherited getDefaultName(defaultName);
end;

function THCheck.isRadio : Boolean;
begin
   result := (_control is TRadioButton);
end;

procedure THCheck.getDefaultChecked(var defChecked : string);
var
   bChecked : Boolean;
begin
   if (_control is TCustomCheckBox) then
      bChecked := THackCheckBox(_control).checked
   else
      bChecked := TRadioButton(_control).checked;
   defChecked := THAttribute.boolean2String(bChecked);
end;

constructor THCheckBox.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Type', atString,
         'HTML <INPUT> Type.');
      with tmpAttr do begin
         options := options - [aoEditable];
         DefValue := 'checkbox';
         Value := DefValue;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Value', atString,
         'HTML <INPUT> Value.');
      with tmpAttr do begin
         DefValue := 'YES';
         Value := DefValue;
      end;
      add(tmpAttr);
   end;
end;

function THCheckBox.getHTML : string;
var
   s : string;
begin
   if (not Form2HTML.WebHubMode) or (not enabled) then
      result := inherited getHTML
   else begin
      {if assigned(_form2HTML.OnBeforeRenderingControl) then
         _form2HTML.OnBeforeRenderingControl(self);}
      result := MacroStart + 'INPUTCHECKBOX|' + Attributes['Name'].Value;
      s := Attributes['ExtraAttributes'].Value;
      if s <> '' then
         result := result + '|' + s;
      result := result + MacroEnd;
      result := result + getHTMLText;
      addBeforeAndAfterElements(result);
      {if assigned(_form2HTML.OnAfterRenderingControl) then
         _form2HTML.OnAfterRenderingControl(self);}
   end;
end;

constructor THRadio.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Type', atString,
         'HTML <INPUT> Type.');
      with tmpAttr do begin
         options := options - [aoEditable];
         DefValue := 'radio';
         Value := DefValue;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Value', atString,
         'HTML <INPUT> Value.');
      with tmpAttr do
         GetDefaultProc := getDefaultValue;
      add(tmpAttr);
   end;
end;

procedure THRadio.getDefaultValue(var defValue : string);
begin
   defValue := _control.Name;
end;

function THRadio.getHTML : string;
var
   s : string;
begin
   if (not Form2HTML.WebHubMode) or (not enabled) then
      result := inherited getHTML
   else begin
      {if assigned(_form2HTML.OnBeforeRenderingControl) then
         _form2HTML.OnBeforeRenderingControl(self);}
      result := MacroStart + 'INPUTRADIO|' + Attributes['Name'].Value +
         ',' + Attributes['Value'].Value;
      s := Attributes['ExtraAttributes'].Value;
      if s <> '' then
         result := result + '|' + s;
      result := result + MacroEnd;
      result := result + getHTMLText;
      addBeforeAndAfterElements(result);
      {if assigned(_form2HTML.OnAfterRenderingControl) then
         _form2HTML.OnAfterRenderingControl(self);}
   end;
end;

function THCheck.getHDims(var hWidth, hHeight : integer) : Boolean;
var
   brf : THBrowserRunFamily;
   tw, th : integer;
begin
   result := true;
   if inherited getHDims(hWidth, hHeight) then
      exit;
   brf := bf2brf(Form2HTML.BrowserFamily);
   hHeight := CHECK_MIN_HEIGHT[brf];
   hWidth := CHECK_MIN_WIDTH[brf];
   _form2HTML.getHDimsOfText(self, THackCheckBox(_control).caption, tw, th);
   inc(hWidth, tw);
   if hHeight < th then
      hHeight := th;
end;

function THCheck.getHTMLText: string;
var
   sCaption, sFont : string;
begin
   sCaption := removeChars(THackCheckBox(_control).caption, '&');
   sFont := _form2HTML.getFontAttr(self, '');
   if _attributes['AsAnchor'].BooleanValue then
      begin
         if _form2HTML.WebHubMode then
            result := _attributes['WHAnchorMacro'].DefValue
         else
            result := sFont + '<A Href="' + _attributes['Href'].Value + '">' +
                        sCaption + '</A>';
      end
   else
      result := sFont + sCaption;
end;

constructor TWHForm2HTML.create(owner : TComponent);
begin
   inherited create(owner);
   _bmp := TBitmap.create;
   _hForm := THFormElement.create;
   _hForm.Form2HTML := self;
   _bChunkOnly := False;
   _BGColor := '[None]';
   _bSetFonts := True;
   _bWHChunkDeclaration := True;
   MainContainer := nil; // needed for initialization through setMainContainer !
end;

destructor TWHForm2HTML.destroy;
begin
   _bmp.free;
   _hForm.free;
   _hMainContainer.free;
   inherited destroy;
end;

function TWHForm2HTML.getVersion : string;
begin
   result := VERSION_NUMBER;
end;

procedure TWHForm2HTML.setVersion(const s : string);
begin
end;

function TWHForm2HTML.getFontOfHCtl(hctl : THControl) : TFont;
begin
   if hctl is THLabel then
      result := THackLabel(hctl.Control).Font
   else if hctl is THCheckBox then
      result := THackCheckBox(hctl.Control).Font
   else if hctl is THRadio then
      result := TRadioButton(hctl.Control).Font
   else
      result := TForm(owner).Font;  
end;

function TWHForm2HTML.hFontSize2FontSize(hfSize : integer) : integer;
begin
   case hfSize of
      1:
         result := 8;
      2:
         result := 10;
      3:
         result := 12;
      4:
         result := 14;
      5:
         result := 18;
      6:
         result := 24;
      else             // 7
         result := 36;
   end;
end;

function TWHForm2HTML.fontSize2HFontSize(fSize : integer) : integer;
begin
   if fSize < 10 then
      result := 1
   else if fSize < 12 then
      result := 2
   else if fSize < 14 then
      result := 3
   else if fSize < 18 then
      result := 4
   else if fSize < 24 then
      result := 5
   else if fSize < 36 then
      result := 6
   else
      result := 7;
end;

function TWHForm2HTML.getFontAttr(hctl : THControl; const sColor : string) : string;
var
   font : TFont;
   sRealColor : string;
   function getNames : string;
   var
      s : string;
   begin
      result := font.Name;
      s := allTrim(_alternateFonts);
      if s <> '' then begin
         if copy(s, 1, 1) <> ',' then
            result := result + ',';
         result := result + s;
      end;
   end;
begin
   if not _bSetFonts then
      begin
         if sColor = '' then
            result := ''
         else
            result := '<FONT Color="' + sColor + '">';
      end
   else begin
      font := getFontOfHCtl(hctl);
      if sColor = '' then
         sRealColor := Color2HColor(font.Color)
      else
         sRealColor := sColor;
      result := '<FONT Face="' + getNames + '" Size="' +
         intToStr(fontSize2HFontSize(font.size)) + '" Color="' +
         sRealColor + '">';
      with font do begin
         if fsBold in Style then
            result := result + '<B>';
         if fsItalic in Style then
            result := result + '<I>';
         if fsUnderline in Style then
            result := result + '<U>';
      end;
   end;
end;

function TWHForm2HTML.getHDimsOfText(hctl : THControl; const s : string; var hWidth, hHeight : integer) : Boolean;
var
   hfSize : integer;
begin
   result := True;
   with _bmp.canvas do begin
      if _bSetFonts then
         begin
            with font do begin
               assign(getFontOfHCtl(hctl));
               hfSize := fontSize2HFontSize(Size);
               Size := hFontSize2FontSize(hfSize);
            end;
         end
      else begin
         with font do begin         // Reference browser default font
            name := 'Times New Roman';
            size := 12;
         end;
      end;
      hWidth := TextWidth(s);
      hHeight := TextHeight(s);
   end;
end;

function TWHForm2HTML.BGColorAttr : string;
begin
   if CompareText(_BGColor, '[None]') = 0 then
      result := ''
   else if CompareText(_BGColor, '[Same]') = 0 then
      result := Color2HColor(TForm(owner).color)
   else
      result := _BGColor;
   if result <> '' then
      result := ' BGColor="' + result + '"';
end;

procedure TWHForm2HTML.setCGIUserAgent(const cua : string);
var
   s : string;
begin
   s := upperCase(cua);
   if copy(s, 1, 9) = 'MOZILLA/4' then
      begin
         if pos('MSIE 4', s) > 0 then
            _browserFamily := bfIE4
         else
            _browserFamily := bfNS4;
      end
   else if copy(s, 1, 8) = 'MOZILLA/' then
      begin
         if pos('MSIE 3', s) > 0 then
            _browserFamily := bfIE3
         else
            _browserFamily := bfNS3;
      end
   else
      _browserFamily := bfDefault;      // same as bfIE4
end;

procedure TWHForm2HTML.setMainContainer(mainContainer : TWinControl);
var
   hCtlClass : THControlClass;
   savedHMainContainer : THContainer;
   procedure restoreAttributesFrom(fromObj : THContainer);
   var
      i : integer;
      hObj, hOldObj : THAttrObject;
   begin
      if _hMainContainer.Name = fromObj.Name then
         _hMainContainer.copyAttributesFrom(fromObj);
      with fromObj do begin
         for i := 0 to count - 1 do begin
            hOldObj := Children[i];
            hObj := _hMainContainer.FindHObjectByName(hOldObj.Name);
            if hObj <> nil then
               hObj.copyAttributesFrom(hOldObj);
            if hOldObj is THContainer then
               restoreAttributesFrom(THContainer(hOldObj));
         end;
      end;
   end;
begin
   if (mainContainer = _mainContainer) and (mainContainer <> nil) then exit;
   savedHMainContainer := nil;
   try
      savedHMainContainer := _hMainContainer;
      if mainContainer = nil then
         mainContainer := TForm(owner);
      _mainContainer := mainContainer;
      hCtlClass := getHContainerClass(mainContainer);
      if hCtlClass <> nil then begin
         _hMainContainer := THContainer(hCtlClass.create(mainContainer));
         _hMainContainer.Form2HTML := self;
         synchronize;
      end;
      if savedHMainContainer <> nil then
         restoreAttributesFrom(savedHMainContainer);
   finally
      savedHMainContainer.free;
   end;
end;

procedure TWHForm2HTML.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation = opRemove) and (AComponent = _mainContainer) then
      MainContainer := nil;
   inherited Notification(AComponent, Operation);
end;

procedure THAttribute.writeData(writer : TWriter);
begin
   with writer do begin
      WriteString(Name);
      WriteString(Value);
   end;
end;

procedure THAttributes.writeData(writer : TWriter);
var
   i : integer;
begin
   writer.WriteListBegin;
   for i := 0 to count - 1 do
      AttributeN[i].writeData(writer);
   writer.WriteListEnd;
end;

procedure THContainer.writeData(writer: TWriter);
var
   i : integer;
begin
   inherited writeData(writer);
   for i := 0 to count - 1 do
      Children[i].writeData(writer);
end;

constructor THExtStoredList.create;
begin
   inherited create;
   _list := TList.create;
end;

destructor THExtStoredList.destroy;
var
   i : integer;
begin
   with _list do begin
      for i := 0 to count - 1 do
         THExtStored(items[i]).free;
      free;
   end;
   inherited destroy;
end;

constructor THExtStored.create(bIsAttr : Boolean);
begin
   inherited create;
   _bIsAttr := bIsAttr;
end;

procedure THExtStored.readData(Reader : TReader);
begin
   with Reader do begin
      _name := readString;
      if _bIsAttr then
         _value := readString;
   end;
end;

procedure THExtStoredList.readData(Reader : TReader);
var
   extStored : THExtStored;
begin
   with Reader do begin
      ReadListBegin;
      while not EndOfList do begin
         extStored := THExtStored.create(False); // an object
         extStored.readData(Reader);
         _list.add(extStored);
         ReadListBegin;
         while not EndOfList do begin // attributes
            extStored := THExtStored.create(True); // an attribute
            extStored.readData(Reader);
            _list.add(extStored);
         end;
         ReadListEnd;
      end;
      ReadListEnd;
   end;
end;

procedure TWHForm2HTML.readData(reader: TReader);
begin
   if _extStoredList <> nil then exit;
   _extStoredList := THExtStoredList.create;
   _extStoredList.readData(reader);
end;

procedure TWHForm2HTML.loaded;
var
   i : integer;
   extStored : THExtStored;
   attrObject : THAttrObject;
   attr : THAttribute;
begin
   inherited loaded;
   if _extStoredList = nil then exit;
   synchronize;
   try
      attrObject := _hForm;
      with _extStoredList.List do begin
         // First item (0) is _hForm
         for i := 1 to count - 1 do begin
            extStored := THExtStored(items[i]);
            if extStored.IsAttr then
               begin
                  if attrObject <> nil then begin
                     with attrObject do begin
                        attr := Attributes[extStored.Name];
                        if attr <> nil then
                           attr.Value := extStored.Value;
                     end;
                  end;
               end
            else
               attrObject := HMainContainer.FindHObjectByName(extStored.Name);
         end;
      end;
   finally
      _extStoredList.free;
      _extStoredList := nil;
   end;
end;

procedure TWHForm2HTML.writeData(writer: TWriter);
begin
   synchronize;
   writer.WriteListBegin;
   _hForm.writeData(writer);
   _hMainContainer.writeData(writer);
   writer.WriteListEnd;
end;

function THAttribute.ValueEquals(compObj : THAttribute) : Boolean;
begin
   result := (Value = compObj.Value);
end;

function THAttributes.ValueEquals(compObj : THAttributes) : Boolean;
var
   i : integer;
begin
   result := (count = compObj.count);
   if result then begin
      for i := 0 to count - 1 do begin
         result := AttributeN[i].ValueEquals(compObj.AttributeN[i]);
         if not result then
            break;
      end;
   end;
end;


constructor THElement.createHAttrObject;
var
   tmpAttr : THAttribute;
begin
   inherited createHAttrObject;
   with _attributes do begin
      tmpAttr := THAttribute.create('BeforeElement', atString,
         'TWHForm2HTML option. Let''s you insert some HTML code before the element.');
      with tmpAttr do
         options := options - [aoHTML];
      add(tmpAttr);
      tmpAttr := THAttribute.create('AfterElement', atString,
         'TWHForm2HTML option. Let''s you insert some HTML code after the element.');
      with tmpAttr do
         options := options - [aoHTML];
      add(tmpAttr);
      tmpAttr := THAttribute.create('ExtraAttributes', atString,
         'Let''s you insert additional HTML attributes such as JavaScript handlers.');
      with tmpAttr do
         options := options + [aoNoName];
      add(tmpAttr);
   end;
end;

function THElement.getAfterElement : string;
begin
   result := Attributes['AfterElement'].Value;
end;

procedure THElement.addBeforeAndAfterElements(var toS : string);
var
   s : string;
begin
   toS := getBeforeElement + toS;
   s := getAfterElement;
   if s <> '' then
      s := sLineBreak + s;
   toS := toS + s;
end;

function THElement.getBeforeElement : string;
begin
   result := Attributes['BeforeElement'].Value;
   if result <> '' then
      result := result + sLineBreak;
end;

function THAttrObject.ValueEquals(compObj : THAttrObject) : Boolean;
begin
   result := _attributes.ValueEquals(compObj.Attributes);
end;

function THContainer.ValueEquals(compObj : THAttrObject) : Boolean;
var
   i : integer;
begin
   result := inherited ValueEquals(compObj);
   if result then
      result := (count = THContainer(compObj).count);
   if result then begin
      for i := 0 to count - 1 do begin
         result := Children[i].ValueEquals(THContainer(compObj).Children[i]);
         if not result then
            break;
      end;
   end;
end;

function TWHForm2HTML.ValueEquals(compObj : TWHForm2HTML) : Boolean;
begin
   result := _hForm.ValueEquals(compObj.HForm);  // <FORM> element : is  a THAttrObject
   if result then
      result := _hMainContainer.ValueEquals(compObj.HMainContainer);  // is a THContainer
end;

procedure TWHForm2HTML.DefineProperties(Filer: TFiler);
   function DoFile: Boolean;
   begin
      if Filer.Ancestor <> nil then
         result := not ValueEquals(TWHForm2HTML(Filer.Ancestor))
      else
         result := true;
   end;
begin
   inherited DefineProperties(Filer);
   with Filer do
      DefineProperty('Links', readData, writeData, DoFile);
end;

function TWHForm2HTML.getBrowseIt : Boolean;
begin
   result := false;
end;

procedure TWHForm2HTML.setBrowseIt(b : Boolean);
begin
   browseHTMLDialog;
end;

procedure TWHForm2HTML.setBrowseWHChunk(b : Boolean);
begin
   browseWHChunkDialog;
end;

function TWHForm2HTML.editExport : Boolean;
var
   saveDlg : TSaveDialog;
   testExt : string;
begin
   result := false;
   saveDlg := nil;
   try
      saveDlg := TSaveDialog.create(application);
      with saveDlg do begin
         if _bWebHubMode then
            begin
               Title := ' Export WebHub chunk';
               FileName := ExportWHChunkFName;
               Filter := 'Text files (*.txt)|*.txt|HTML files (*.HTML)|*.HTML|All files (*.*)|*.*';
               DefaultExt := 'txt';
            end
         else begin
            Title := ' Export HTML';
            FileName := ExportFName;
            Filter := 'HTML files (*.HTML)|*.HTML|All files (*.*)|*.*';
            DefaultExt := 'HTML';
         end;
         Options := [ofHideReadOnly, ofNoReadOnlyReturn, ofOverwritePrompt, ofPathMustExist];
         if execute then begin
            if _bWebHubMode then
               ExportWHChunkFName := FileName
            else
               ExportFName := FileName;
            //<TSaveDialog.bug>    // on extensions with more than 3 char's
            testExt := extractFileExt(FileName);
            if upperCase(testExt) = '.HTM' then begin
               if copy(testExt, 4, 1) = 'M' then
                  ExportFName := ExportFName + 'L'
               else
                  ExportFName := ExportFName + 'l';
            end;
            //</TSaveDialog.bug>
            result := doExport;
         end;
      end;
   finally
      saveDlg.free;
   end;
end;

function TWHForm2HTML.getHTMLString : string;
var
   bForm : Boolean;
begin
   try
      synchronize;
      if _bWebHubMode then begin
         _hForm.WebHubMode := True;
         _hMainContainer.WebHubMode := True;
      end;
      if assigned(_OnBeforeRendering) then
         _OnBeforeRendering(_hMainContainer._control, _hMainContainer);
      result := getFormBeginTag;
      bForm := (not _bWebHubMode) or (_WHChunkOutputScope in [cosAll, cosForm]);
      if bForm then
         result := result + _hMainContainer.HTML;
      result := result + getFormEndTag;
   finally
      if _bWebHubMode then begin
         _hForm.WebHubMode := False;
         _hMainContainer.WebHubMode := False;
         _bWebHubMode := False;
      end;
      if assigned(_OnRendering) then
         _OnRendering(_hMainContainer._control, _hMainContainer, result);
      if assigned(_OnAfterRendering) then
         _OnAfterRendering(_hMainContainer._control, _hMainContainer);
   end;
end;

function TWHForm2HTML.getHTML : string;
begin
   _bWebHubMode := False;
   result := getHTMLString;
end;

function TWHForm2HTML.getWHChunk : string;
begin
   _bWebHubMode := True;
   result := getHTMLString;
end;
                             
function TWHForm2HTML.doExport : Boolean;
var
   sh : AnsiString;
   hf : THandle;
   expFName: string;
   msgTitle : string;
begin
   result := false;
   if _bWebHubMode then
      begin
         expFName := _exportWHChunkFName;
         msgTitle := 'Export WebHub chunck';
      end
   else begin
      expFName := _exportFName;
      msgTitle := 'Export HTML';
   end;
   if expFName = '' then
      exit;
   hf := INVALID_HANDLE_VALUE;
   try
      screen.cursor := crHourglass;
      sh := AnsiString(getHTMLString);
      hf := CreateFile(
            pChar(expFName),
            GENERIC_READ or GENERIC_WRITE,
            0,
            nil,
            CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            0);
      if hf <> INVALID_HANDLE_VALUE then begin
         if integer(_lWrite(hf, pAnsiChar(sh), length(sh))) = length(sh) then
            result := true;
      end;
   finally
      CloseHandle(hf);
      screen.cursor := crDefault;
      if not result then
      begin
         Forms.Application.MessageBox(
            pChar('Cannot create file ''' +
               expFName + ''' !'),
               pChar(msgTitle),
               MB_APPLMODAL or MB_ICONERROR or MB_OK);
      end;
   end;
end;

function TWHForm2HTML.edit : Boolean;
begin
   try
      screen.cursor := crHourglass;
      synchronize;
      f2hFrm := Tf2hFrm.create(application);
      with f2hFrm do begin
         Form2HTML := self;
         screen.cursor := crDefault;
         result := (showModal = mrOK);
      end;
   finally
      f2hFrm.free;
      f2hFrm := nil;
      screen.cursor := crDefault;
   end;
end;

procedure TWHForm2HTML.doBrowse(bWH : Boolean);
var
   bCanBrowse : Boolean;
   expFName : string;
begin
   _bWebHubMode := bWH;
   if bWH then
      expFName := _exportWHChunkFName
   else
      expFName := _exportFName;
   if expFName = '' then
      bCanBrowse := editExport
   else
      bCanBrowse := doExport;
   if bCanBrowse then begin
      if ShellExecute(
         0,                // hwnd
         nil,              // operation
         pChar(expFName),
         nil,              // parameters
         nil,              // default directory
         SW_SHOWDEFAULT    // nShowCmd
         ) <= 32 then
            Forms.Application.MessageBox(
               pChar('Cannot open file ''' +
               expFName + ''' !'),
               'Browse',
               MB_APPLMODAL or MB_ICONERROR or MB_OK);
   end;
end;

procedure TWHForm2HTML.browseHTMLDialog;
begin
   doBrowse(False);
end;

procedure TWHForm2HTML.browseWHChunkDialog;
begin
   doBrowse(True);
end;

function TWHForm2HTML.exportHTMLDialog : Boolean;
begin
   _bWebHubMode := False;
   result := editExport;
end;

function TWHForm2HTML.exportHTML : Boolean;
begin
   _bWebHubMode := False;
   result := doExport;
end;

function TWHForm2HTML.exportWHChunk : Boolean;
begin
   _bWebHubMode := True;
   result := doExport;
end;

function TWHForm2HTML.exportWHChunkDialog : Boolean;
begin
   _bWebHubMode := True;
   result := editExport;
   _bWebHubMode := False;
end;

procedure TWHForm2HTML.synchronize;
begin
   if _hMainContainer = nil then exit;
   with _hMainContainer do begin
      synchronized := false;
      synchronize;
      synchronizeDone;
   end;
end;

constructor THPlacer.create(hControl : THControl; width, height, xPrec, yPrec : integer);
var
   n : integer;
begin
   inherited create;
   _hContainer := THContainer(hControl);
   if _hContainer is THGroupBox then
      dec(height, 10);
   _form2HTML := hControl.form2HTML;
   _xPrec := xPrec; _yPrec := yPrec;
   _rows := TList.create;
   n := height div _yPrec;
   if (height mod _yPrec) <> 0 then
      inc(n);
   addRows(n);
   n := width div _xPrec;
   if (width mod _xPrec) <> 0 then
      inc(n);
   addCols(n);
end;

destructor THPlacer.destroy;
var
   i : integer;
begin
   for i := 0 to rowCount - 1 do
      TList(_rows[i]).free;
   _rows.free;
   inherited destroy;
end;

function THPlacer.colCount : integer;
begin
   if rowCount = 0 then
      result := 0
   else
      result := TList(_rows[0]).count;
end;

function THPlacer.rowCount : integer;
begin
   result := _rows.count;
end;

procedure THPlacer.addRows(nRows : integer);
var
   i, j, nRow : integer;
begin
   for i := 1 to nRows do begin
      nRow := _rows.Add(TList.create);
      for j := 1 to colCount do
         TList(_rows[nRow]).add(nil);
   end;
end;

procedure THPlacer.addCols(nCols : integer);
var
   i, j : integer;
begin
   for i := 0 to rowCount - 1 do begin
      for j := 1 to nCols do
         TList(_rows[i]).Add(nil);
   end;
end;

function THPlacer.getCell(col, row : integer) : THControl;
begin
   result := nil;
   if (row > rowCount - 1) or (col > colCount -1) then
      exit;
   result := THControl(TList(_rows[row])[col]);
end;

procedure THPlacer.setCell(col, row : integer; hControl : THControl);
begin
   if (row > rowCount - 1) then
      addRows(row - (rowCount - 1));
   if (col > colCount - 1) then
      addCols(col - (colCount -1));
   TList(_rows[row])[col] := hControl;
end;

function THPlacer.getHTML : string;
var
   r, c, n, nt, cur, i : integer;
   b : Boolean;
   hCtls, tmpHCtls, hCols, hRows : TList;
   hControl, hc : THControl;
   ret, s : string;
   procedure writeTable;
   var
      cellsDone, tmpList, notRenderedList : TList;
      i, j, k, r, c, nEmpty : integer;
      procedure newCell(bNewLine : Boolean);
      begin
         if (bNewLine and (c > 0)) or (r = 0) then
            ret := ret + sLineBreak;
         ret := ret + '<TD';
         if c = 0 then
            ret := ret + ' Height="' + intToStr(integer(hRows[r])) + '"';
         if r = 0 then
            ret := ret + ' Width="' + intToStr(integer(hCols[c])) + '"';
      end;
   begin
      cellsDone := nil;
      notRenderedList := nil;
      try
         cellsDone := TList.create;
         for i := 0 to hRows.count - 1 do begin
            tmpList := TList.create;
            for j := 0 to hCols.count - 1 do
               tmpList.add(nil);
            cellsDone.add(tmpList);
         end;
         ret := '<TABLE' + _form2HTML.BGColorAttr + ' Cellspacing="0" Cellpadding="0">';
         with hCtls do
            for i := 0 to count - 1 do
               THControl(items[i])._bRendered := false;
         for r := 0 to hRows.count - 1 do begin
            ret := ret + sLineBreak + '<TR>';
            nEmpty := 0;
            for c := 0 to hCols.count - 1 do begin
               if TList(cellsDone[r])[c] <> nil then
                  continue;
               with hCtls do begin
                  b := True;
                  for i := 0 to count - 1 do begin
                     hc := THControl(items[i]);
                     with hc do begin
                        if (_row = r) and (_col = c) then begin
                           _bRendered := true;
                           b := False;
                           if nEmpty > 0 then begin
                              for k := 1 to nEmpty do begin
                                 newCell(False);
                                 ret := ret + '></TD>';
                              end;
                              nEmpty := 0;
                           end;
                           newCell(True);
                           if _colspan > 1 then
                              ret := ret + ' Colspan="' + intToStr(_colspan) + '"';
                           if _rowspan > 1 then
                              ret := ret + ' Rowspan="' + intToStr(_rowspan) + '"';
                           if RightAligned then     // labels placed on the left side
                              ret := ret + ' Align="right" VAlign="top"'
                           else if hControl is THLabel then  // labels placed on the top side
                              ret := ret + ' Align="left" VAlign="bottom"'
                           else
                              ret := ret + ' Align="left" VAlign="top"';
                           ret := ret + '>';
                           with _form2HTML do begin
                                 if assigned(_OnBeforeRendering) then
                                    _OnBeforeRendering(hc._control, hc);
                                 s :=  hc.HTML;
                                 if assigned(_OnRendering) then
                                    _OnRendering(hc._control, hc, s);
                                 ret := ret + s;
                                 if assigned(_OnAfterRendering) then
                                    _OnAfterRendering(hc._control, hc);
                           end;
                           for j := 0 to _rowspan - 1 do begin
                              for k := 0 to _colspan - 1 do
                                 TList(cellsDone[r + j])[c + k] := pointer(1);
                           end;
                           break;
                        end;
                     end;
                  end;
                  if b then // Empty cell
                     begin
                        if (r = 0) or (c = 0) then
                           begin
                              newCell(False);
                              ret := ret + '></TD>';
                           end
                        else
                           inc(nEmpty);
                     end
                  else
                     ret := ret + '</TD>';
               end;
            end;
            ret := ret + '</TR>';
         end;
         notRenderedList := TList.create;
      finally
         ret := ret + sLineBreak + '</TABLE>' + sLineBreak;
         with cellsDone do begin
            for i := 0 to count - 1 do
               TList(items[i]).free;
            free;
         end;
         with hCtls do
            for i := 0 to count - 1 do
               if not THControl(items[i])._bRendered then
                  notRenderedList.add(THControl(items[i]));
         with notRenderedList do begin
            if count > 0 then begin
               s := 'The following controls could not be rendered' + sLineBreak +
                  'because overlapped in the target form :' + sLineBreak;
               for i := 0 to count - 1 do
                  s := s + '- ' + THControl(items[i]).name + sLineBreak;
               s := s + 'Some controls are too close together in the source form.' + sLineBreak +
                  'Spread them and try again...';
               {$IFDEF CodeSite}
               CodeSite.SendWarning(S);
               {$ENDIF}
               {$IFDEF IN_WEB_APPLICATION}
               LogSendWarning(S, 'getHTML');
               {$ELSE}
               Forms.Application.messageBox(pChar(s), 'Lost controls',
                 MB_ICONWARNING	or MB_OK);
               {$ENDIF}
            end;
            free;
         end;
      end;
   end;
begin
   hCols := nil;
   hRows := nil;
   hCtls := nil;
   tmpHCtls := nil;
   ret := '';
   try
      hCols := TList.create;
      hRows := TList.create;
      hCtls := TList.create;
      tmpHCtls := TList.create;
      // First step : optimizing columns widths
      cur := 0;
      nt := 0;
      while cur < colCount do begin
         n := colCount;
         if cur > 0 then begin
            b := False;
            for r := 0 to rowCount - 1 do begin
               hControl := Cell[cur - 1, r];
               if (hControl <> nil) and (hControl <> Cell[cur, r]) then begin
                  with tmpHCtls do begin
                     i := indexOf(hControl);
                     if i <> -1 then begin
                        tmpHCtls[i] := nil;
                        b := true;
                     end
                  end;
               end;
            end;
            if b then
               tmpHCtls.pack;
         end;
         for r := 0 to rowCount - 1 do begin
            hControl := Cell[cur, r];
            if hControl <> nil then begin
               with tmpHCtls do begin
                  if indexOf(hControl) = -1 then begin
                     add(hControl);
                     hControl._col := hCols.count;
                  end;
               end;
               with hCtls do begin
                  if indexOf(hControl) = -1 then
                     add(hControl);
               end;
            end;
            i := 0;
            for c := cur to colCount - 1 do begin
               hc := Cell[c, r];
               if (hc = hControl) or (hc = nil) then
                  inc(i)
               else begin
                  if i < n then
                     n := i;
                  break;
               end;
            end;
         end;
         if nt + n < colCount then
            inc(nt, n)
         else
            n := colCount - nt;
         hCols.add(pointer(n * _xPrec));
         inc(cur, n);
         with tmpHCtls do begin
            for i := 0 to count - 1 do
               inc(THControl(items[i])._colspan);
         end;
      end;
      // Second step : optimizing rows heights
      cur := 0;
      nt := 0;
      tmpHCtls.clear;
      while cur < rowCount do begin
         n := rowCount;
         if cur > 0 then begin
            b := False;
            for c := 0 to colCount - 1 do begin
               hControl := Cell[c, cur - 1];
               if (hControl <> nil) and (hControl <> Cell[c, cur]) then begin
                  with tmpHCtls do begin
                     i := indexOf(hControl);
                     if i <> -1 then begin
                        tmpHCtls[i] := nil;
                        b := true;
                     end
                  end;
               end;
            end;
            if b then
               tmpHCtls.pack;
         end;
         for c := 0 to colCount - 1 do begin
            hControl := Cell[c, cur];
            if hControl <> nil then begin
               with tmpHCtls do begin
                  if indexOf(hControl) = -1 then begin
                     add(hControl);
                     hControl._row := hRows.count;
                  end;
               end;
            end;
            i := 0;
            for r := cur to rowCount - 1 do begin
               hc := Cell[c, r];
               if (hc = hControl) or (hc = nil) then
                  inc(i)
               else begin
                  if i < n then
                     n := i;
                  break;
               end;
            end;
         end;
         if nt + n < rowCount then
            inc(nt, n)
         else
            n := rowCount - nt;
         hRows.add(pointer(n * _yPrec));
         inc(cur, n);
         with tmpHCtls do begin
            for i := 0 to count - 1 do
               inc(THControl(items[i])._rowspan);
         end;
      end;
      // Colspan's / Rowspan's computing
      with hCtls do begin
         for i := 0 to count - 1 do begin
            hc := THControl(items[i]);
            with hc do begin
               getHDims(c, r);
               n := _col;
               _colspan := 0;
               while (c > 0) and (n < hCols.count) do begin
                  inc(_colspan);
                  dec(c, integer(hCols[n]));
                  inc(n);
               end;
               n := _row;
               _rowspan := 0;
               while (r > 0) and (n < hRows.count) do begin
                  inc(_rowspan);
                  dec(r, integer(hRows[n]));
                  inc(n);
               end;
            end;
         end;
      end;
      FreeAndNil(tmpHCtls);
      writeTable;
   finally
      result := ret;
      FreeAndNil(hCols);
      FreeAndNil(hRows);
      FreeAndNil(hCtls);
      FreeAndNil(tmpHCtls);
   end;
end;

procedure THPlacer.placeControl(hControl : THControl);
var
   left, top : integer;
   hWidth, hHeight : integer;
   cLeft, cWidth, cTop, cHeight : integer;
   i, j : integer;
   bRightAligned : Boolean;
begin
   if not hControl.Visible then exit;
   bRightAligned := false;
   hControl.getHDims(hWidth, hHeight);
   if (not _form2HTML.setFonts) and (hControl.control is TCustomLabel) then begin
      with THackLabel(hControl.control) do begin
         if focusControl <> nil then
            if focusControl.left >= left + width then
               bRightAligned := true;
      end;
   end;
   hControl.RightAligned := bRightAligned;
   left := hControl.control.left;
   top := hControl.control.top;
   if _hContainer is THGroupBox then
      dec(top, 10);
   if (hControl is THEdit) and (not hControl.enabled) then begin
      inc(top, 4);
      inc(left, 4);
   end;
   if bRightAligned then
      inc(left, hControl.control.width);
   cLeft := left div _xPrec;
   cTop := top div _yPrec;
   cWidth := hWidth div _xPrec;
   if (hWidth mod _xPrec) <> 0 then
      inc(cWidth);
   cHeight := hHeight div _yPrec;
   if (hHeight mod _yPrec) <> 0 then
      inc(cHeight);
   if bRightAligned then begin
      dec(cLeft, cWidth);
      if cLeft < 0 then cLeft := 0;
   end;
   for i := 0 to cWidth - 1 do begin
      for j := 0 to cHeight - 1 do
         Cell[cLeft + i, cTop + j] := hControl;
   end;
end;

procedure THPlacer.getHDims(var hWidth, hHeight : integer);
begin
   hWidth := colCount * _xPrec;
   hHeight := rowCount * _yPrec;
end;

constructor THFormElement.create;
var
   tmpAttr : THAttribute;
begin
   inherited createHAttrObject;
   with _attributes do begin
      add(THAttribute.create('Action', atString,
         'HTML <FORM> Action tag. Enter URL or WebHub PageID.'));
      add(THAttribute.create('Enctype', atString,
         'HTML <FORM> Enctype.'));
      tmpAttr := THAttribute.create('Method', atList,
         'HTML <FORM> Method. The possible values are "get" and "post".');
      with tmpAttr.ValuesList do begin
         add('post');
         add('get');
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('WHAction', atString,
         'HTML <FORM> Action tag, used when exporting as a WebHub chunk.');
      with tmpAttr do begin
         Options := Options - [aoHTML] - [aoEditable] + [aoWebHub];
         StdAttrName := 'Action';
         GetDefaultProc := getDefaultWHAction;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('WHTargetPageID', atString,
         'The destination page ID, using WebHub syntax of [AppID:]PageID[(ServerID)].');
      with tmpAttr do begin
         Options := Options - [aoHTML];
         GetDefaultProc := getDefaultWHTargetPageID;
         ValueChangedProc := updateWHAction;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('WHCbk', atBoolean,
         'Sets the .CKB command, which is used to enable save-state with checkboxes in WebHub.');
      with tmpAttr do begin
         Options := Options - [aoHTML];
         DefValue := boolean2String(False);
         Value := DefValue;
         ValueChangedProc := updateWHAction;
      end;
      add(tmpAttr);
   end;
end;

procedure THFormElement.getDefaultWHTargetPageID(var defWHTargetPageID : string);
begin
   defWHTargetPageID := 'pg' + Form2HTML.MainContainer.Name;
end;

procedure THFormElement.getDefaultWHAction(var defWHAction : string);
var
   s : string;
begin
   with _attributes do begin
      s := MacroStart + 'ACTION|' + alltrim(Attribute['WHTargetPageID'].Value);
      if Attribute['WHCbk'].BooleanValue then
         s := s + ',.CKB';
      s := s + MacroEnd;
      defWHAction := s;
   end;
end;

procedure THFormElement.updateWHAction(attr : THAttribute);
begin
   with _attributes['WHAction'] do
      Value := DefValue;
end;

function THFormElement.getName : string;
begin
   result := '<FORM>..</FORM>';
end;

constructor THForm.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Border', atBoolean,
         'TWHForm2HTML option. If True, the form is rendered with a border around it.');
      with tmpAttr do
         options := options - [aoHTML];
      add(tmpAttr);
      tmpAttr := THAttribute.create('CaptionBGColor', atString,
         'TWHForm2HTML option. Let''s you specify the background color of the title bar.');
      with tmpAttr do begin
         options := options - [aoHTML] + [aoDialog];
         DialogProc := CaptionColorDlg;
         Value := 'Navy';
         DefValue := Value;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('CaptionColor', atString,
         'TWHForm2HTML option. Let''s you specify the text color of the caption.');
      with tmpAttr do begin
         options := options - [aoHTML] + [aoDialog];
         DialogProc := CaptionColorDlg;
         Value := 'White';
         DefValue := Value;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('ShowCaption', atBoolean,  // Must be the last one !
         'TWHForm2HTML option. If True, the form is rendered with a title bar and a border around it.');
      with tmpAttr do begin
         options := options - [aoHTML];
         ValueChangedProc := ShowCaptionChanged;
         ShowCaptionChanged(tmpAttr);
      end;
      add(tmpAttr);
   end;
end;

procedure THForm.CaptionColorDlg(attr : THAttribute; var bEdited : Boolean);
var
   sColor : string;
begin
   with attr do begin
      sColor := Value;
      bEdited := editBGColor(sColor);
      if bEdited then
         Value := sColor;
   end;
end;

procedure THForm.ShowCaptionChanged(attr : THAttribute);
begin
   with _attributes do begin
      if attr.BooleanValue then        // ShowCaption
         begin
            with Attribute['CaptionBGColor'] do
               Options := Options + [aoEditable];
            with Attribute['CaptionColor'] do
               Options := Options + [aoEditable];
         end
      else begin
         with Attribute['CaptionBGColor'] do
            Options := Options - [aoEditable];
         with Attribute['CaptionColor'] do
            Options := Options - [aoEditable];
      end;
   end;
end;

function TWHForm2HTML.getFormBeginTag : string;
begin
   result := '';
   if _bWebHubMode then
      begin
         if not (_WHChunkOutputScope in [cosAll, cosForm]) then  // cosReset only
            exit;
         if _bWHChunkDeclaration then
            result := '<H1>-Chunk:ch' + MainContainer.Name + '</H1>' + sLineBreak;
      end
   else if ChunkOnly then
      result := ''
   else begin
      result := '<HTML>' + sLineBreak + '<HEAD>' + sLineBreak + '<TITLE>';
      result := result + TForm(owner).caption + '</TITLE>' + sLineBreak;
      result := result + '<BODY>' + sLineBreak + sLineBreak;
   end;
   result := result + '<!-- Generated on ' + DateTimeToStr(Now) + ' by TWHForm2HTML,' + sLineBreak;
   result := result + 'the VCL FORM to HTML converter component.' + sLineBreak;
   result := result + 'Author: Philippe Maquet.' + sLineBreak;
   result := result + 'Copyright (c) 1998 HREF Tools Corp.  All Rights Reserved.';
   result := result + ' -->' + sLineBreak;
   if not _bWebHubMode then
      result := result + sLineBreak;
   result := result + _hForm.getBeforeElement + '<FORM' + _hForm.Attributes.HTML + '>';
end;

function TWHForm2HTML.getFormEndTag : string;
var
   clearList : TStringList;
   i : integer;
   bForm, bResetChunk : Boolean;
begin
   bForm := (not _bWebHubMode) or (_WHChunkOutputScope in [cosAll, cosForm]);
   bResetChunk := _bWebHubMode and (_WHChunkOutputScope in [cosAll, cosReset]);
   if bForm then begin
      result := '</FORM>' + sLineBreak;
      result := result + _hForm.getAfterElement;
   end;
   if not (ChunkOnly or _bWebHubMode) then
      result := result + '</BODY>' + sLineBreak + '</HTML>' + sLineBreak
   else if bResetChunk then begin
      if _bWHChunkDeclaration then begin
         if bForm then
            result := result + sLineBreak;
         result := result + '<H1>-Chunk:ch' + MainContainer.Name + 'Reset</H1>' + sLineBreak;
      end;
      clearList := nil;
      try
         clearList := TStringList.create;
         result := result + _hMainContainer.getWHResetToDefaultMacro(clearList);
         with clearList do begin
            if count > 0 then begin
               sort;
               result := result + MacroStart + 'CLEAR|';
               for i := 0 to count - 1 do begin
                  if i > 0 then
                     result := result + ',';
                  result := result + strings[i];
               end;
               result := result + MacroEnd + sLineBreak;
            end;
         end;
      finally
         clearlist.free;
      end;
   end;
end;

function THGroupBox.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   hHeight := THackGroupBox(_control).clientHeight;
   hWidth := THackGroupBox(_control).clientWidth;
   result := true;
end;

function THGroupBox.getHTMLText: string;
var
   hw, hh : integer;
begin
   _form2HTML.getHDimsOfText(self, THackGroupBox(_control).caption, hw, hh);
   inc(hh,2);
   result := '<TABLE' + _form2HTML.BGColorAttr + ' Border="1" Cellspacing="0" Cellpadding="0">' + sLineBreak;
   result := result + '<TR><TD Height="' + intToStr(hh) + '" Valign="middle">' + _form2HTML.getFontAttr(self, '') + '&nbsp;' +
      removeChars(THackGroupBox(_control).caption, '&') + '</TD>' + sLineBreak + '<TR><TD>';
   result := result + inherited getHTMLText;
   result := result + '</TD></TR></TABLE>';
end;

function THRadioGroup.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   hHeight := THackRadioGroup(_control).clientHeight;
   hWidth := THackRadioGroup(_control).clientWidth;
   result := true;
end;

function THRadioGroup.getHTML : string;
begin
   {if assigned(_form2HTML.OnBeforeRenderingControl) then
      _form2HTML.OnBeforeRenderingControl(self);}
   result := getHTMLText;
   {if assigned(_form2HTML.OnAfterRenderingControl) then
      _form2HTML.OnAfterRenderingControl(self);}
end;

function THRadioGroup.itemsHTML : string;
var
   ret : string;
   nr, nc, ni, n, r, c : integer;
   srh, scw : string;
   hw, hh : integer;
   function HTMLOfItem(r, c : integer) : string;
   var
      ret, s : string;
      i : integer;
   begin
      i := (r - 1) + (c - 1) * nr;
      if Form2HTML.WebHubMode then
         begin
            ret := MacroStart + 'INPUTRADIO|' + Attributes['Name'].Value +
               ',' + intToStr(i);
            s := Attributes['ExtraAttributes'].Value;
            if s <> '' then
               ret := ret + '|' + s;
            ret := ret + MacroEnd;
         end
      else begin
         ret := '<INPUT Type="radio"';
         if THackRadioGroup(_control).itemIndex = i then
            ret := ret + ' Checked';
         ret := ret + Attributes.HTML + ' Value="' + THackRadioGroup(_control).items[i] + '">';
      end;
      ret := ret + _form2HTML.getFontAttr(self, '') + THackRadioGroup(_control).items[i];
      result := ret;
   end;
begin
   _form2HTML.getHDimsOfText(self, THackRadioGroup(_control).caption, hw, hh);
   inc(hh,2);
   ret := '<TABLE' + _form2HTML.BGColorAttr + ' Cellspacing="0" Cellpadding="0">' + sLineBreak;
   nc := THackRadioGroup(_control).columns;
   ni := THackRadioGroup(_control).items.count;
   nr := ni div nc;
   if (ni mod nc) <> 0 then
      inc(nr);
   if nr > 0 then begin
      srh := intToStr((THackRadioGroup(_control).clientHeight - hh) div nr); // row height
      scw := intToStr((THackRadioGroup(_control).clientWidth - 4) div nc); // columns width
      n := 0;
      for r := 1 to nr do begin
         if n = ni then break;
         ret := ret + '<TR><TD Height="' + srh + '" Width="' + scw + '">';
         ret := ret + HTMLOfItem(r, 1) + '</TD>';
         inc(n);
         for c := 2 to nc do begin
            if n = ni then break;
            ret := ret + sLineBreak + '<TD Width="' + scw + '">';
            ret := ret + HTMLOfItem(r, c) + '</TD>';
            inc(n);
         end;
         ret := ret + '</TR>' + sLineBreak;
      end;
   end;
   ret := ret + '</TABLE>' + sLineBreak;
   result := ret;
end;

function THRadioGroup.getHTMLText: string;
var
   hw, hh : integer;
begin
   _form2HTML.getHDimsOfText(self, THackRadioGroup(_control).caption, hw, hh);
   inc(hh,2);
   result := '<TABLE' + _form2HTML.BGColorAttr + ' Border="1" Cellspacing="0" Cellpadding="0">' + sLineBreak;
   result := result + '<TR><TD Height="' + intToStr(hh) + '" Valign="middle">' + _form2HTML.getFontAttr(self, '') + '&nbsp;' +
      removeChars(THackGroupBox(_control).caption, '&') + '</TD>' + sLineBreak + '<TR><TD>';
   result := result + itemsHTML;
   result := result + '</TD></TR></TABLE>';
   addBeforeAndAfterElements(result);
end;

function THForm.getHTMLText: string;
var
   bShowCaption, bBorder : Boolean;
   th, tw : integer;
   s : string;
   brf : THBrowserRunFamily;
   function getOuterTableBGColorAttr : string;
   var
      sCaptionBGColor : string;
   begin
      if not bShowCaption then
         result := _form2HTML.BGColorAttr
      else begin
         sCaptionBGColor := _attributes['CaptionBGColor'].Value;
         if sCaptionBGColor = '' then
            result := _form2HTML.BGColorAttr
         else
            result := ' BGColor="' + sCaptionBGColor + '"';
      end;
   end;
begin
   result := '';
   with _attributes do begin
      bShowCaption := Attribute['ShowCaption'].BooleanValue;
      if bShowCaption then
         bBorder := true
      else
         bBorder := Attribute['Border'].BooleanValue;
   end;
   if bBorder then
      result := result + '<TABLE' + getOuterTableBGColorAttr + ' Border="3" Cellspacing="0" Cellpadding="0">' + sLineBreak;
   if bShowCaption then begin
      _form2HTML.getHDimsOfText(self, TForm(_control).caption, tw, th);
      result := result + '<TR><TD Height="' + intToStr(th + 7) + '" VAlign="middle">' +
      _form2HTML.getFontAttr(self, _attributes['CaptionColor'].Value) + '<B>&nbsp;' + TForm(_control).caption +
      '</TD>' + sLineBreak;
   end;
   if bBorder then begin
      result := result + '<TR><TD>' + sLineBreak;
      s := _form2HTML.BGColorAttr;
      if bShowCaption then begin      // Netscape bug (the whole table gets the caption backColor if the
                                    // inner backColor is empty)
         brf := bf2brf(Form2HTML.BrowserFamily);
         if (s = '') and (brf in [brfNS3, brfNS4]) then
            s := ' BGColor="White"';
      end;
      result := result + '<TABLE' + s + ' Cellspacing="0" Cellpadding="0"> <TR><TD>' + sLineBreak;
   end;
   result := result + inherited getHTMLText;
   if bBorder then begin
      result := result + '</TD></TR></TABLE>';
      result := result + '</TD></TR></TABLE>' + sLineBreak;
   end;
end;

constructor THPanel.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Border', atInteger,
         'TWHForm2HTML option. If greater then 0, the panel is rendered with a border around it.');
      with tmpAttr do begin
         options := options - [aoHTML];
         GetDefaultProc := getDefaultBorder;
         ValidateProc := validateBorder;
      end;
      add(tmpAttr);
   end;
end;

procedure THPanel.getDefaultBorder(var defBorder : string);
var
   nBorder : integer;
begin
   with THackPanel(_control) do begin
      nBorder := 0;
      if BevelOuter <> bvNone then
         nBorder := BevelWidth;
      if BevelInner <> bvNone then
         inc(nBorder,BevelWidth);
   end;
   defBorder := intToStr(nBorder);
end;

procedure THPanel.validateBorder(var sBorder : string; var bOK : Boolean);
var
   n : integer;
   s : string;
begin
   if sBorder = '' then
      s := '0'
   else
      s := sBorder;
   try
      n := strToInt(sBorder);
      if (n < 0) or (n > 8) then
         bOK := false;
   except
      bOK := false;
   end;
end;

function THPanel.getHDims(var hWidth, hHeight : integer) : Boolean;
var
   nBorder : integer;
begin
   result := inherited getHDims(hWidth, hHeight);
   nBorder := strToInt(_attributes['Border'].Value);
   inc(hWidth, 2 * nBorder);
   inc(hHeight, 2 * nBorder);
end;

function THPanel.getHTMLText: string;
var
   nBorder : integer;
   brf : THBrowserRunFamily;
begin
   result := '';
   with _attributes do
      nBorder := strToInt(Attribute['Border'].Value);
   if nBorder > 0 then begin
      brf := bf2brf(Form2HTML.BrowserFamily);
      if brf in [brfNS3, brfNS4] then
         inc(nBorder);
      result := result + '<TABLE' + _form2HTML.BGColorAttr + ' Border="' + intToStr(nBorder) + '" Cellspacing="0" Cellpadding="0">' + sLineBreak;
      result := result + '<TR><TD>' + sLineBreak;
   end;
   result := result + inherited getHTMLText;
   if nBorder > 0 then
      result := result + '</TD></TR></TABLE>';
end;

constructor THLabel.create(aControl : TControl);
begin
   inherited create(aControl);
   with _attributes do
      remove('ExtraAttributes');
   addAnchorsAttributes;  // In THControl
end;

function THLabel.getHTML: string;
begin
   {if assigned(_form2HTML.OnBeforeRenderingControl) then
      _form2HTML.OnBeforeRenderingControl(self);}
   result := getHTMLText;
   {if assigned(_form2HTML.OnAfterRenderingControl) then
      _form2HTML.OnAfterRenderingControl(self);}
end;

function THLabel.getHTMLText: string;
var
   sCaption, sFont : string;
begin
   sCaption := removeChars(THackLabel(_control).caption, '&');
   sFont := _form2HTML.getFontAttr(self, '');
   if _attributes['AsAnchor'].BooleanValue then
      begin
         if _form2HTML.WebHubMode then
            result := _attributes['WHAnchorMacro'].DefValue
         else
            result := sFont + '<A Href="' + _attributes['Href'].Value + '">' +
                        sCaption + '</A>';
      end
   else
      result := sFont + sCaption;
end;

function THLabel.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   result := _form2HTML.getHDimsOfText(self, THackLabel(_control).caption, hWidth, hHeight);
end;

function THWinControl.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   if Enabled then
      result := False  // job is to be done by the descendant
   else
      result := _form2HTML.getHDimsOfText(self, getHTMLText, hWidth, hHeight);
end;

function THWinControl.getHTMLText : string;
var
   pBuf : pChar;
begin
   if Enabled then
      begin
         if _Form2HTML.WebHubMode then
            result := inherited getHTML
         else
            result := inherited getHTMLText;
      end
   else begin
      pBuf := nil;
      try
         getMem(pBuf, 512);
         GetWindowText(
            TWinControl(_control).handle,
            pBuf,
            512);
         result := strpas(pBuf);
      finally
         freeMem(pBuf);
      end;
   end;
end;

function THWinControl.getHTML : string;
begin
   if Enabled then
      result := inherited getHTML
   else begin
      {if assigned(_form2HTML.OnBeforeRenderingControl) then
         _form2HTML.OnBeforeRenderingControl(self);}
      result := getBeforeElement + _form2HTML.getFontAttr(self, '') + getHTMLText + getAfterElement;
      {if assigned(_form2HTML.OnAfterRenderingControl) then
         _form2HTML.OnAfterRenderingControl(self);}
   end;
end;

constructor THControl.create(aControl : TControl);
begin
   inherited createHAttrObject;
   _control := aControl;
end;

procedure THControl.addAnchorsAttributes;
var
   tmpAttr : THAttribute;
begin
   with _attributes do begin
      tmpAttr := THAttribute.create('Href', atString,
         'HTML Href attribute, used when AsAnchor is true. Specify the URL or WebHub pageID.');
      with tmpAttr do begin
         ValueChangedProc := asWHAnchorChanged;
         Options := Options - [aoHTML];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('WHAnchorType', atList,
         'TWHForm2HTML option. Based on WHAnchorType, this defines the string to be used when exporting as a WebHub chunk.');
      with tmpAttr do begin
         Options := Options - [aoHTML];
         ValueChangedProc := asWHAnchorChanged;
         with ValuesList do begin
            add('GO');
            add('GOR');
            add('HIDE');
            add('HIDER');
            add('HREF');
            add('JUMP');
            add('JUMPR');
         end;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('WHAnchorMacro', atString,
         'TWHForm2HTML option. Defines the type of WebHub anchor macro to be used.  Double-click to see choices.');
      with tmpAttr do begin
         Options := Options - [aoHTML, aoEditable];
         GetDefaultProc := getDefaultWHAnchorMacro;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('AsAnchor', atBoolean,
         'TWHForm2HTML option. Specifies whether the element will create an HTML anchor or not.');
      with tmpAttr do begin
         ValueChangedProc := asAnchorChanged;
         Value := boolean2String(False);
         DefValue := Value;
         Options := Options - [aoHTML];
      end;
      add(tmpAttr);
   end;
end;

procedure THControl.asAnchorChanged(attr : THAttribute);
begin
   with _attributes do begin
      if attr.BooleanValue then        // AsAnchor
         begin
            with Attribute['Href'] do
               Options := Options + [aoEditable] - [aoIgnore];
            with Attribute['WHAnchorType'] do
               Options := Options + [aoEditable];
         end
      else begin
         with Attribute['Href'] do
            Options := Options - [aoEditable] + [aoIgnore];
         with Attribute['WHAnchorType'] do
            Options := Options - [aoEditable];
      end;
      Attribute['WHAnchorMacro']._bChanged := True;
   end;
end;

procedure THControl.getDefaultWHAnchorMacro(var defWHAnchorMacro : string);
var
   s, s2 : string;
begin
   if not _attributes['AsAnchor'].BooleanValue then
      defWHAnchorMacro := ''
   else begin
      if not (self is THImage) then
         s := _form2HTML.getFontAttr(self, '')
      else
         s := '';
      s := s + MacroStart + _attributes['WHAnchorType'].Value +
         '|' + _attributes['Href'].Value;
      if self is THImage then
         begin
            s := s + '|<IMG ';
            s2 := _attributes['Alt'].Value;
            if s2 <> '' then
               s := s + 'Alt="' + s2 + '" ';
            s := s + 'Height="' + _attributes['Height'].Value + '" Src="' +
               _attributes['Src'].Value + '" Width="' + _attributes['Width'].Value + '">' + MacroEnd;
         end
      else begin
         if self is THLabel then
            s := s + '|' + removeChars(THackLabel(_control).caption, '&') + MacroEnd
         else
            s := s + '|' + removeChars(THackCheckBox(_control).caption, '&') + MacroEnd
      end;
      defWHAnchorMacro := s;
   end;
end;

procedure THControl.asWHAnchorChanged(attr : THAttribute);
begin
   _attributes['WHAnchorMacro']._bChanged := True;
end;

function THControl.getNamePath : string;
begin
   result := getName;
   if _hContainer <> nil then
      result := _hContainer.getNamePath + '_' + result;
end;

function THControl.getName : string;
begin
   result := _control.name;
end;

function THControl.getVisible : Boolean;
begin
   result := _control.Visible;
end;

function THControl.getEnabled : Boolean;
begin
   with _control do
      result := (csAcceptsControls in _control.ControlStyle) or Enabled;
end;

constructor THAttrObject.createHAttrObject;
begin
   inherited create;
   _attributes := THAttributes.create;
end;

destructor THAttrObject.destroy;
begin
   _attributes.free;
   inherited destroy;
end;

procedure THAttrObject.copyAttributesFrom(fromAO : THAttrObject);
var
   i : integer;
begin
   for i := 0 to fromAO.Attributes.count - 1 do
      self.Attributes.AttributeN[i].Value := fromAO.Attributes.AttributeN[i].Value;
end;

procedure THAttrObject.writeData(writer : TWriter);
begin
   writer.WriteString(Name);
   _attributes.writeData(writer);
end;

procedure THAttrObject.setOptionsFilter(optionsFilter : THAttrOptions);
begin
   Attributes.OptionsFilter := optionsFilter;
end;

function THControl.getWHResetToDefaultMacro(clearList : TStringList) : string;
begin
   result := '';
end;

function THEdit.getWHResetToDefaultMacro(clearList : TStringList) : string;
var
   sValue : string;
begin
   sValue := _attributes['Value'].Value;
   if sValue <> '' then
      result := MacroStart + 'SET|' + _attributes['Name'].Value + '=' + sValue + MacroEnd + sLineBreak
   else begin
      result := '';
      clearList.add(_attributes['Name'].Value);
   end;
end;

function THMemo.getWHResetToDefaultMacro(clearList : TStringList) : string;
var
   sValue : string;
begin
   sValue := _attributes['Text'].Value;
   if sValue <> '' then
      result := MacroStart + 'SET|textarea.' + _attributes['Name'].Value + '=' + sValue + MacroEnd + sLineBreak
   else begin
      result := '';
      clearList.add('textarea.' + _attributes['Name'].Value);
   end;
end;

function THCheckBox.getWHResetToDefaultMacro(clearList : TStringList) : string;
var
   bChecked : Boolean;
begin
   bChecked := _attributes['Checked'].BooleanValue;
   if bChecked then
      result := MacroStart + 'CHECK|' + _attributes['Name'].Value + MacroEnd + sLineBreak
   else begin
      result := '';
      clearList.add(_attributes['Name'].Value);
   end;
end;

function THRadio.getWHResetToDefaultMacro(clearList : TStringList) : string;
var
   bChecked : Boolean;
begin
   bChecked := _attributes['Checked'].BooleanValue;
   if bChecked then
      result := MacroStart + 'SET|' + _attributes['Name'].Value + '=' + _attributes['Value'].Value + MacroEnd + sLineBreak
   else
      result := '';
end;

function THList.getWHResetToDefaultMacro(clearList : TStringList) : string;
var
   i : integer;
begin
   i := TCustomComboBox(_control).itemIndex;
   if i = -1 then
      begin
         result := '';
         clearList.add(_attributes['Name'].Value);
      end
   else
      result := MacroStart + 'SET|' + _attributes['Name'].Value + '=' + getItemValue(i) + MacroEnd + sLineBreak;
end;

function THRadioGroup.getWHResetToDefaultMacro(clearList : TStringList) : string;
var
   i : integer;
begin
   i := THackRadioGroup(_control).itemIndex;
   if i = -1 then
      begin
         result := '';
         clearList.add(_attributes['Name'].Value);
      end
   else
      result := MacroStart + 'SET|' + _attributes['Name'].Value + '=' + intToStr(i) + MacroEnd + sLineBreak;
end;

function THContainer.getWHResetToDefaultMacro(clearList : TStringList) : string;
var
   i : integer;
begin
   result := '';
   for i := 0 to count - 1 do
      result := result + Children[i].getWHResetToDefaultMacro(clearList);
end;

procedure THNamed.getDefaultName (var defaultName : string);
begin
   if _form2HTML.QualifiedNames then
      defaultName := getNamePath
   else
      defaultName := _control.name
end;

constructor THNamed.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Name', atString,
         'HTML element''s name. Symbolic name used in transferring the output.');
      tmpAttr.GetDefaultProc := getDefaultName;
      add(tmpAttr);
   end;
end;

function THControl.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   result := false;
   hWidth := 0; hHeight := 0;
end;

function THControl.getHTML : string;
var
   s : string;
begin
   {if assigned(_form2HTML.OnBeforeRenderingControl) then
      _form2HTML.OnBeforeRenderingControl(self);}
   s := getHTMLBeginTag;
   result := s;
   result := result + attributes.HTML;
   if s <> '' then
      result := result + '>';
   result := result + getHTMLText;
   result := result + getHTMLEndTag;
   addBeforeAndAfterElements(result);
   {if assigned(_form2HTML.OnAfterRenderingControl) then
      _form2HTML.OnAfterRenderingControl(self);}
end;

function THControl.getHTMLBeginTag : string;
begin
   result := '';
end;

function THControl.getHTMLEndTag : string;
begin
   result := '';
end;

function THControl.getHTMLText: string;
begin
   result := '';
end;

procedure THControl.setSynchronized(bOnOff : Boolean);
begin
   _bSynch := bOnOff;
end;

constructor THContainer.create(aControl : TControl);
begin
   inherited create(aControl);
   with _attributes do
      remove('ExtraAttributes');
   _children := TList.create;
end;

destructor THContainer.destroy;
var
   i : integer;
begin
   for i := 0 to count - 1 do
      Children[i].free;
   _children.free;
   freePlacer;
   inherited destroy;
end;

function THContainer.FindHObjectByName(const objName : string) : THAttrObject;
var
   i : integer;
   testName : string;
begin
   testName := UpperCase(objName);
   if testName = UpperCase(self.Name) then begin
      result := self;
      exit;
   end;
   result := nil;
   for i := 0 to count - 1 do begin
      if UpperCase(Children[i].Name) = testName then
         begin
            result := Children[i];
            break;
         end
      else if Children[i] is THContainer then begin
         result := THContainer(Children[i]).FindHObjectByName(objName);
         if result <> nil then
            break;
      end;
   end;
end;

procedure THContainer.setOptionsFilter(optionsFilter : THAttrOptions);
var
   i : integer;
begin
   inherited setOptionsFilter(optionsFilter);
   for i := 0 to count - 1 do
      Children[i].OptionsFilter := optionsFilter;
end;

procedure THContainer.createPlacer;
var
   i : integer;
begin
   if _hPlacer <> nil then exit;  // already created
   with _control do
      _hPlacer := THPlacer.create(self, clientWidth, clientHeight, 4, 4);
   with _hPlacer do begin
      for i := 0 to count - 1 do
         placeControl(Children[i]);
   end;
end;

procedure THContainer.freePlacer;
begin
   _hPlacer.free;
   _hPlacer := nil;
end;

function THContainer.getHTMLText: string;
begin
   createPlacer;
   result := _hPlacer.HTML;
end;

function THContainer.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   result := true;
   createPlacer;
   _hPlacer.getHDims(hWidth, hHeight);
end;

function THContainer.getChild(index : integer) : THControl;
begin
   result := THControl(_children[index]);
end;

procedure THContainer.setChild(index : integer; anHControl : THControl);
begin
   _children[index] := anHControl;
end;

function THContainer.getCount : integer;
begin
   result := _children.count;
end;

procedure THContainer.setSynchronized(bOnOff : Boolean);
var
   i : integer;
begin
   inherited setSynchronized(bOnOff);
   if not bOnOff then begin
      for i := 0 to count - 1 do
         Children[i].synchronized := false;
   end;
end;

procedure THContainer.synchronize;
var
   i,j : integer;
   ctl, testCtl : TControl;
   hControl : THControl;
   bIn : Boolean;
   hCtlClass : THControlClass;
   winContainer : TWinControl;
begin
   freePlacer;
   winContainer := TWinControl(_control);
   synchronized := True;
   with winContainer do begin
      for i := 0 to controlCount - 1 do begin
         ctl := controls[i];
         hCtlClass := Form2HTML.getHControlClass(ctl);
         if hCtlClass <> nil then begin  // ctl's type is supported
            bIn := false;
            hControl := nil; // just to avoid a compile warning !
            with self do begin
               for j := 0 to count - 1 do begin
                  hControl := Children[j];
                  testCtl := hControl.control;
                  if (ctl = testCtl) and (ctl.classType = testCtl.classType) then begin
                     bIn := true;
                     break;
                  end;
               end;
               if not bIn then
                  hControl := createNewChild(ctl);
               with hControl do begin
                  synchronized := True;
                  _hContainer := self;
               end;
            end;
            if hControl is THContainer then
               THContainer(hControl).synchronize;
         end;
      end;
   end;
end;

procedure THContainer.synchronizeDone;
var
   i : integer;
   hControl : THControl;
   bPack : Boolean;
begin
   bPack := false;
   for i := 0 to count - 1 do begin
      hControl := Children[i];
      if not hControl.synchronized then
         begin
            hControl.free;
            Children[i] := nil;
            bPack := true;
         end
      else if hControl is THContainer then
         THContainer(hControl).synchronizeDone;
   end;
   if bPack then
      _children.pack;
end;

class function TWHForm2HTML.getHContainerClass(fromCtl : TControl) : THControlClass;
begin
   result := nil;
   if not (csAcceptsControls in fromCtl.ControlStyle) then exit;
   if fromCtl is TForm then // TCustomForm doesn't exist in Delphi2
      result := THForm
   else if (fromCtl is TCustomGroupBox) and (not (fromCtl is TCustomRadioGroup)) then
      result := THGroupBox
   else if fromCtl is TCustomPanel then
      result := THPanel
   else
      result := THContainer;
end;

function TWHForm2HTML.getHControlClass(fromCtl : TControl) : THControlClass;
begin
   result := getHContainerClass(fromCtl);
   if result <> nil then exit;
   if fromCtl is TCustomLabel then
      result := THLabel
   else if fromCtl is TCustomMemo then // must stay *before* TCustomEdit !
      result := THMemo
   else if fromCtl is TCustomEdit then
      result := THEdit
   else if fromCtl is TCustomCheckBox then
      result := THCheckBox
   else if fromCtl is TRadioButton then
      result := THRadio
   else if fromCtl is TCustomComboBox then
      result := THComboBox
   else if fromCtl is TCustomListBox then
      result := THListBox
   else if fromCtl is TButton then
      result := THButton
   else if fromCtl is TImage then
      result := THImage
   else if fromCtl is TCustomRadioGroup then
      result := THRadioGroup
   else
      result := nil;
end;

function THContainer.createNewChild(fromCtl : TControl) : THControl;
var
   hCtlClass : THControlClass;
begin
   hCtlClass := Form2HTML.getHControlClass(fromCtl);
   if hCtlClass <> nil then
      begin
         result := hCtlClass.create(fromCtl);
         result.Form2HTML := _form2HTML;
         _children.add(result);
      end
   else
      result := nil;
end;

constructor THAttribute.create(const aName : string; aType : THAttrType; const aHint : string);
begin
   inherited create;
   _bIsDefault := true;
   _name := aName;
   _hint := aHint;
   _attrType := aType;
   _options := [aoEditable, aoHTML];
   case _attrType of
      atBoolean:
         _defValue := boolean2String(False);
      atList:
         _valuesList := TStringList.create;
   end;
end;

destructor THAttribute.destroy;
begin
   _valuesList.free;
   inherited destroy;
end;

procedure THAttribute.setOptions(options : THAttrOptions);
begin
   _options := options;
   _bChanged := True;
end;

procedure THAttribute.setValue(const aValue : string);
begin
   _value := aValue;
   _bIsDefault := (_value = DefValue);
   _bChanged := True;
   if assigned(_valueChangedProc) then
      _valueChangedProc(self);
end;

procedure THAttribute.setDefValue(const aValue : string);
begin
   _defValue := aValue;
   _bIsDefault := (_defValue = _value);
end;

function THAttribute.getValue : string;
begin
   if _bIsDefault then
      result := DefValue
   else
      result := _value;
end;

function THAttribute.getBooleanValue : Boolean;
begin
   result := string2Boolean(Value);
end;

procedure THAttribute.setBooleanValue(b : Boolean);
begin
   Value := boolean2String(b);
end;

procedure THAttribute.restore;
begin
   _value := _savedValue;
end;

procedure THAttribute.save;
begin
   _savedValue := _value;
   _bChanged := False;
end;

function THAttribute.getBChanged : Boolean;
begin
   result := _bChanged;
   _bChanged := False;
end;

function THAttribute.getDefValue : string;
begin
   result := _defValue;
   if (result = '') and (_attrType = atList) then begin
      if _valuesList.count > 0 then
         result := _valuesList.strings[0];
   end;
   if assigned(_getDefaultProc) then
      _getDefaultProc(result);
end;

function THAttribute.getNextValue : string;
var
   i : integer;
begin
   result := '';
   if _attrType = atList then begin
      with _valuesList do begin
         if count = 0 then
            exit;
         i := indexOf(Value);
         if (i = count - 1) then
            result := strings[0]
         else
            result := strings[i+1];
      end;
   end;
end;


class function THAttribute.boolean2String(b : Boolean) : string;
begin
   if b then
      result := 'True'
   else
      result := 'False';
end;

class function THAttribute.string2Boolean(const s : string) : Boolean;
begin
   result := (s = 'True')
end;

function THAttribute.getHTML : string;
begin
   result := '';
   if (aoHTML in _options) then begin
      if _attrType = atBoolean then
         begin
            if BooleanValue then
               result := ' ' + _name;
         end
      else if Value <> '' then begin
         if (aoNoName in _options) then
            result := ' ' + Value   // Used by ExtraAttributes
         else
            result := ' ' + _name + '="' + Value + '"';
      end;
   end;
end;

function THAttribute.isValid(var aValue : string) : Boolean;
begin
   if _attrType = atList then
      result := (_valuesList.IndexOf(aValue) <> -1)
   else
      result := true;
   if result and assigned(_validateProc) then
      _validateProc(aValue, result);
end;

destructor THAttributes.destroy;
var
   i : integer;
begin
   if _attributes <> nil then begin
      for i := 0 to count - 1 do
         AttributeN[i].free;
   end;
   _attributes.Free;
   inherited destroy;
end;

procedure THAttributes.save;
var
   i : integer;
begin
   for i := 0 to count -1 do
      AttributeN[i].save;
end;

procedure THAttrObject.setWebHubMode(b : Boolean);
var
   i : integer;
   stdAttr, attr : THAttribute;
begin
   with _attributes do begin
      for i := 0 to count - 1 do begin
         attr := AttributeN[i];
         with attr do begin
            if aoWebHub in Options then begin
               stdAttr := Attribute[StdAttrName];
               if b then
                  begin
                     stdAttr.save;
                     stdAttr.Value := Value;
                  end
               else
                  stdAttr.restore;
            end;
         end;
      end;
   end;
end;

procedure THContainer.setWebHubMode(b : Boolean);
var
   i : integer;
begin
   inherited setWebHubMode(b);
   for i := 0 to count - 1 do
      Children[i].WebHubMode := b;
end;

procedure THAttributes.restore;
var
   i : integer;
begin
   for i := 0 to count -1 do
      AttributeN[i].restore;
end;

procedure THAttributes.add(anAttr : THAttribute);
var
   i : integer;
   insertPos : integer;
begin
   if _attributes = nil then
      _attributes := TList.create;
   insertPos := -1;
   for i := 0 to count - 1 do begin
      if AttributeN[i].name >= anAttr.name then begin
         insertPos := i;
         break;
      end;
   end;
   if insertPos = -1 then
      _attributes.add(anAttr)
   else
      _attributes.insert(insertPos, anAttr);
end;

procedure THAttributes.remove(const attrName : string);
var
   attr : THAttribute;
begin
   attr := Attribute[attrName];
   if attr <> nil then begin
      _attributes.remove(attr);
      attr.free;
   end;
end;

function THAttributes.getAttributeN(index : integer) : THAttribute;
var
   i, n : integer;
   testAttr : THAttribute;
begin
   if count = 0 then
      result := nil
   else if _optionsFilter = [] then
      result := THAttribute(_attributes.items[index])
   else begin
      result := nil;
      n := -1;
      with _attributes do begin
         for i := 0 to count - 1 do begin
            testAttr := THAttribute(items[i]);
            if (testAttr.Options * _optionsFilter) <> [] then begin
               inc(n);
               if n = index then begin
                  result := testAttr;
                  exit;
               end;
            end;
         end;
      end;
   end;
end;

function THAttributes.getAttribute(const attrName : string) : THAttribute;
var
   i : integer;
   s1, s2 : string;
begin
   s1 := UpperCase(attrName);
   for i := 0 to count - 1 do begin
      result := AttributeN[i];
      s2 := UpperCase(result.Name);
      if s2 = s1 then
         exit
      else if s2 > s1 then
         break;
   end;
   result := nil;
end;

function THAttributes.getCount : integer;
var
   i : integer;
begin
   if _attributes = nil then
      result := 0
   else if _optionsFilter = [] then
      result := _attributes.count
   else begin
      result := 0;
      with _attributes do begin
         for i := 0 to count - 1 do begin
            if (THAttribute(items[i]).Options * _optionsFilter) <> [] then
               inc(result);
         end;
      end;
   end;
end;

function THAttributes.getHTML : string;
var
   i : integer;
   attr : THAttribute;
begin
   result := '';
   if _attributes <> nil then begin
      for i := 0 to count - 1 do begin
         attr := AttributeN[i];
         if not (aoIgnore in attr.Options) then
            result := result + attr.HTML;
      end;
   end;
end;

constructor THButton.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Type', atList,
         'HTML <INPUT> Type. If AsImage is true, use "image".  Otherwise, use "submit" or "reset".');
      with tmpAttr do
         ValidateProc := validateType;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Value', atString,
         'HTML <INPUT> Value. Specifies the button''s caption.');
      with tmpAttr do
         GetDefaultProc := getDefaultValue;
      add(tmpAttr);
      add(THAttribute.create('Src', atString,
         'HTML <INPUT> Src. Defines the source file for the image when the attribute Type is set to "image".'));
      tmpAttr := THAttribute.create('Alt', atString,
         'HTML Alt tag. This is defaulted based on the Hint property.');
      with tmpAttr do
         GetDefaultProc := getDefaultAlt;
      add(tmpAttr);
      tmpAttr := THAttribute.create('AsImage', atBoolean,  // must stay the last one !
         'TWHForm2HTML option. Specifies whether the button is to be rendered as an image.');
      with tmpAttr do begin
         ValueChangedProc := asImageChanged;
         Value := boolean2String(False);
         DefValue := Value;
         Options := Options - [aoHTML];
      end;
      add(tmpAttr);
   end;
end;

procedure THButton.getDefaultAlt(var defAlt : string);
begin
   defAlt := TButton(_control).hint;
end;

procedure THButton.asImageChanged(attr : THAttribute);
begin
   with _attributes do begin
      if attr.BooleanValue then        // AsImage
         begin
            with Attribute['Type'] do begin
               DefValue := 'image';
               Options := Options - [aoEditable];
               with ValuesList do begin
                  clear;
                  add('image');
               end;
            end;
            with Attribute['Src'] do
               Options := Options + [aoEditable] - [aoIgnore];
            with Attribute['Alt'] do
               Options := Options + [aoEditable] - [aoIgnore];
            with Attribute['Value'] do
               Options := Options - [aoEditable] + [aoIgnore];
         end
      else begin
         with Attribute['Type'] do begin
            DefValue := 'submit';
            Options := Options + [aoEditable];
            with ValuesList do begin
               clear;
               add('submit');
               add('reset');
            end;
         end;
         with Attribute['Src'] do
            Options := Options - [aoEditable] + [aoIgnore];
         with Attribute['Alt'] do
            Options := Options - [aoEditable] + [aoIgnore];
         with Attribute['Value'] do
            Options := Options + [aoEditable] - [aoIgnore];
      end;
      with Attribute['Type'] do
         Value := DefValue;
   end;
end;

procedure THButton.getDefaultValue(var defValue : string);
begin
   defValue := removeChars(TButton(_control).Caption, '&');
end;

procedure THButton.validateType(var sType : string; var bOK : Boolean);
var
   s : string;
begin
   s := upperCase(sType);
   if _attributes['AsImage'].booleanValue then
      bOK := (s = 'IMAGE')
   else
      bOK := (s = 'SUBMIT') or (s = 'RESET');
end;

function THButton.getPureCaption : string;
begin
   result := removeChars(Attributes['Value'].Value, '&');
   if result = '' then
      result := removeChars(Attributes['Value'].defValue, '&');
end;

function THButton.getHWidth(const sCaption : string) : integer;
var
   brf : THBrowserRunFamily;
   tmpDouble : double;
   w : integer;
begin
   brf := bf2brf(Form2HTML.BrowserFamily);
   with _form2HTML._bmp.canvas do begin
      with font do begin
         name := 'Times New Roman';
         size := BUT_FNT_MOD_SIZE[brf];
         style := [];
      end;
      w := textWidth(sCaption);
      if w < BUT_MIN_TW[brf] then
         w := BUT_MIN_TW[brf];
      tmpDouble := w;
      tmpDouble := tmpDouble * BUT_ADD_WPCT[brf];
      inc(w, round(tmpDouble));
      inc(w, BUT_ADD_W[brf]);
      result := w;
   end;
end;

function THButton.getHTML : string;
var
   s, s1, s2 : string;
   valueAttr : THAttribute;
   n : integer;
begin
   if Attributes['AsImage'].booleanValue then
      begin
         if not Form2HTML.WebHubMode then
            result := inherited getHTML
         else begin
            {if assigned(_form2HTML.OnBeforeRenderingControl) then
               _form2HTML.OnBeforeRenderingControl(self);}
            result := MacroStart + 'INPUTIMAGE|' + Attributes['Name'].Value +
               ',' + Attributes['Src'].Value;
            s1 := Attributes['Alt'].Value;
            s2 := Attributes['ExtraAttributes'].Value;
            if (s1 <> '') or (s2 <> '') then
               result := result + '|';
            if (s1 <> '') then
               result := result + 'Alt="' + s1 + '"';
            if (s2 <> '') then begin
               if (s1 <> '') then
                  result := result + ' ';
               result := result + s2;
            end;
            result := result + MacroEnd;
            addBeforeAndAfterElements(result);
            {if assigned(_form2HTML.OnAfterRenderingControl) then
               _form2HTML.OnAfterRenderingControl(self);}
         end;
      end
   else begin
      s := getPureCaption;
      n := 0;
      while getHWidth(s) < _control.Width do begin
         if (n mod 2) = 0 then
            s := ' ' + s
         else
            s := s + ' ';
         inc(n);
      end;
      valueAttr := Attributes['Value'];
      with valueAttr do begin
         save;  // Save attribute's Value
         Value := s;
         if not Form2HTML.WebHubMode then
            result := inherited getHTML
         else begin
            {if assigned(_form2HTML.OnBeforeRenderingControl) then
               _form2HTML.OnBeforeRenderingControl(self);}
            if Attributes['Type'].Value = 'submit' then
               result := MacroStart + 'INPUTSUBMIT|'
            else
               result := MacroStart + 'INPUTRESET|';
            result := result + Attributes['Name'].Value +
               ',' + Attributes['Value'].Value;
            s := Attributes['ExtraAttributes'].Value;
            if s <> '' then
               result := result + '|' + s;
            result := result + MacroEnd;
            addBeforeAndAfterElements(result);
            {if assigned(_form2HTML.OnAfterRenderingControl) then
               _form2HTML.OnAfterRenderingControl(self);}
         end;
         restore;    // restore attribute's Value
      end;
   end;
end;

function THButton.getHDims(var hWidth, hHeight : integer) : Boolean;
var
   brf : THBrowserRunFamily;
begin
   result := true;
   if inherited getHDims(hWidth, hHeight) then
      exit;
   with TButton(_control) do begin
      if Attributes['AsImage'].booleanValue then
         begin
            hWidth := Width;
            hHeight := Height;
         end
      else begin
         brf := bf2brf(Form2HTML.BrowserFamily);
         hHeight := BUT_HEIGHT[brf];
         hWidth := getHWidth(getPureCaption);
         if hWidth < Width then
            hWidth := Width;
      end;
   end;
end;

constructor THImage.create(aControl : TControl);
var
   tmpAttr : THAttribute;
begin
   inherited create(aControl);
   with _attributes do begin
      tmpAttr := THAttribute.create('Type', atString,
         'HTML <INPUT> Type. Only used if AsButton is False.');
      with tmpAttr do begin
         Value := 'image';
         DefValue := Value;
         Options := Options - [aoEditable];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Alt', atString,
         'HTML Alt Value. Specifies the alternate string for the image.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultAlt;
         ValueChangedProc := asWHAnchorChanged;
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Height', atInteger,
         'HTML Height attribute. Sets the height of the image in pixels.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultHeight;
         Options := Options - [aoEditable];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Width', atInteger,
         'HTML Width attribute. Sets the width of the image in pixels.');
      with tmpAttr do begin
         GetDefaultProc := getDefaultWidth;
         Options := Options - [aoEditable];
      end;
      add(tmpAttr);
      tmpAttr := THAttribute.create('Src', atString,
         'HTML <INPUT> or <IMG> Src. Defines the source file for the image.');
      with tmpAttr do
         ValueChangedProc := asWHAnchorChanged;
      add(tmpAttr);
      addAnchorsAttributes;  // In THControl
      tmpAttr := THAttribute.create('AsButton', atBoolean,   // must remain the last one
         'TWHForm2HTML option. Specifies whether the image will act as a button (<INPUT Type="image">).');
      with tmpAttr do begin
         ValueChangedProc := asButtonChanged;
         Value := boolean2String(True);
         DefValue := Value;
         Options := Options - [aoHTML];
      end;
      add(tmpAttr);
   end;
end;

procedure THImage.getDefaultAlt(var defAlt : string);
begin
   defAlt := TImage(_control).hint;
end;

procedure THImage.getDefaultHeight(var defHeight : string);
begin
   defHeight := intToStr(TImage(_control).height);
end;

procedure THImage.getDefaultWidth(var defWidth : string);
begin
   defWidth := intToStr(TImage(_control).Width);
end;

procedure THImage.asButtonChanged(attr : THAttribute);
begin
   with _attributes do begin
      if attr.BooleanValue then        // AsButton
         begin
            with Attribute['Type'] do
               Options := Options - [aoIgnore];
            with Attribute['AsAnchor'] do begin
               Options := Options - [aoEditable];
               Value := boolean2String(False);
            end;
            with Attribute['Name'] do
               Options := Options - [aoIgnore] + [aoEditable];
         end
      else begin
         with Attribute['Type'] do
            Options := Options + [aoIgnore];
         with Attribute['AsAnchor'] do begin
            Options := Options + [aoEditable];
            Value := Value;
         end;
         with Attribute['Name'] do
            Options := Options + [aoIgnore] - [aoEditable];
      end;
   end;
end;

function THImage.getHTMLBeginTag : string;
begin
   if _attributes['AsButton'].BooleanValue then
      result := '<INPUT'
   else begin
      if _attributes['AsAnchor'].BooleanValue then
         begin
            with Attributes['Href'] do begin
               Options := Options + [aoHTML];
               result := '<A' + HTML + '><IMG';
               Options := Options - [aoHTML];
            end;
         end
      else
         result := '<IMG';
   end;
end;

function THImage.getHTMLEndTag : string;
begin
   if _attributes['AsButton'].BooleanValue then
      result := ''
   else begin
      if _attributes['AsAnchor'].BooleanValue then
         begin
            result := '</A>';
            Attributes['Href'].Options := Attributes['Href'].Options - [aoIgnore];
         end
      else
         result := '';
   end;
end;

function THImage.getHTML : string;
var
   s1, s2 : string;
begin
   if _attributes['AsAnchor'].BooleanValue then
      begin
         if not Form2HTML.WebHubMode then
            result := inherited getHTML
         else begin
            {if assigned(_form2HTML.OnBeforeRenderingControl) then
               _form2HTML.OnBeforeRenderingControl(self);}
            result := _attributes['WHAnchorMacro'].DefValue;
            addBeforeAndAfterElements(result);
            {if assigned(_form2HTML.OnAfterRenderingControl) then
               _form2HTML.OnAfterRenderingControl(self);}
         end;
      end
   else if not _attributes['AsButton'].BooleanValue then
      result := inherited getHTML
   else begin
      if not Form2HTML.WebHubMode then
         result := inherited getHTML
      else begin
         {if assigned(_form2HTML.OnBeforeRenderingControl) then
            _form2HTML.OnBeforeRenderingControl(self);}
         result := MacroStart + 'INPUTIMAGE|' + Attributes['Name'].Value +
            ',' + Attributes['Src'].Value;
         s1 := Attributes['Alt'].Value;
         s2 := Attributes['ExtraAttributes'].Value;
         if (s1 <> '') or (s2 <> '') then
            result := result + '|';
         if (s1 <> '') then
            result := result + 'Alt="' + s1 + '"';
         if (s2 <> '') then begin
            if (s1 <> '') then
               result := result + ' ';
            result := result + s2;
         end;
         result := result + MacroEnd;
         addBeforeAndAfterElements(result);
         {if assigned(_form2HTML.OnAfterRenderingControl) then
            _form2HTML.OnAfterRenderingControl(self);}
      end;
   end;
end;

function THImage.getHDims(var hWidth, hHeight : integer) : Boolean;
begin
   result := True;
   hWidth := _control.Width;
   hHeight := _control.Height;
end;

end.

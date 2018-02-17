unit f2hf;

{*******************************************************}
{                                                       }
{       Form to HTML converter "F2H"                    }
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

{$I hrefdefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Grids, 
  tpIntegerTypes,
  f2h;

type
  NaturalNumber = 1..High(Integer);

type
  TSplitterD2 = class(TGraphicControl) // for Delphi2 compatibility
  private
    FLineDC: HDC;
    FDownPos: TPoint;
    FSplit: Integer;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FControl: TControl;
    FNewSize: Integer;
    FActiveControl: TWinControl;
    FOldKeyDown: TKeyEvent;
    FBeveled: Boolean;
    FLineVisible: Boolean;
    FOnMoved: TNotifyEvent;
    procedure AllocateLineDC;
    procedure DrawLine;
    procedure ReleaseLineDC;
    procedure UpdateSize(X, Y: Integer);
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetBeveled(Value: Boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure StopSizing;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align default alLeft;
    property Beveled: Boolean read FBeveled write SetBeveled default True;
    property Color;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ParentColor;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  end;

   Tf2hFrm = class(TForm)
      panLeft: TPanel;
      panRight: TPanel;
      phCtls: TPanel;
      phAttrs: TPanel;
      panBottom: TPanel;
    grid: TDrawGrid;
      treeCtls: TTreeView;
      butOK: TButton;
      butCancel: TButton;
      butExport: TButton;
      butBrowse: TButton;
      labCtls: TLabel;
      labAttrs: TLabel;
    butDefault: TButton;
    hintBar: TPanel;
    chkWH: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure treeCtlsChange(Sender: TObject; Node: TTreeNode);
    procedure gridDrawCell(Sender: TObject; Col, Row: Int32; Rect: TRect;
      State: TGridDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure butOKClick(Sender: TObject);
    procedure gridEnter(Sender: TObject);
    procedure gridExit(Sender: TObject);
    procedure gridDblClick(Sender: TObject);
    procedure gridKeyPress(Sender: TObject; var Key: Char);
    procedure butExportClick(Sender: TObject);
    procedure butBrowseClick(Sender: TObject);
    procedure butDefaultClick(Sender: TObject);
    procedure gridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure gridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure gridGetEditText(Sender: TObject; ACol, ARow: Int32;
      var Value: string);
    procedure gridSetEditText(Sender: TObject; ACol, ARow: Int32;
      const Value: string);
    procedure gridSelectCell(Sender: TObject; Col, Row: Int32;
      var CanSelect: Boolean);
   private
      _bConfirmed : Boolean;
      _editAttr : THAttribute;
      _form2HTML : TWHForm2HTML;
      _lastHR : integer;
      split : TSplitterD2;
      procedure gridResized;
      procedure invalidateChangedAttributes;
      procedure invalidateGridCell(col, row : integer);
      procedure restoreFormPosAndSize;
      procedure saveFormPosAndSize;
      procedure setAttrToDefault;
      procedure setGridEditMode(row : integer);
      procedure seTWHForm2HTML(Form2HTML : TWHForm2HTML);
      procedure splitMoved(Sender: TObject);
      procedure treeAddhAttrObj(parentNode : TTreeNode; hAttrObj : THAttrObject);
      procedure WMGetMinMaxInfo(var msg : TWmGetMinMaxInfo); message WM_GETMINMAXINFO;
   public
      property Form2HTML : TWHForm2HTML read _form2HTML write seTWHForm2HTML;
   end;

var
  f2hFrm: Tf2hFrm;

implementation

{$R *.DFM}

uses
   registry;

const
   CREGINI_PATH = 'Software\HREFTools\Form2HTML';
   INI_SECTION = 'Editor';

procedure Tf2hFrm.WMGetMinMaxInfo(var msg : TWmGetMinMaxInfo);
begin
   with msg.MinMaxInfo^.ptMinTrackSize do begin
      X := 568;
      Y := 245;
   end;
   inherited;
end;

procedure Tf2hFrm.treeAddhAttrObj(parentNode : TTreeNode; hAttrObj : THAttrObject);
var
   newParentNode : TTreeNode;
   i : integer;
begin
   if not (hAttrObj is THContainer) then begin
      if hAttrObj.Attributes.count = 0 then
         exit;
   end;
   hAttrObj.Attributes.save;
   with treeCtls.items do begin
      newParentNode := addChildObject(parentNode, hAttrObj.Name, hAttrObj);
      if hAttrObj is THContainer then begin
         with THContainer(hAttrObj) do begin
            for i := 0 to count - 1 do
               treeAddhAttrObj(newParentNode, Children[i]);
         end;
      end;
   end;
   treeCtls.AlphaSort;
end;

procedure Tf2hFrm.seTWHForm2HTML(Form2HTML : TWHForm2HTML);
begin
   _form2HTML := Form2HTML;
   treeAddhAttrObj(nil, _form2HTML.HForm);
   treeAddhAttrObj(nil, _form2HTML.HMainContainer);
   treeCtls.FullExpand;
end;

procedure Tf2hFrm.gridResized;
begin
   with grid do
      colWidths[1] := clientWidth - colWidths[0] - 1;
end;

procedure Tf2hFrm.FormResize(Sender: TObject);
begin
   gridResized;
end;

procedure Tf2hFrm.splitMoved(Sender: TObject);
begin
   gridResized;
end;

{procedure Tf2hFrm.splitMoved(Sender: TObject);
begin
   gridResized;
end;}

procedure Tf2hFrm.treeCtlsChange(Sender: TObject; Node: TTreeNode);
var
   hAttrObj : THAttrObject;
   nAttributes : integer;
begin
   hAttrObj := THAttrObject(treeCtls.selected.data);
   nAttributes := hAttrObj.Attributes.count;
   if nAttributes > 0 then
      begin
         with grid do begin
            rowCount := nAttributes + 1;
            phAttrs.visible := true;
            if visible then
               refresh
            else
               visible := true;
         end;
         setGridEditMode(1);
      end
   else begin
      phAttrs.visible := false;
      grid.visible := false;
   end;
end;

procedure Tf2hFrm.invalidateGridCell(col, row : integer);
var
   r : TRect;
begin
   // Because TCustomgrid.invalidateCell is not accessible from
   // TDrawGrid !
   r := grid.cellRect(col, row);
   Windows.InvalidateRect(grid.handle, @r, False);
end;

procedure Tf2hFrm.gridDrawCell(Sender: TObject; Col, Row: Int32;
  Rect: TRect; State: TGridDrawState);
var
   s : string;
   hAttrObj : THAttrObject;
   attr : THAttribute;
begin
   s := '';
   with grid.canvas do begin
      with font do begin
         if col = 0 then
            style := style + [fsBold]
         else
            style := style - [fsBold];
      end;
      if row = 0 then
         begin
            if col = 0 then
               s := 'Name'
            else
               s := 'Value';
            font.color := clBtnText;

         end
      else begin
         hAttrObj := treeCtls.selected.data;
         with hAttrObj.Attributes do begin
            attr := AttributeN[row-1];
            with font do begin
               if aoEditable in attr.options then
                  begin
                     if (gdSelected in state) and (not (gdFocused in state)) then
                        color := clHighlightText
                     else
                        color := clWindowText;
                  end
               else
                  color := clGrayText;
            end;
            with attr do begin
               if col = 0 then
                  s := Name
               else
                  s := Value;
            end;
         end;
      end;
      with rect do
         TextOut(left + 2, top + 2, s);
   end;
end;

procedure Tf2hFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
   i : integer;
begin
   if not _bConfirmed then begin
      with treeCtls.Items do begin
         for i := 0 to count - 1 do
            THAttrObject(item[i].data).attributes.restore;
      end;
   end;
   saveFormPosAndSize;
end;

procedure Tf2hFrm.setGridEditMode(row : integer);
var
   attr : THAttribute;
begin
   if treeCtls.selected <> nil then begin
      attr := THAttrObject(treeCtls.selected.data).attributes.attributeN[row - 1];
      with grid do begin
         if not (aoEditable in attr.options) then
            options := options - [goEditing]
         else begin
            if attr.attrType = atBoolean then
               options := options - [goEditing]
            else
               options := options + [goEditing];
         end;
      end;
   end;
end;

procedure Tf2hFrm.gridSelectCell(Sender: TObject; Col, Row: Int32;
  var CanSelect: Boolean);
begin
   if col = 0 then
      begin
         CanSelect := false;
         grid.col := 1;
         grid.row := row;
      end
   else
      setGridEditMode(row);
end;

(*procedure Tf2hFrm.gridSelectCell(Sender: TObject; Col, Row: Integer;
  var CanSelect: Boolean);
begin
   if col = 0 then
      begin
         CanSelect := false;
         grid.col := 1;
         grid.row := row;
      end
   else
      setGridEditMode(row);
end;*)

procedure Tf2hFrm.restoreFormPosAndSize;
var
   regIni : TRegIniFile;
   l, t, w, h : integer;
   r : TRect;
   procedure putRectInScreen;
   begin
      with r do begin
         if left < 0 then
            offsetRect(r, -left, 0);
         if (right > screen.width) then
            offsetRect(r, -(right-screen.width), 0);
         if top < 0 then
            offsetRect(r, 0, -top);
         if (bottom > screen.height) then
            offsetRect(r, 0, -(bottom-screen.height));
      end;
   end;
begin
   regIni := nil;
   try
      regIni := TRegIniFile.create(CREGINI_PATH);
      with regIni do begin
         w := ReadInteger(INI_SECTION, 'Width', 620);
         h := ReadInteger(INI_SECTION, 'Height', 440);
         l := ReadInteger(INI_SECTION, 'Left', (screen.Width - w) div 2);
         t := ReadInteger(INI_SECTION, 'Top', (screen.Height - h) div 2);
      end;
      r := Rect(l, t, l + w, t + h);
      putRectInScreen;
      with r do
         SetBounds(left, top, right - left, bottom - top);
   finally
      regIni.free;
   end;
end;

procedure Tf2hFrm.saveFormPosAndSize;
var
   regIni : TRegIniFile;
begin
   regIni := nil;
   try
      regIni := TRegIniFile.create(CREGINI_PATH);
      with regIni do begin
         WriteInteger(INI_SECTION, 'Width', Width);
         WriteInteger(INI_SECTION, 'Height', Height);
         WriteInteger(INI_SECTION, 'Left', Left);
         WriteInteger(INI_SECTION, 'Top', Top);
      end;
   finally
      regIni.free;
   end;
end;

procedure Tf2hFrm.FormShow(Sender: TObject);
begin
   grid.col := 1;
   restoreFormPosAndSize;
end;

procedure Tf2hFrm.butOKClick(Sender: TObject);
begin
   _bConfirmed := true;
   modalResult := mrOK;
end;

procedure Tf2hFrm.gridEnter(Sender: TObject);
begin
   butOK.default := false;
   setGridEditMode(grid.row);
end;

procedure Tf2hFrm.gridExit(Sender: TObject);
begin
   butOK.default := true;
end;

procedure Tf2hFrm.invalidateChangedAttributes;
var
   i : integer;
   hAttrObj : THAttrObject;
begin
   with grid do begin
      hAttrObj := THAttrObject(treeCtls.selected.data);
      with hAttrObj do begin
         for i := 0 to Attributes.count - 1 do begin
            if Attributes.AttributeN[i].Changed then begin
               invalidateGridCell(0, i + 1);
               invalidateGridCell(1, i + 1);
            end;
         end;
      end;
   end;
end;

procedure Tf2hFrm.gridDblClick(Sender: TObject);
var
   bEdited : Boolean;
begin
   with grid do begin
      _editAttr := THAttrObject(treeCtls.selected.data).attributes.attributeN[row - 1];
      with _editAttr do begin
         if (aoEditable in Options) then begin
            if (aoDialog in Options) then
               begin
                  if assigned(DialogProc) then
                     begin
                        EditorMode := False;
                        DialogProc(_editAttr, bEdited);
                     end
                  else
                     bEdited := false;
               end
            else begin
               bEdited := True;
               case attrType of
                  atBoolean:
                     BooleanValue := not BooleanValue;
                  atList:
                     begin
                        EditorMode := False;
                        Value := NextValue;
                     end;
                  else
                     bEdited := False;
               end;
            end;
            if bEdited then begin
               if _editAttr.Changed then
                  invalidateGridCell(col, row);
               invalidateChangedAttributes;
            end;
         end;
      end;
   end;
end;

procedure Tf2hFrm.gridKeyPress(Sender: TObject; var Key: Char);
begin
   with grid do begin
      _editAttr := THAttrObject(treeCtls.selected.data).attributes.attributeN[row - 1];
      with _editAttr do begin
         if (attrType = atBoolean) and (aoEditable in Options) then begin
            case key of
               #13, #32:
                  BooleanValue := not BooleanValue;
               't', 'T':
                  BooleanValue := true;
               'f', 'F':
                  BooleanValue := false;
            end;
            if _editAttr.Changed then
               invalidateGridCell(col, row);
            invalidateChangedAttributes;
         end;
      end;
   end;
end;

procedure Tf2hFrm.gridGetEditText(Sender: TObject; ACol, ARow: Int32;
  var Value: String);
begin
   _editAttr := THAttrObject(treeCtls.selected.data).attributes.attributeN[ARow - 1];
   value := _editAttr.value;
end;

procedure Tf2hFrm.gridSetEditText(Sender: TObject; ACol, ARow: Int32;
  const Value: String);
var
   v : string;
begin
   if grid.EditorMode then exit;
   v := Value;
   if _editAttr.isValid(v) then
      begin
         _editAttr.value := v;
         invalidateChangedAttributes;
      end
   else
      MessageBeep(word(-1));
end;

(*procedure Tf2hFrm.gridGetEditText(Sender: TObject; ACol, ARow: Int32;
  var Value: String);
begin
   _editAttr := THAttrObject(treeCtls.selected.data).attributes.attributeN[ARow - 1];
   value := _editAttr.value;
end;

procedure Tf2hFrm.gridSetEditText(Sender: TObject; ACol, ARow: Int32;
  const Value: String);
var
   v : string;
begin
   if grid.EditorMode then exit;
   v := Value;
   if _editAttr.isValid(v) then
      begin
         _editAttr.value := v;
         invalidateChangedAttributes;
      end
   else
      MessageBeep(word(-1));
end;*)

procedure Tf2hFrm.butExportClick(Sender: TObject);
begin
   if Form2HTML <> nil then begin
      with Form2HTML do begin
         if chkWH.Checked then
            exportWHChunkDialog
         else
            exportHTMLDialog;
      end;
   end;
end;

procedure Tf2hFrm.butBrowseClick(Sender: TObject);
begin
   if Form2HTML <> nil then begin
      with Form2HTML do begin
         if chkWH.Checked then
            browseWHChunkDialog
         else
            browseHTMLDialog;
      end;
   end;
end;

procedure Tf2hFrm.setAttrToDefault;
var
   attr : THAttribute;
begin
   attr := THAttrObject(treeCtls.selected.data).attributes.attributeN[grid.row - 1];
   with attr do
      Value := DefValue;
   with grid do begin
      if attr.Changed then
         invalidateGridCell(col, row);
      invalidateChangedAttributes;
   end;
end;

procedure Tf2hFrm.butDefaultClick(Sender: TObject);
begin
   setAttrToDefault;
   grid.SetFocus;
end;

procedure Tf2hFrm.gridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if not grid.editorMode then begin
      if (key = VK_DELETE) and (shift = []) then begin
         setAttrToDefault;
         key := 0;
      end;
   end;
end;


procedure Tf2hFrm.gridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  r : Integer;    // col, row
begin
   with grid do begin
      if _lastHR <> -1 then begin
         if getCapture <> handle then
            setCapture(handle); // To be informed when mouse goes out
      end;
      r := Y div (grid.defaultRowHeight + 1);
      if (X < 0) or (X > grid.Width) or (r < 1) or (r >= grid.rowCount) then begin
         releaseCapture;
         hintBar.caption := '';
         _lastHR := -1;
         exit;
      end;
      // Display hint only if not visible already
      if (_lastHR <> r)then begin
         _lastHR := r;
         // Test hint string
         hintBar.caption := ' ' + THAttrObject(treeCtls.selected.data).Attributes.AttributeN[r-1].Hint; // Ouf!
      end;
   end;
end;

procedure Tf2hFrm.FormCreate(Sender: TObject);
begin
   _lastHR := -1;
   split := TSplitterD2.create(self);
   with split do begin
      parent := self;
      cursor := crHSplit;
      left := 185;
      minSize := 30;
      width := 2;
      beveled := False;
      OnMoved := splitMoved; 
   end;
   panLeft.align := alLeft;
   split.align := alLeft;
   panRight.align := alClient;

   // Reference http://www.bobswart.nl/Weblog/Blog.aspx?RootId=5:5029
   butCancel.ModalResult := ID_CANCEL; 
end;

type
  THack = class(TWinControl);

constructor TSplitterD2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alLeft;
  Width := 3;
  Cursor := crHSplit;
  FMinSize := 30;
  FBeveled := True;
end;

procedure TSplitterD2.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
end;

procedure TSplitterD2.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TSplitterD2.ReleaseLineDC;
begin
  ReleaseDC(Parent.Handle, FLineDC);
end;

procedure TSplitterD2.Paint;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
      InflateRect(R, -1, 2) else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
end;

procedure TSplitterD2.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  function FindControl: TControl;
  var
    P: TPoint;
    I: Integer;
  begin
    Result := nil;
    P := Point(Left, Top);
    case Align of
      alLeft: Dec(P.X);
      alRight: Inc(P.X, Width);
      alTop: Dec(P.Y);
      alBottom: Inc(P.Y, Height);
    else
      Exit;
    end;
    for I := 0 to Parent.ControlCount - 1 do
    begin
      Result := Parent.Controls[I];
      if PtInRect(Result.BoundsRect, P) then Exit;
    end;
    Result := nil;
  end;

var
  I: Integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alLeft, alRight] then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := THack(FActiveControl).OnKeyDown;
          THack(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      DrawLine;
    end;
  end;
end;

procedure TSplitterD2.UpdateSize(X, Y: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    FSplit := X - FDownPos.X
  else
    FSplit := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft: S := FControl.Width + FSplit;
    alRight: S := FControl.Width - FSplit;
    alTop: S := FControl.Height + FSplit;
    alBottom: S := FControl.Height - FSplit;
  end;
  FNewSize := S;
  if S < FMinSize then
    FNewSize := FMinSize
  else if S > FMaxSize then
    FNewSize := FMaxSize;
  if S <> FNewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - FNewSize else
      S := FNewSize - S;
    Inc(FSplit, S);
  end;
end;

procedure TSplitterD2.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    DrawLine;
    UpdateSize(X, Y);
    DrawLine;
  end;
end;

procedure TSplitterD2.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    DrawLine;
    case Align of
      alLeft: FControl.Width := FNewSize;
      alTop: FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    StopSizing;
  end;
end;

procedure TSplitterD2.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TSplitterD2.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TSplitterD2.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      THack(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

end.

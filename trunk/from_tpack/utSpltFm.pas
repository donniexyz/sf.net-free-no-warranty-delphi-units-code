unit utSpltFm; {Utility code, introduction of splitter}
(*
Permission is hereby granted, on 11-May-2005, free of charge, to any person
obtaining a copy of this file (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies of the
Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

// Author of original version of this file: Michael Ax

{Note: This unit is no longer required in WebHub as of v2.045, May 2005.}

interface

{$DEFINE DOHELP}

{$I hrefdefines.inc}

uses
{$IFDEF MSWINDOWS}
  Windows, Forms, Controls, Graphics, ExtCtrls, Messages,
  {$IFDEF DOHELP}
  ComCtrls, Menus,
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  QForms, QControls, QGraphics, QExtCtrls, Types,
{$ENDIF}
  Classes, SysUtils, utForm;

type
  TtpSplitterForm = class(TtpFitForm)
//    procedure FormClose(Sender: TObject; var Action: TCloseAction);
//    procedure ExitBtnClick(Sender: TObject);
    procedure SplitEndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    { Private declarations }
    fPriorEndDrag: TEndDragEvent;
    fDragForm: TForm;
    fDragControl: TControl;
    fDragImg: TBitMap;
    fDragPaintBox: TPaintBox;
    fDragPaintPanel: TPanel;
    MouseCo: TPoint;
    fDragWasX: Integer;
    fDragPanel: TPanel;
    procedure SplitPaintBoxOver(Sender, Source: TObject;x,y:Integer;State:TDragState; var Accept:Boolean);
    procedure SplitPaintBoxPaint(Sender: TObject);
{$IFDEF MSWINDOWSNOLINUXYET}
  {$IFDEF DOHELP}
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
  {$ENDIF}
{$ENDIF}
  public
    constructor Create(aOwner:Tcomponent); override;
    class procedure SetPopupHelpIDs(const Value:String);
//    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure SplitterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    property SplitterControl: TControl read fDragControl write fDragControl;
    property PriorEndDrag: TEndDragEvent read fPriorEndDrag write fPriorEndDrag;
    end;

var
  SplitterForm: TtpSplitterForm;    //set or adjust this pointer from the app.
                                    // it defaults to the first TtpSplitterForm
                                    // created.

{
//to use, set the cursors and use this code:

procedure THubAppForm.SplitMainPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SplitterControl:=pnInstantForm;
  SplitterMouseDown(Sender,Button,Shift,X,Y);
end;
}

implementation

uses
  ucString
{$IFDEF DEBUG}
  ,webinfou
{$ENDIF}
;

{---------------------------------------------------------------}

var
  PopUpHelpIDs: string='';

class procedure TtpSplitterForm.SetPopupHelpIDs(const Value:String);
begin
  PopupHelpIDs:=Value;
end;

{---------------------------------------------------------------}

constructor TtpSplitterForm.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  if SplitterForm=nil then
    SplitterForm:=Self;
end;


//------------------------------------------------------------------------------

{$IFDEF MSWINDOWSNOLINUXYET}
  {$IFDEF DOHELP}
procedure TtpSplitterForm.WMHelp(var Message: TWMHelp);
var
  Control: TWinControl;
  ContextID: Integer;

  function GetMenuHelpContext(Menu: TMenu): Integer;
  begin
    Result := 0;
    if Menu = nil then Exit;
    Result := Menu.GetHelpContext(Message.HelpInfo.iCtrlID, True);
    if Result = 0 then
      Result := Menu.GetHelpContext(Message.HelpInfo.hItemHandle, False);
  end;

begin
  with Message.HelpInfo^ do
  begin
    if Message.HelpInfo.iContextType = HELPINFO_WINDOW then
    begin
      Control := FindControl(hItemHandle);
      if Control is TPageControl then
//        if ((Control.Parent=Application.Mainform)
//        or (Control.Parent.Parent=Application.Mainform))
//        and (Control.ControlCount>0) then
//          Control:= TPageControl(Control).ActivePage.Controls[0]
//        else
          Control:= TPageControl(Control).ActivePage;

      while (Control <> nil) and (Control.HelpContext = 0) do
      //
        if Control is TPageControl then
          Control:= TPageControl(Control).ActivePage
        else
        if (Control is TTabsheet) then begin
          if (Control.ControlCount>0)
          and (Control.Controls[0] is tPanel)
          and (tWincontrol(Control.Controls[0]).HelpContext<>0) then
            Control:= tWincontrol(Control.Controls[0])
          else
            Control := TTabsheet(Control).PageControl.Parent;
          end
        else
      //
        Control := Control.Parent;
      //
      if Control = nil then Exit;
      ContextID := Control.HelpContext;
    end
    else  { Message.HelpInfo.iContextType = HELPINFO_MENUITEM }
    begin
      ContextID := GetMenuHelpContext(Menu);
      if ContextID = 0 then
        ContextID := GetMenuHelpContext(PopupMenu);
//      inherited; // WMHelp(Message);
//      exit;
    end;
  end;
  //
{$IFDEF DEBUG}
  SetStatus('Help Context '+inttostr(ContextID));
{$ENDIF}
  //
  if ContextID= 0 then
    ContextID:= Application.Mainform.HelpContext;
  if (ContextID>=20000)
  or (biHelp in BorderIcons)
  or IsIn(PopUpHelpIDs,IntToStr(ContextID),',')
  then
    Application.HelpCommand(HELP_CONTEXTPOPUP, ContextID)
  else
    Application.HelpContext(ContextID);
end;
  {$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------
//tray related code


(*
uses TrayForm;

procedure TtpSplitterForm.WMSysCommand(var Message: TWMSysCommand);
begin
  if (Message.CmdType and $FFF0 = SC_MINIMIZE)
  and (not TrayControlForm.MainIconShowing) then
    Close //close the form in newshell.
  else
    inherited;
end;

procedure TtpSplitterForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caHide;
  TrayControlForm.HideForm;
end;

procedure TtpSplitterForm.ExitBtnClick(Sender: TObject);
begin
  TrayControlForm.TerminateApp;
end;
*)

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TtpSplitterForm.SplitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  wc:TWinControl;
  fEndDrag: TEndDragEvent;
begin
  if not (Sender is tPanel) then
    raise Exception.Create('Splitting requires a panel!');

  fEndDrag:=SplitEndDrag;
  if (longint(@fPriorEndDrag)=longint(@tPanel(Sender).OnEndDrag))
  or (longint(@tPanel(Sender).OnEndDrag){longint(@fPriorEndDrag)}=longint(@fEndDrag)) then
    fPriorEndDrag:=nil
  else
    fPriorEndDrag:=tPanel(Sender).OnEndDrag;

  tPanel(Sender).OnEndDrag:=SplitEndDrag;
  fDragPanel:= TPanel(Sender);

  wc:=fDragPanel.Parent;  //the splitter panel may be on a 'foreign-form'
  while not (wc is TForm) and (wc.Parent<>nil) do
    wc:=wc.parent;      //find the form that visually 'owns' the splitter.
  if not (wc is TForm) then
    raise Exception.Create('Splitter-Panel must be visually owned by a TForm!');
  fDragForm:= TForm(wc);
{$IFDEF MSWINDOWSNOLINUXYET}
  fDragImg:= fDragForm.GetFormImage; //wow!
{$ELSE}
  fDragImg:= TBitMap.Create;
{$ENDIF}

  GetCursorPos(MouseCo);
  MouseCo:=fDragForm.ScreenToCLient(MouseCo);

  case fDragPanel.Align of
  alLeft,
  alRight:
    fDragWasX:= -MouseCo.x//(TControl(Sender).Left+X)
  else
    fDragWasX:= -MouseCo.y//(TControl(Sender).Top+Y)
    end;

  fDragPanel.BeginDrag(True);
  if not assigned(fDragPaintPanel) then begin
    fDragPaintPanel:=TPanel.Create(Self);
    with fDragPaintPanel do begin
      Visible:=False;
      Parent:=fDragForm;
      BevelInner:=bvNone;
      BevelOuter:=bvNone;
      end;
    fDragPaintBox:=TPaintBox.Create(fDragPaintPanel);
    with fDragPaintBox do begin
      Parent:=fDragPaintPanel;
      OnDragOver:=SplitPaintBoxOver;
      OnPaint:=SplitPaintBoxPaint;
      Align:=alClient;
      end;
    end;
  with fDragPaintPanel do begin
    Top:=0;//-1;
    Left:=0;//-1;
    Width:=fDragForm.ClientWidth;  //self.
    Height:=fDragForm.ClientHeight-20;
    BringToFront;
    Visible:=True;
    end;
end;

procedure TtpSplitterForm.SplitEndDrag(Sender, Target: TObject; X,Y: Integer);
var
  aMouseCo: Tpoint;
  fEndDrag: TEndDragEvent;
begin
  GetCursorPos(aMouseCo);
  aMouseCo:=fDragForm.ScreenToCLient(aMouseCo);
  case fDragPanel.Align of
    alLeft:  fDragControl.Width:=fDragControl.Width+aMouseCo.x-MouseCo.x;
   alRight:  fDragControl.Width:=fDragControl.Width+MouseCo.x-aMouseCo.x;
     alTop: fDragControl.Height:=fDragControl.Height+aMouseCo.y-MouseCo.y;
  alBottom: fDragControl.Height:=fDragControl.Height+MouseCo.y-aMouseCo.y;
    end;
  fDragPaintPanel.free;
  fDragPaintPanel:=nil;
  //fDragPaintPanel.SendToBack;
  //fDragPaintPanel.Visible:=False;
  fDragImg.Free;
  if assigned(fPriorEndDrag)
  and assigned(sender)
  and (sender is tpanel) then begin
    tPanel(Sender).OnEndDrag:=fPriorEndDrag;
    fEndDrag:=fPriorEndDrag;
    fPriorEndDrag:=nil;
    fEndDrag(Sender,Target,x,y);
    end;
end;


procedure TtpSplitterForm.SplitPaintBoxOver(Sender, Source: TObject;x,y:Integer;State:TDragState; var Accept:Boolean);
var
  t,h:integer;
  al,at:integer;
  c:tControl;
begin
  Accept:= (Source=fDragPanel) or (Sender=fDragPanel);
  if Accept and (fDragWasX>0) then begin

    at:=fDragPanel.Top;
    al:=fDragPanel.Left;
    c:=fDragPanel.parent;
    while assigned(c) and (not (c is tForm)) do begin
      at:=at+c.Top;
      al:=al+c.Left;
      c:=c.Parent;
      end;

    case fDragPanel.Align of
    alLeft,
    alRight:
      begin
      t:=at;
      h:=t+fDragPanel.Height;
      if assigned(Menu) then
        h:=h-19;
      with fDragPaintBox, Canvas do begin
        PolyLine([Point(fDragWasX,t), Point(fDragWasX,h)]);
        PolyLine([        Point(X,t),         Point(X,h)]);
        fDragWasX:=x;
        end;
      end;

    alTop,
    alBottom:
      begin
      t:=al;
      h:=t+fDragPanel.Width;
      with fDragPaintBox, Canvas do begin
        PolyLine([Point(t,fDragWasX), Point(h,fDragWasX)]);
        PolyLine([Point(t,y),         Point(h,y)]);
        fDragWasX:=y;
        end;
      end;

    else
      end;

    end;
end;


procedure TtpSplitterForm.SplitPaintBoxPaint(Sender: TObject);
var
  t,h:integer;
  al,at:integer;
  c:tControl;
begin
  with fDragPaintBox, Canvas do begin
    Draw(0,0,fDragImg);

    at:=fDragPanel.Top;
    al:=fDragPanel.Left;
    c:=fDragPanel.parent;
    while assigned(c) and (not (c is tForm)) do begin
      at:=at+c.Top;
      al:=al+c.Left;
      c:=c.Parent;
      end;

    case fDragPanel.Align of
    alLeft,
    alRight:
      begin
      t:=at;
      h:=t+fDragPanel.Height;
      if assigned(Menu) then
        h:=h-19;
      if fDragWasX<0 then begin
        fDragWasX:=-fDragWasX;
        Pen.Mode:= pmXor;
        Pen.Color:= clWhite;
        PolyLine([Point(fDragWasX,t), Point(fDragWasX,h)]);
        end;
      end;

    alTop,
    alBottom:
      begin
      t:=al;
      h:=t+fDragPanel.Width;
      if fDragWasX<0 then begin
        fDragWasX:=-fDragWasX;
        Pen.Mode:= pmXor;
        Pen.Color:= clWhite;
        PolyLine([Point(t,fDragWasX), Point(h,fDragWasX)]);
        end;
      end;
    else
      end;
    end;
end;

{---------------------------------------------------------------}
{---------------------------------------------------------------}
end.


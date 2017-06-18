unit tpKiosk.pas; {Kiosk component}
(*
Permission is hereby granted, on 21-Mar-2003, free of charge, to any person
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

//This component arose from after a discussion on the merits of letting
//any app take over windows and force the user into a single application.
//this took place in early june'97 in the delphi-aquarium, a mailing list
//sponsored by DELPHI OBJECT LESSONS, a delphi specific technical journal.
//http://www.objectlessons.com/~ol/
//Contributors (thanks to):
//Greg Lief, Kent Kingery
//original question by Robert Elmore

//since the issue of keeping an app exclusively in the foreground will very
//likely involve putting some checks on the user's starting and stopping of
//the app, and making some kind of supervisor mode available, this is in no
//way a complete solution to the task of writing a test-taking or kiosk app.
//but it's a start..

interface

uses
  Windows, Sysutils, Messages, Controls, Classes, Forms;

type
  TNotFormOwnerError = class(Exception);

  TtpKiosk = class(TComponent)
  private
    fEnabled: Boolean;
    fAppWndProc : Pointer;
    fOldAppWndProc : Pointer;
    procedure ApplicationWndProc(var WinMsg: TMessage);
    procedure SetEnabled(Value:Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read fEnabled write SetEnabled default false;
    end;

implementation

constructor TtpKiosk.Create(AOwner: TComponent);
begin
  inherited;
  if not (AOwner is TForm) then
    //make sure that we're owned by a form, because after all,
    //the job is to return to/keep our owner in focus.
    raise TNotFormOwnerError.Create('A Form must be the owner');
end;

destructor TtpKiosk.Destroy;
begin
  //unhook ourselves in any case.
  Enabled:=False;
  inherited;
end;

//While a 16 bit version of this code could take advantage of a windows
//function to take over the screen, the 32 bit api does not allow the same.
//to 'kludge' around that limitation, ostensibly for test-taking apps,
//this code will re-activate the app as soon as it's disabled.
//to do so, we'll plug into the Application's Window procedure and do a
//single call to the api whenever we switch away from our app.
// since this is good for the entire app, we may circumvent problems inherent
//in the 16 bit approach which is only useful for single forms and mdi apps at
//this time..  (this code should relatively easy adapt to 16 bit)

procedure TtpKiosk.ApplicationWndProc(var WinMsg: TMessage);
begin
  //might want to eat WM_CLOSE here and in wndproc for the main form as well..
  with WinMsg do begin
    //if we're leaving, let it ripple through as we'll re-activate in a sec..
    Result:= CallWindowProc(fOldAppWndProc,Application.Handle,Msg,wParam,lParam);
    if Msg = CM_DEACTIVATE then
      //return the owning form to the front, re-activating the application
      SetForegroundWindow(tForm(Owner).Handle);
      //BringWindowToTop (16bit) supposedly does not work. why?
    end;
end;

procedure TtpKiosk.SetEnabled(Value:Boolean);
const
  TaskBarShowCommands: array[Boolean] of Integer = (SW_SHOW,SW_HIDE); //false,true
var
  hTaskBar: HWND;
begin
  if Value=fEnabled then
    exit;
  //when/if extending for 'TtpKioskIt', follow the same pattern to link
  //a WndProc for the main form which can deal with interception WM_Close.
  if Value then begin
    //link the above proc into the window proc chain
    fOldAppWndProc:= Pointer(GetWindowLong(Application.Handle,GWL_WNDPROC));
    fAppWndProc:= MakeObjectInstance(ApplicationWndProc);
    if not assigned(fAppWndProc) then
      raise EOutOfResources.Create('Resources Exhausted');
    SetWindowLong(Application.Handle,GWL_WNDPROC, LongInt(fAppWndProc));
    end
  else begin
    //break the connection.
    if assigned(fOldAppWndProc) then begin
      SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(fOldAppWndProc));
      fOldAppWndProc:=nil;
      end;
    if assigned(fAppWndProc) then begin
      FreeObjectInstance(fAppWndProc);
      fAppWndProc:=nil;
      end;
    end;
  //this took care of the application's message processing.
  //let's see if there's a taskbar (e.g. 32bit newshell) and hide that
  //now that it's going to be useless (but for the clock)
  hTaskBar:= FindWindow('Shell_TrayWnd', nil);
  if hTaskBar<>0 then
    //while this basically works, the app does not take over the entire screen
    //as somehow the available screen size does not get recomputed.
    //need to dig up how to do that..
    ShowWindow(hTaskBar, TaskBarShowCommands[Value]);
  //so far so good, store the flag.
  fEnabled:=Value;
end;

end.

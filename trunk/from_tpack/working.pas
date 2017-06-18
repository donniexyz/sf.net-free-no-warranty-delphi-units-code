unit working;
(*
Permission is hereby granted, on 24-Mar-2003, free of charge, to any person 
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

Author of original version of this file: Michael Ax
*)

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Windows, Forms, Dialogs, 
  StdCtrls, Buttons, ExtCtrls, 
  OkCore, utForm;

type
{------------------------------------------------------------------------------}
{TOkBoxForm defines the look the generic OkBox. Change this form or modify the
look at runtime via TOk.OnActivate; Take care to keep the default events!
attached to FormClose and ButtonClick; The Defaults are implemented at the end.}

  TWorkingMsgFormStop   = procedure(Sender: TObject;Var CanStop:Boolean) of object;

  TWorkingForm = class(ttpFitForm)
    Panel1: TPanel;
    StopLabel: TLabel;
    StopButton: TBitBtn;
    procedure StopButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanStop: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  protected
    { Protected declarations }
    fOkClose: Boolean;
    fOnOkStop: TWorkingMsgFormStop;
  public
    { Public declarations }
    property OkClose:Boolean read fOkClose write fOkClose;
    property OnOkStop: TWorkingMsgFormStop read fOnOkStop write fOnOkStop;
  published
    { Published declarations }
  end;

{------------------------------------------------------------------------------}

  TOkFormActivate = procedure(StopForm:TForm;StopLabel:TLabel;StopButton:TButton) of object;

  TWorkingMsg = class(TOk)
    Form:  TWorkingForm;
  private
    { Private declarations }
    fVisible:        Boolean;
    fOnFormActivate: TOkFormActivate;  {used to setup form.}
  protected
    { Protected declarations }
    function BusyCount(Add:ShortInt):Integer;
    procedure   SetVisible(Flag:Boolean);            Virtual;
    procedure   SetActive(Flag:Boolean);             override;
    procedure   DoOkStart(Var CanStart:Boolean);     override;
    function    FreezeFormHandle:HWND;               override;
    procedure   SetCritical(Flag:Boolean);           override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);           override;
    procedure   Reset;                               override;
    procedure   OkStopHandler(Sender:TObject;Var CanStop:Boolean);

    function IsNotBusy:Boolean;
    procedure BusyReset;
    procedure BusyOn;
    procedure BusyMsg(const Text:String);
    procedure BusyOff;

  published
    { Published declarations }
    property Visible:     Boolean      read fVisible    write SetVisible default true;
    property OnFormActivate: TOkFormActivate read fOnFormActivate write fOnFormActivate;
  end;

{------------------------------------------------------------------------------}

//procedure Register;

procedure waitNSeconds(nSeconds:DWORD; msg:TWorkingMsg);

implementation

{------------------------------------------------------------------------------}
{$R *.DFM}


procedure waitNSeconds(nSeconds:DWORD; msg:TWorkingMsg);
var
  dt,i:DWORD;
begin
  dt:= GetTickCount + nseconds*1000;
  //
  while (nSeconds>0) do begin
    if (msg<>nil) and NOT msg.ok then break;
    if (GetTickCount>dt) then
      break;
    //
    i:=GetTickCount+1000;
    while (GetTickCount<i) do begin
      if (msg<>nil) and NOT msg.ok then break;
      application.processMessages;
      sleep(50);
      end;
    dec(nSeconds);
    end;
end;

{------------------------------------------------------------------------------}
{TOkBoxUForm.  *Here* is the OkBox Form code which defines ESSENTIAL events.
You can completely redefine the Form via Ok.OnActivate; Below are the
things that have to happen on the Form and CancelButton}

procedure TWorkingForm.StopButtonClick(Sender: TObject);
begin
  If Assigned(fOnOkStop) then   {if a OnOkStop proc is defined, allow closing.}
    fOnOkStop(Sender,fOkClose);
end;

procedure TWorkingForm.FormCloseQuery(Sender: TObject; var CanStop: Boolean);
begin
  StopButtonClick(Sender);
  CanStop:=fOkClose;      {Form Property can be reset via fOnOkStop proc}
end;

procedure TWorkingForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 { Action:=caFree;}   {can't free all the time; keep one instance up to reset.}
end;

{------------------------------------------------------------------------------}

constructor TWorkingMsg.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  fVisible:=true;
end;

procedure TWorkingMsg.SetVisible(Flag:Boolean);
{must be much stronger on memory issues?
  just not call activate if not visible!}
begin
  if flag<>fVisible then begin
    fVisible:=Flag;
    if assigned(Form) then
      Form.Visible:=Flag;
    end;
end;

procedure TWorkingMsg.SetActive(Flag:Boolean);
begin
  if Enabled and Flag<>Active then begin
    inherited SetActive(Flag);
    if not Active and assigned(Form) then  {do we have one?}
      Form.hide{Close};
    end;
end;

procedure TWorkingMsg.SetCritical(Flag:Boolean);
{OkTry can not be stopped when in a critical section}
begin
  if flag<>Critical then begin
    inherited SetCritical(Flag);
    if assigned(Form) then
      Form.StopButton.Enabled:=not Critical;
    end;
end;

procedure TWorkingMsg.DoOkStart(Var CanStart:Boolean);
begin
  Inherited DoOkStart(CanStart);
  if CanStart then begin
    Application.CreateForm(TWorkingForm,Form);
    Form.OnOkStop:=OkStopHandler;
    Form.OkClose:=True;
    if assigned(fOnFormActivate) then
      fOnFormActivate(tForm(Form),Form.StopLabel,Form.StopButton);
    if fVisible then begin
      Form.Show;
      Form.Update;
      end;
    end;
end;

function TWorkingMsg.FreezeFormHandle:HWND;
begin
  result:=Form.Handle;
end;

procedure TWorkingMsg.OkStopHandler(Sender:TObject;Var CanStop:Boolean);
begin
  if Critical then                {can not exit in a critical section!}
    CanStop:=False               {let the user beware!}
  else begin
    Active:=False;  {try to turn off.. ancestor will now call user's CanCanel proc}
    CanStop:=Stop; {if that proc concurs, we allow the OkBoxform to close}
     {That's all. The Active Flag has already been set to false when we return}
    end;
end;

procedure TWorkingMsg.Reset;
{unconditional deactivate. very useful while setting up OkTryes. put a call
on a button somewhere to allow you to break out of things regardless of what you
coded! very helpful during development (to me anyway) it is not used inside here}
begin
  inherited Reset;
  if assigned(Form) then  {do we have one?}
    with Form do begin     {begin manual override <g>}
      OnOkStop:=nil;            {no denying the exit this time}
      OkClose:=True;          {manually allow Close-- that'll set the vars.}
      end;
  Active:=False;
  if assigned(Form) then begin
    Form.Free;
    Form:=nil;
    end;
end;



{------------------------------------------------------------------------------}
{ BUSY..                                                                       }
{------------------------------------------------------------------------------}
{the following is a set of procs to manage telling the user that we're busy.}
{in order to implement this well, we're tracking how often the box has been turned
on and off, placing it and removing it as needed.}

var
  Count:Integer;
  c:TCursor;

function TWorkingMsg.BusyCount(Add:ShortInt):Integer;
{keep track of how often we've been turned on.}
{parameters used to combine get/set}
begin
  Count:=Count+Add;
  if Count<=0 then begin
    Count:=0;
    Screen.Cursor:=c;
    end
  else
    if Count=Add then begin {just turned on}
      c:=Screen.Cursor;
      Screen.Cursor:=crHourGlass;
      end;
  Result:=Count;
end;

function TWorkingMsg.IsNotBusy:Boolean;
{inquire about the box status and syncronize the counter if it's off}
begin
  Result:=Stop;
  if Result then
    if BusyCount(0)>0 then
      BusyCount(-BusyCount(0));
end;

procedure TWorkingMsg.BusyOn;
{Turn it on anyway!! (resetting the box) and up the counter}
begin
  OkOn;
  BusyCount(1);
end;

procedure TWorkingMsg.BusyMsg(const Text:String);
{could stall! this is why this functionality belongs into StopBox! and TOK!}
begin
  Form.StopLabel.Caption:=Text;
end;

procedure TWorkingMsg.BusyOff;
{turn off only when we're counting back to 0 and it's on in the second place}
begin
  if BusyCount(0)=1 then
    if not IsNotBusy then
      OkOff;
  BusyCount(-1);
end;

procedure TWorkingMsg.BusyReset;
begin
  if BusyCount(0)>0 then
    BusyCount(-BusyCount(0));
  BusyOff;
end;

//----------------------------------------------------------------------
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TWorkingMsg]);
//end;
//----------------------------------------------------------------------

initialization
  Count:=0;
  c:=crDefault;

end.

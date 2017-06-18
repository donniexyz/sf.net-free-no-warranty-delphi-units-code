unit tpStatus;  {defines customized panels within a status bar.}

(*
Permission is hereby granted, on 18-Feb-2005, free of charge, to any person
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

TtpStatusBar  renamed to TtpStatusBar1995           18-Feb-2005
TtpStatusBar2 renamed to TtpStatusBarDateTime1995   18-Feb-2005
*)

interface

uses Forms, Windows, SysUtils, Graphics, TypInfo, ComCtrls, Dialogs, Classes,
  Controls, Buttons, ExtCtrls,
  utIPCObj, utThread, ucInteg, ucString;

type
  TtpNextStatus = Class(TtpThread)
  private
    fOwner: TStatusBar;
    fStrings: TStringList;
    ev:TtpEvent;
    cs:TtpCriticalSection;
    procedure SetStatus(const Value:String);
    function GetStatus:String;
  Protected
    procedure Execute; override;
    procedure Synchronized;
  public
    constructor Create(CreateSuspended:Boolean;aOwner:TStatusBar);
    destructor Destroy; override;
    property NewStatus:String write SetStatus;
    End;

type
  TtpStatusBar1995 = class(TStatusBar)
  private
    fIsHint:boolean;
    ftpNextStatus: TtpNextStatus;
    function GetStatus:String;
    procedure SetStatus(const Value:String);
    procedure SetUniqueName(const Value:String);
  protected
  public
    constructor Create(aOwner:TComponent); Override;
    destructor Destroy; override;
    procedure Loaded; Override;
    procedure Flash;
    procedure SetTheHint(const Value:String);
    procedure SetObjectStatus(Sender:TObject;const Value:String);
    property UniqueName:String write SetUniqueName stored false;
  published
    property Status:String read GetStatus write SetStatus stored false;
    end;

type
  TMpModes=(mpTime,mpDate);

  TtpStatusBarDateTime1995 = class(TtpStatusBar1995)
  private
    fmpMode: TmpModes;
    fTimer: TTimer;
  protected
    procedure UpdatePanel(Sender: TObject);
    procedure SetMpMode(Value:TmpModes);
    function GetTimeSlot:String;
    procedure SetTimeSlot(const Value:String);
  public
    constructor Create(aOwner:TComponent); Override;
    destructor Destroy; Override;
    procedure Click; Override;
  published
    property TimeSlot:String read GetTimeSlot write SetTimeSlot stored false;
    property mpMode: TmpModes read fmpMode write SetMpMode;
    end;

procedure Register;

implementation

{---------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('TPack', [TtpStatusBar1995, TtpStatusBarDateTime1995]);
end;

Constructor TtpNextStatus.Create(CreateSuspended: Boolean;aOwner:TStatusBar);
begin
  inherited Create(true);
  fOwner:=aOwner;
  fStrings:=TStringList.Create;
  cs:=TtpCriticalSection.Create;
  ev:=TtpEvent.CreateNamed('',False); //autoreset

  FreeOnTerminate:=True;  //this thread will not be destroyed here, it will
                          //be cleared when it's done running.

  if not CreateSuspended then
    Resume;
end;

destructor TtpNextStatus.Destroy;
begin
  cs.Free;
  cs := nil;
  ev.Free;
  ev := nil;
  fStrings.Free;
  fStrings := nil;
  inherited Destroy;
end;

procedure TtpNextStatus.SetStatus(const Value:String);
begin
  with cs,fStrings do try
    lock;
    Text:=Value;  //do not use a buffer right in this rev.
//    Add(Value);
  finally
    unlock;
    ev.signal;
    end;
//Log('SetNewMail '+inttostr(fMail.count));
end;

function TtpNextStatus.GetStatus:String;
begin
//Log('GetNextMailText');
  with cs,fStrings do try
    lock;
    if Count=0 then
      Result:=''
    else begin
      Result:=Strings[0];
      Delete(0);
      end;
  finally
    unlock;
    end;
end;

procedure TtpNextStatus.Synchronized;
var
  a1:string;
begin  //tthread //classes
  if terminated
  or not assigned(application) //in an app
  or application.terminated //app is running
  then
    exit;

  a1:='';

  if assigned(fOwner)
  then
    with fOwner do
      if (componentState=[])   //not destroying, reading or writing
      or (componentState=[csDesigning]) then
        a1:=GetStatus;

  if (a1<>'') and (a1[1]='!') then begin
    SysUtils.beep;
    delete(a1,1,1);
    end;

//  if a1<>'' then
    with fOwner do
        if Panels.Count>0 then
          Panels[pred(Panels.Count)].Text:=a1;
    //    Update;
//        end;
//      else
//        SysUtils.beep;
end;

procedure TtpNextStatus.Execute;
var
  b: Boolean;
begin
  while not terminated do
  begin
    if  assigned(application) //in an app
    and (not application.terminated) //app is running
    and assigned(fOwner)
    and ((fOwner.componentState=[])   //not destroying, reading or writing
      or (fOwner.componentState=[csDesigning])) then
      b:=true//(fStrings.Count>0)
    else
      b:=false;

    if b then
      Self.Synchronize(Synchronized);
    if not terminated then
      ev.wait(INFINITE);
  end;
end;

{----------------------------------------------------------------------}
{----------------------------------------------------------------------}

constructor TtpStatusBar1995.Create(aOwner:TComponent);
begin
  inherited Create(aOwner); {sleuth}
  ControlStyle:= ControlStyle + [csAcceptsControls];
  ShowHint:=True;
  if not (csLoading in aOwner.ComponentState) then
    UniqueName:='tpStatusBar';
  ftpNextStatus:=TtpNextStatus.Create(False,Self);
end;

destructor TtpStatusBar1995.Destroy;
begin
 if assigned(fTpNextStatus) then
 begin
    ftpNextStatus.Terminate;
    ftpNextStatus.ev.signal;
    {ftpNextStatus.WaitFor is not appropriate here because this is a TtpThread,
     not a Delphi TThread. 28-May-2003 AML}
    ftpNextStatus:=nil;
  end;
  inherited Destroy;
end;

procedure TtpStatusBar1995.Loaded;
begin
  inherited Loaded;
  if Panels.Count=0 then
    Panels.Add;

  if not (csdesigning in owner.componentstate)
  and not assigned(fTpNextStatus) then
  begin
    ftpNextStatus:=TtpNextStatus.Create(False,Self);
  end;

end;

//

procedure TtpStatusBar1995.SetUniqueName(const Value:String);
var
  i:integer;
  a:string[4];
begin
  a:='1';
  i:=1;
  while true do
    if owner.findcomponent(Value+a)=nil then begin
      name:= Value+a;
      break;
      end
    else begin
      inc(i);
      a:=inttostr(i);
      end;
end;

function TtpStatusBar1995.GetStatus:String;
begin
  if Panels.Count>0 then
    Result:=Panels[pred(Panels.Count)].Text
  else
    Result:='';
end;

procedure TtpStatusBar1995.SetTheHint(const Value:String);
begin
  if not fIsHint and (Value='') then
    exit;
  SetStatus(Value);
  fIsHint:=True;
end;

procedure TtpStatusBar1995.SetStatus(const Value:String);
begin
  if self=nil then
    exit;
  fIsHint:=False;
  if assigned(fTpNextStatus) then
    ftpNextStatus.NewStatus:=Value
  else
    if Panels.Count>0 then
      Panels[pred(Panels.Count)].Text:=Value;
end;

procedure TtpStatusBar1995.Flash;
var
  i:integer;
  c:tColor;
begin
  c:=Color;
  for i:=0 to 1 do begin
    Color:=tColor(not Color);
    Update;
    Sleep(16);
    end;
  Color:=tColor($FFFFFF);
  Update;
  Sleep(7);
  Color:=tColor(random($FFFF) shl 8);
  Update;
  Sleep(52);
  for i:=0 to 1 do begin
    Color:=tColor(not Color);
    Update;
    Sleep(21);
    end;
  Color:=c;
  Update;
end;

procedure TtpStatusBar1995.SetObjectStatus(Sender:TObject;const Value:String);
begin
  if not Visible then
    exit;
  if Sender=nil then
    Status:=Value
  else
    if Sender is tComponent then
      Status:=tComponent(Sender).Name+': '+Value
    else
      Status:=Sender.ClassName+': '+Value;
end;

{}

constructor TtpStatusBarDateTime1995.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  Panels.Add;
  fmpMode:=mpTime;
  fTimer:=TTimer.Create(self);
  with fTimer do
  begin
    Enabled:=True;
    OnTimer:=UpdatePanel;
    Interval:=1000;
  end;
end;

destructor TtpStatusBarDateTime1995.Destroy;
begin
  fTimer.Enabled:=false;
  fTimer.Free;
  fTimer := nil;
  inherited Destroy;
end;

procedure TtpStatusBarDateTime1995.SetMpMode(Value:TMpModes);
begin
  if Value<>fmpMode then
  begin
    fmpMode:=Value;
    fTimer.Enabled:=(Value=mpTime);
    updatePanel(self);
  end;
end;

procedure TtpStatusBarDateTime1995.Click;
begin
  if fmpmode<high(tmpmodes) then
    mpMode:=succ(fMpMode)
  else
    mpmode:=low(tmpmodes);
  inherited Click;
end;

procedure TtpStatusBarDateTime1995.UpdatePanel(Sender: TObject);
begin
  case fmpMode of
    mpTime: TimeSlot:= TimeToStr(Time);
    mpDate: TimeSlot:= DateToStr(Now);
  end;
end;

function TtpStatusBarDateTime1995.GetTimeSlot:String;
begin
  if Panels.Count>1 then
    Result:=Panels[pred(pred(Panels.Count))].Text
  else
    Result:='';
end;

procedure TtpStatusBarDateTime1995.SetTimeSlot(const Value:String);
begin
  if Panels.Count>1 then
    Panels[pred(pred(Panels.Count))].Text:=Value;
end;

end.


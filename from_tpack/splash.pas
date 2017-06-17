unit splash;
(*
Permission is hereby granted, on 1-Nov-2003, free of charge, to any person
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
  SysUtils, Messages, Classes, Graphics, Controls, Windows
, Forms, Dialogs, StdCtrls, ExtCtrls
, UpdateOk, tpAction
, utForm;


type
  TtPackSplashScreen = class(TtpFitForm)
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    fTimer: TTimer;
    fOnDeactivating:TNotifyEvent;
    procedure TimerOff(Sender: TObject);
  public
    { Public declarations }
    procedure ShowFor(PauseTime,FloatTime:Longint);
    property OnDeactivating:TNotifyEvent read fOnDeactivating write fOnDeactivating;
    end;

 TtpSplash = class(TTpAction)
 private
   fPauseTime: Longint;
   fFloatTime: Longint;
   fOnDeactivating:TNotifyEvent;
 protected
   function GetTest:Boolean; override;
 public
   constructor Create(aOwner:TComponent); override;
   procedure Execute; override;
 published
   property PauseTime:LongInt read fPauseTime write fPauseTime default 1000;
   property FloatTime:LongInt read fFloatTime write fFloatTime default 200;
   property OnDeactivating:TNotifyEvent read fOnDeactivating write fOnDeactivating;
   end;

{-------------------------------------------------------------------------}

//procedure Register;

implementation

{-------------------------------------------------------------------------}

{$R *.DFM}

Type
  TSplashFlag=(splInitialized,splInDCL);
  TSplashFlags= set of TSplashFlag;
var
  SplashFlags:TSplashFlags;
  SplashScreen:TtPackSplashScreen;

{ This will make the form disappear, and get freed, when ANY other form or window pops up.}

procedure TtPackSplashScreen.ShowFor(PauseTime,FloatTime:Longint);
begin
  Show;                        {bring up the form}
  Update;                       {the display}
  if PauseTime>0 then begin
    fTimer:=TTimer.Create(self); {make a timer}
    with fTimer do try
      Enabled:=False;            {begin setting it}
      Interval:=PauseTime;         {set duration and event}
      OnTimer:=TimerOff;
      Enabled:=True;             {start the time}
      while Enabled do
        Application.ProcessMessages;     {AND WAIT RIGHT HERE TILL THE PAUSE TIME IS UP}
    finally
      fTimer.Free;
      fTimer:=nil;
      end;
    end;
  if FloatTime>0 then begin
    fTimer:=TTimer.Create(self); {make a timer}
    with fTimer do begin
      Enabled:=False;            {begin setting it}
      Interval:=FloatTime;         {set duration and event}
      OnTimer:=TimerOff;
      Enabled:=True;             {start the time}
      end;                       {DO NOT WAIT HERE BUT GO ON WITH OTHER INITS.}
    end;
end;

procedure TtPackSplashScreen.TimerOff(Sender: TObject);
begin
  fTimer.Enabled:=False;
end;

procedure TtPackSplashScreen.FormDeactivate(Sender: TObject);
begin
  try
    if assigned(fOnDeactivating) then
      fOnDeactivating(Self); {can turn off the timer!}
    if fTimer<>nil then
      try
        while fTimer.Enabled do
          Application.ProcessMessages;
      finally
        fTimer.Free;
        fTimer:=nil;
        end;
  finally
    SplashScreen.Free;
    SplashScreen:= nil;
    end;
end;

{}

constructor TtpSplash.Create(aOwner:TComponent);
begin
  inherited create(aOwner);
  fPauseTime:=1000;
  fFloatTime:=200;
  if not (csDesigning in ComponentState) then
    Execute;
end;

procedure TtpSplash.Execute;
begin
  if splInDCL in SplashFlags then
    exit;
  SplashScreen:= TtPackSplashScreen.Create(nil);
  With SplashScreen do begin
    OnDeActivate:=FormDeactivate;
    OnDeactivating:=Self.OnDeactivating;
    if splInDCL in SplashFlags then
      SplashScreen.Show {do not use timers while in a library}
    else
      SplashScreen.ShowFor(fPauseTime,fFloatTime);
    end;
end;


function TtpSplash.GetTest:Boolean;
begin
  Result:= Assigned(SplashScreen);
end;

{----------------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtpSplash]);
//end;

{----------------------------------------------------------------------------------------}

initialization
  SplashFlags:=[];
  SplashScreen:=nil;
//{$IFNDEF WIN32}
//{ Here's the call to automatically show the SplashScreen when this unit is initialized!}
//  if not (splInitialized in SplashFlags) then
//    Initialize;
//{$ENDIF}
end.


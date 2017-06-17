unit retry;
(*
Permission is hereby granted, on 6-May-2003, free of charge, to any person 
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

interface

uses Classes, SysUtils, ExtCtrls, Forms, Dialogs, Controls, 
  ucInteg;

type
  TExceptionReAction = (reAsk, reRetry, reIgnore, reRaise);
    {defines what Retry/ErrorMsg can do in response to an exception}

type

{------------------------------------------------------------------------------}
{ TInterval                                                                    }
{------------------------------------------------------------------------------}

  TInterval = class(TPersistent)
  private
    fInterval:Longint;
    fRandom:Longint;
    fProcessMessages: Boolean;
    fOnWaitOver: TNotifyEvent;
  protected
    procedure   TimerEvent(Sender: TObject);
    procedure   DoWaitOver(Sender: TObject); {virtual;}
  public
    constructor Create;
    constructor CreateDelay(DelayMS:LongInt;WaitNow:Boolean);
    constructor CreateRandomDelay(ShortMS,LongMS:LongInt;WaitNow:Boolean);
    procedure   Execute;
  published
    property Interval: LongInt read fInterval write fInterval;
    property RandomTime: LongInt read fRandom write fRandom;
    property ProcessMessages: Boolean read fProcessMessages write fProcessMessages default True;
    property OnWaitOver: TNotifyEvent read fOnWaitOver write fOnWaitOver;
  end;

{------------------------------------------------------------------------------}
{ TRetry                                                                       }
{------------------------------------------------------------------------------}

  TRetryExceptionEvent =
    procedure(Sender:TObject;E:Exception;var Action:TExceptionReAction) of object;

  TRetry = class(TPersistent)
  private
    fAskFirst: Boolean;   {should we ask to ok retries before retrying?}
    fAutoRetry: Boolean;  {if true we will do fMaxRetries before giving up}
    fAllowRetry: Boolean; {if the retry mechanism gives up, the user can allow more tries}
    fMaxRetries: Integer; {number of retries allowed before asking for feedback}
    fInterval: TInterval;
    fOnAction: TNotifyEvent;
    fCanIgnore: Boolean; {can an exception be ignored?}
    fErrorAction: TExceptionReAction; {defaults to reAsk}
    fOnException: TRetryExceptionEvent;
  protected
    procedure RetryException(Sender:TObject;E:Exception;var Action:TExceptionReAction);
    procedure DoOnException(Sender:TObject;E:Exception;var Action:TExceptionReAction); virtual;
  public
    constructor Create;
    constructor CreateAction(Action:TNotifyEvent;Exception:TRetryExceptionEvent);
    destructor Destroy; override;
    procedure Retry(Action:TNotifyEvent;Exception:TRetryExceptionEvent);
    procedure RetryAction(Action:TNotifyEvent);
    procedure Execute;
  published
    property Interval: TInterval read fInterval write fInterval;
    property RetryAuto: Boolean read fAutoRetry write fAutoRetry default True;
    property RetryMax:  Integer read FMaxRetries write FMaxRetries default 1;
    property RetryAllow: Boolean read fAllowRetry write fAllowRetry default True;
    property RetryAskFirst: Boolean read fAskFirst write fAskFirst default True;
    property CanIgnore: Boolean read fCanIgnore write fCanIgnore;
    property ErrorAction: TExceptionReAction read fErrorAction write fErrorAction;
    property OnAction: TNotifyEvent read fOnAction write fOnAction;
    property OnException: TRetryExceptionEvent read fOnException write fOnException;
    end;

{------------------------------------------------------------------------------}

//procedure Register;

implementation

uses UpdateOk;

{------------------------------------------------------------------------------}
{ TINTERVAL OBJECT                                                             }
{------------------------------------------------------------------------------}

constructor TInterval.Create;
begin {sleuth}
  inherited create;
  fProcessMessages:=True;
end;

constructor TInterval.CreateDelay(DelayMS:LongInt;WaitNow:Boolean);
begin
  inherited create;
  fProcessMessages:=True;
  if DelayMS>-1 then
    fInterval:=DelayMS;
  if WaitNow then
    Execute;
end;

constructor TInterval.CreateRandomDelay(ShortMS,LongMS:LongInt;WaitNow:Boolean);
begin
  inherited create;
  fProcessMessages:=True;
  LongintsLowHigh(ShortMS,LongMS);
  if ShortMS>-1 then
    fInterval:= ShortMS;
  if LongMS>-1 then
    fRandom:= (LongMS-ShortMS);
  if WaitNow then
    Execute;
end;

procedure TInterval.Execute;
begin
  if fInterval+fRandom>0 then
    {get timer}
    with TTimer.Create(nil) do
      try
        Enabled:=false;
        OnTimer:=TimerEvent;
        Interval:=fInterval+Random(fRandom);
        Enabled:=true;                  {Send the timer on it's way..}
        while Enabled do                {and wait till it turns itself off.}
          if fProcessMessages then      {could freeeeze right here! be careful}
            Application.ProcessMessages; {loop until time interval used up}
      finally
        Free;
        end;
end;

procedure TInterval.TimerEvent(Sender:TObject); {this is the timer event handler}
begin
  with TTimer(Sender) do
    Enabled:=false; {done with the delay}
  DoWaitOver(Sender);
end;

procedure TInterval.DoWaitOver(Sender:TObject); {this is the timer event handler}
begin
  if assigned(fOnWaitOver) then fOnWaitOver(Sender);
end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

constructor TRetry.Create;
begin {sleuth}
  inherited Create;
  fInterval:=TInterval.Create;
  fAutoRetry:=True;
  fMaxRetries:=1;
  fAllowRetry:=True;
  fAskFirst:=True;
end;

destructor TRetry.Destroy;
begin
  fInterval.Free;
  inherited Destroy;
end;

constructor TRetry.CreateAction(Action:TNotifyEvent;Exception:TRetryExceptionEvent);
begin
  Create;
  Retry(Action,Exception);
end;

procedure TRetry.Retry(Action:TNotifyEvent;Exception:TRetryExceptionEvent);
begin
  fOnAction:=Action;
  fOnException:=Exception;
  Execute;
end;

procedure TRetry.RetryAction(Action:TNotifyEvent);
begin
  fOnAction:=Action;
  Execute;
end;

procedure TRetry.Execute;
var
  Retries:Integer;
  Action:TExceptionReAction;
begin
  Retries:=0;
  while true do
    try
      if assigned(fOnAction) then
        fOnAction(Self);
      Break;
    except
    on E: Exception do begin
      Action:=fErrorAction;
      if fAskFirst and (Retries = 0) and (fMaxRetries>1) then
        RetryException(Self,E,Action);
      if Action in [reAsk,reRetry] then begin
        fInterval.Execute;
        Retries:=Retries+1;
        if (Retries >= fMaxRetries) then begin
          RetryException(Self,E,Action);
          Retries:=0;
          end;
        end;
      case Action of
      reIgnore: Break;
      reRetry: {continue};
      else
        Raise;
        end;
      end;
    else
      Raise;
    end;
end;

procedure TRetry.RetryException(Sender:TObject;E:Exception;var Action:TExceptionReAction);
var
  b:TMsgDlgButtons;
begin
  if (Action=reIgnore) and (not fCanIgnore) then {adjust parameters}
    Action:=reAsk;

  DoOnException(Self,E,Action);

  if Action=reAsk then begin                      {process error locally}
    if fCanIgnore then
      b:=mbAbortRetryIgnore
    else
      b:=mbOkCancel;
    case MessageDlg(E.ClassName+'!'+#13
                       +E.Message+#13+'Retry the operation?', mtError,b,0) of
      mrOk,
      mrRetry: Action:=reRetry;
      mrIgnore: Action:=reIgnore;
      else
        Action:=reRaise;
      end;
    end;
end;

{------------------------------------------------------------------------------}

procedure TRetry.DoOnException(Sender:TObject;E:Exception;var Action:TExceptionReAction);
begin
  if assigned(fOnException) then                  {call custom event}
    fOnException(Self,E,Action);
end;

//----------------------------------------------------------------------
//procedure Register;
//begin
//  RegisterPropertyEditor(TypeInfo(TRetry),nil,'', TtpSubPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TInterval),nil,'', TtpSubPropertyEditor);
//end;
//----------------------------------------------------------------------

end.

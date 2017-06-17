unit okcore.pas;        {components for floating 'cancel' box and running processes.}
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
  SysUtils, Messages, Classes, Graphics, Windows, Controls, Forms, Dialogs, 
  StdCtrls, 
  Retry, UpdateOk, tpAction;

type

{------------------------------------------------------------------------------}
{TOk defines the essential stopability that other components rely on.
It requests permission to change states and performs tries on on/off
it also can disable 'other' windows when running to make is seem modal.}

  TOk = class;

  TOkState      = (stsActive,stsCritical,stsReady,stsCanceled,stsDisabled);  {must improve!}

  TOkAware = class(TtpAction)
  private
    fEnabled:        Boolean;  {allow changes only if enabled}
    fActive:         Boolean;   {read only, true until done}
    fProcessMessages: Boolean;
  protected
    procedure SetActive(Flag:Boolean); override;
    function GetActive:Boolean;
  public
    constructor Create(AOwner:TComponent); override;
    procedure DoExecute; override;
  published
    property  Active: Boolean   read GetActive   write SetActive;
    property  Enabled: Boolean  read fEnabled  write fEnabled default true;
    property  ProcessMessages: Boolean read fProcessMessages write fProcessMessages default true;
  end;

{------------------------------------------------------------------------------}

  TOkDo = class;
  TOkDoResult = Longint;
  TOkDoEvent  = procedure(Sender: TOkDo;Var Success:Boolean) of object;
  TOkDoContinue =  procedure(Sender: TOkDo;Var CanContinue,Success:Boolean) of object;

  EOkAlreadyActive = class(Exception);
  EOkDoAlreadyActive = class(EOkAlreadyActive);

  TOkDo = class(TOkAware)
  private
    fOk:           TOk;
    fDoResult:     TOkDoResult;
    fOnDoInit:     TOkDoContinue;
    fOnDoAction:   TOkDoContinue;
    fOnDoExcept:   TOkDoEvent;
    fOnDoFinally:  TOkDoEvent;
    fOnDoDone:     TOkDoEvent;
  protected
  public
    constructor Create(AOwner:TComponent);                 Override;
    procedure   Run(Ok: TOk;Var Success: Boolean);         Virtual;
    procedure   DoInit(Var CanContinue,Success:Boolean);   Virtual;
    procedure   DoExcept(Var Success:Boolean);             Virtual;
    procedure   DoFinally(Var Success:Boolean);            Virtual;
    procedure   DoAction(Var CanContinue,Success:Boolean); Virtual;
    procedure   DoDone(Var Success: Boolean);              Virtual;
  published
    property DoResult:    TOkDoResult read fDoResult     write fDoResult;
    property OnDoInit:    TOkDoContinue read fOnDoInit   write fOnDoInit;
    property OnDoAction:  TOkDoContinue read fOnDoAction write fOnDoAction;
    property OnDoExcept:  TOkDoEvent  read fOnDoExcept   write fOnDoExcept;
    property OnDoFinally: TOkDoEvent  read fOnDoFinally  write fOnDoFinally;
    property OnDoDone:    TOkDoEvent  read fOnDoDone     write fOnDoDone;
  end;

{------------------------------------------------------------------------------}

  TOkOnOkStart  = procedure(Sender: TOk;Var CanStart:Boolean) of object;
  TOkOnOkStop   = procedure(Sender: TOk;Var CanStop:Boolean) of object;
  TOkOnOkChange = procedure(Sender: TOk;NewState:TOkState;Var CanChange:Boolean) of object;

  TOk = class(TOkAware)
    {While this is a component, its purpose is the most obscure. TOk
    remains completely invisible but has the ability to call begin/end procs
    which make it simple for you to hook up changes to captions or do whatever
    you need to keep the user happy while running your Ok action. To use,
    set Active:=True when beginning your loop, then check 'Active' or 'Stop'
    while looping to Process Messages and exit properly. Your Cancel button can
    signal a normal or cancel outcome by setting Active:=False or Canceled:=True.
    Deactivating in either ways can be denied by the OnOkStop procedure.}
    {you could call this component from a button like this:
      Ok1.Active:=not Ok1.Active;
      if Ok1.Active then
        Button1.Caption:='Running'
      else
        Button1.Caption:='Stopped';
    }
  private
    { Private declarations }
    fCritical:       Boolean; {disable OkBox button, on hold}
    fCanceled:       Boolean; {Ok canceled on last try}
    fFrozen:         Boolean;   {other forms disabled while true}
    fFreeze:         Boolean;   {disable other forms while true}
    fOkStart:        TOkDo;
    fOkStop:         TOkDo;
    fOnOkStart:      TOkOnOkStart;{OnOkStart proc to ok-ok. eg. oks ative=true}
    fOnOkChange:     TOkOnOkChange;{ok-ok. eg. oks ative=true}
    fOnOkStop:       TOkOnOkStop;  {ok-ok. eg. oks ative=false}
  protected
    { Protected declarations }
    procedure   SetEnabled(Flag:Boolean);            virtual;
    procedure   SetActive(Flag:Boolean);             override;
    procedure   SetStop(Flag:Boolean);               virtual;
    procedure   SetCritical(Flag:Boolean);           virtual;
    procedure   SetCanceled(Flag:Boolean);           virtual;
    procedure   SetState(State:TOkState);            virtual;
    procedure   SetFrozen(Flag:Boolean;ButNot:HWND); virtual;
    function    GetStop:Boolean;
    function    GetState:TOkState;
    function    GetStringState:String;
    procedure   DoOkStart(Var CanStart:Boolean);                     virtual;
    procedure   DoOkChange(NewState:TOkState;Var CanChange:Boolean); virtual;
    procedure   DoOkStop(Var CanStop:Boolean);                       virtual;
    function    FreezeFormHandle:HWND;               virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);           override;
    procedure   Run(Sender:TObject;Var Success: Boolean); virtual;
    procedure   OkOn;                                virtual;
    procedure   OkOff;                               virtual;
    procedure   Reset;                               virtual;
    procedure   Notification(AComponent: TComponent; Operation: TOperation);          Override;
    function    BenchmarkLoopsPerSecond:LongInt;
    function    StringState(State:TOkState):String;
  published
    property Enabled: Boolean            read fEnabled  write SetEnabled default true;
{    property Active: Boolean             read fActive   write SetActive;}
    {note if you disable, only enabled and state will change. you can not
    disable the control in a critical section.}
    property Ok: Boolean                 read fActive   write SetActive; {ALIAS}
    property Stop: Boolean               read GetStop   write SetStop default true;
    property Critical: Boolean           read fCritical write SetCritical;
    property Canceled: Boolean           read fCanceled write SetCanceled;
    property State: TOkState             read GetState  write SetState default stsReady;
    property StateString: String         read GetStringState;
    property FreezeForms: Boolean        read fFreeze   write fFreeze;
    property OkStart: TOkDo              read fOkStart  write fOkStart;
    property OkStop: TOkDo               read fOkStop   write fOkStop;
    property OnOkStart: TOkOnOkStart     read fOnOkStart write fOnOkStart;
    property OnOkChange: TOkOnOkChange   read fOnOkChange write fOnOkChange;
    property OnOkStop: TOkOnOkStop       read fOnOkStop write fOnOkStop;
  end;

{------------------------------------------------------------------------------}
  TOkDoDelay = class(TOKDo)
  private
    fInterval: TInterval;
  protected
  public
    constructor Create(AOwner:TComponent);                  Override;
    destructor  Destroy;                                    Override;
    procedure   DoAction(Var CanContinue,Success:Boolean);  Override;
  published
    property Interval: TInterval read fInterval write fInterval;
  end;

{------------------------------------------------------------------------------}

  TOkBranch = class;
  TOkBranchResult = Longint;
  TOkBranchEvent  = procedure(Sender: TOkBranch;Var Success:Boolean) of object;
  TOkBranchContinue =  procedure(Sender: TOkBranch;Var CanContinue,Success:Boolean) of object;

  TOkBranch = class(TPersistent)
  private
    fOnBranch:     TOkBranchEvent;
    fBranchSuccess:    TOkDo;
    fBranchFailure:    TOkDo;
  protected
  public
    procedure Run(Ok: TOk;Var Success: Boolean);
    procedure DoDone(Var Success: Boolean);                Virtual;
    procedure DoSuccess(Ok: TOk;Var Success: Boolean); Virtual;
    procedure DoFailure(Ok: TOk;Var Success: Boolean); Virtual;
  published
    property BranchSuccess: TOkDo    read fBranchSuccess write fBranchSuccess;
    property BranchFailure: TOkDo    read fBranchFailure write fBranchFailure;
    property OnBranch:      TOkBranchEvent read fOnBranch   write fOnBranch;
  end;

{------------------------------------------------------------------------------}
  TOkDoBranch = class(TOkDo)
  private
    fBranch:    TOkBranch;
  protected
  public
    constructor Create(AOwner:TComponent);       Override;
    destructor  Destroy;                         Override;
    procedure   DoDone(Var Success: Boolean);    Override;
  published
    property    Branch:  TOkBranch  read fBranch write fBranch;
  end;

{------------------------------------------------------------------------------}

  TOkTry = class;
  TOkTryResult = Longint;
  TOkTryEvent  = procedure(Sender: TOkTry;Var Success:Boolean) of object;
  TOkTryTry    = procedure(Sender: TOkTry; Var CanRetry:Boolean) of object;

  EOkTryAlreadyActive = class(EOkAlreadyActive);

  TOkTry = class(TOkDoBranch)
  private
    fTryResult:     TOkTryResult;
    fTryProcessMessages: Boolean;
    fTryOkRetry:       Boolean;
    fTryRetryCount:    LongInt;
    fTryRetryInterval: TInterval;
    fTryMaxRetryCount: LongInt;
  protected
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);         Override;
    destructor  Destroy;                           Override;
    procedure   Run(Ok: TOk;Var Success: Boolean); Override;
{    property Critical;}
  published
    { Published declarations }
{    property Active:    Boolean     read fActive       write SetActive;}
    property TryResult:     TOkTryResult read fTryResult    write fTryResult;
    property TryProcessMessages: Boolean read fTryProcessMessages write fTryProcessMessages default true;
    property TryOkRetry:       Boolean   read fTryOkRetry   write fTryOkRetry;
    property TryRetryCount:    LongInt   read fTryRetryCount;
    property TryRetryInterval: TInterval read fTryRetryInterval write fTryRetryInterval;
    property TryMaxRetryCount: LongInt   read fTryMaxRetryCount write fTryMaxRetryCount default 1;
  end;

{------------------------------------------------------------------------------}
  TOkLoop = class;
  TOkLoopResult = Longint;
  TOkLoopArrive =  procedure(Sender: TOkLoop;Var CanArrive,CanContinue,Success:Boolean) of object;
  TOkLoopLoop =  procedure(Sender: TOkLoop;Var CanContinue,Success:Boolean) of object;
  TOkLoopEvent = procedure(Sender: TOkLoop;Var Success:Boolean) of object;

  EOkLoopAlreadyActive = class(EOkTryAlreadyActive);

  TOkLoop = class(tOkTry)
  private
    { Private declarations }
    fLoopResult:     TOkLoopResult;
    fLoopBranch:     TOkBranch;
    fLoopCount:      LongInt;
    fLoopMaxCount:   LongInt;
    fLoopProcessMessages: Boolean;
    fLoopInterval:   TInterval;
    fOnLoopInit:     TOkLoopLoop;
    fOnLoopArrive:   TOkLoopArrive;
    fOnLoopNext:     TOkLoopLoop;
    fOnLoopExcept:   TOkLoopEvent;
    fOnLoopFinally:  TOkLoopEvent;
    fOnLoopDone:     TOkLoopEvent;
  protected
    { Protected declarations }
    procedure   SetLoopMaxCount(Value:Longint);                               Virtual;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);                                    Override;
    destructor  Destroy;                                                      Override;
    procedure   Run(Ok: TOk;Var Success: Boolean);                            Override;
    procedure   DoLoopInit(Var CanContinue,Success:Boolean); Virtual;
    procedure   DoLoopArrive(Var CanArrive,CanContinue,Success:Boolean); Virtual;
    procedure   DoLoopNext(Var CanContinue,Success:Boolean); Virtual;
    procedure   DoLoopFinally(Var Success:Boolean);          Virtual;
    procedure   DoLoopExcept(Var Success:Boolean);           Virtual;
    procedure   DoLoopDone(Var Success:Boolean);             Virtual;
  published
    { Published declarations }
    property LoopResult:    TOkLoopResult read fLoopResult    write fLoopResult;
    property LoopProcessMessages: Boolean read fLoopProcessMessages write fLoopProcessMessages default true;
    property LoopCount:     LongInt       read fLoopCount;
    property LoopMaxCount:  LongInt       read fLoopMaxCount  write SetLoopMaxCount default 1;
    property LoopInterval:  TInterval     read fLoopInterval  write fLoopInterval;
    property LoopBranch:    TOkBranch     read fLoopBranch    write fLoopBranch;
    property OnLoopInit:    TOkLoopLoop   read fOnLoopInit    write fOnLoopInit;
    property OnLoopArrive:  TOkLoopArrive read fOnLoopArrive  write fOnLoopArrive;
    property OnLoopNext:    TOkLoopLoop   read fOnLoopNext    write fOnLoopNext;
    property OnLoopExcept:  TOkLoopEvent  read fOnLoopExcept  write fOnLoopExcept;
    property OnLoopFinally: TOkLoopEvent  read fOnLoopFinally write fOnLoopFinally;
    property OnLoopDone:    TOkLoopEvent  read fOnLoopDone    write fOnLoopDone;
  end;

{-------------------------------------------------------------------------}

{$IFDEF ALL_TPACK}
//procedure Register;
{$ENDIF}

implementation

{-------------------------------------------------------------------------}

constructor TOkAware.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  fEnabled:=true;
  fProcessMessages:=true;
end;

procedure TOkAware.SetActive(Flag:Boolean);
begin
  if fActive<>Flag then
    fActive:=Flag;
end;

function TOkAware.GetActive:Boolean;
begin
  if fProcessMessages then
    Application.ProcessMessages;
  Result:=fActive;
end;

procedure TOkAware.DoExecute;
begin
  Active:=True;
  inherited DoExecute;
end;

{------------------------------------------------------------------------------}
{Let's begin.. here goes the 'root' component, e.g.  the Ok capability.}

constructor TOk.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  fEnabled:=True;
end;

procedure TOk.Reset;
{Unconditially resets the component and puts it in ready mode. use at own risk.
{this methd allows you to stop a OkTry regardless of the callback method's
opinion. we simply take it out, shut off and put it back. it never knows.}
var
  e:TOkOnOkStop;
begin
  e:=fOnOkStop;
  fOnOkStop:=nil;
  State:=stsReady;
  {note: this is the only time we actually resort to changing the component's
  state. usually we just manipulate the flags directly but here we want to take
  advantage of the override logic in SetState.}
  {you might think we should make the status canceled if we shut down a loop.
  that wouldn't be right either because we really 'excepted' out of the OkTry.}
  fOnOkStop:=e;
end;

procedure TOk.Run(Sender:TObject;Var Success: Boolean);
begin
  SetActive(True);
  SetActive(False);
end;

procedure TOk.OkOn;
begin
  SetActive(True);
end;

procedure TOk.OkOff;
begin
  SetActive(False);
end;

function TOk.BenchmarkLoopsPersecond:Longint;
begin
  result:=-1;
  {instantiate timer w/proc to signal end (could use another ok)
  then count how often we can turn ok on/off inside that time.}
end;

procedure TOk.SetEnabled(Flag:Boolean);
begin
  if Flag<>fEnabled then begin
    if (not Flag) and fActive and fCritical then {can not stop in a critical section!}
      Exit;
    if fActive and (Flag=false) then
      Active:=False; {turn off. OnOkStop may deny.}
   {implement okchange!}
    fEnabled:=fActive or Flag;
    end;
end;

procedure TOk.SetCanceled(Flag:Boolean);
begin
  if fEnabled and Flag<>fCanceled then begin
    if flag then       {do not activate when resetting flag}
      SetStop(Flag);
    if Flag<>fCanceled then begin
      DoOkChange(stsCanceled,Flag);
      fCanceled:=Flag;
      end;
    end;
end;

procedure TOk.SetStop(Flag:Boolean);
begin
  Active:=not Flag;
end;

function TOk.GetStop:Boolean;
begin
  Result:=not Active;
end;

procedure TOk.SetActive(Flag:Boolean);
var
  Close: Boolean;
begin
  if fEnabled and Flag<>fActive then begin
    if Flag then begin
      if fActive then
        raise EOkAlreadyActive.Create('TOk: Already Active');
      fCanceled:=False;
      DoOkChange(stsActive,Flag);
      if flag then
        DoOkStart(Flag);
      factive:=false;
      if not flag then
        exit;
      end
    else begin
      if fActive and fCritical then {can not stop in a critical section!}
        Exit;
      Close:= true;
      DoOkChange(stsReady,close);
      if Close then
        DoOkStop(close);
      factive:=true;
      if not Close then
        exit;
      end;
    if flag<>fActive then begin
      fActive:=Flag;
      SetFrozen(fActive and fFreeze,FreezeFormHandle);
      end;
    end;
end;

procedure TOk.SetCritical(Flag:Boolean);
{OkTry can not be stopped when in a critical section}
{it can start in 'critical' mode where it can not be stopped-
however the component can not be enabled without resetting critical to neutral,
note that 'enabling' is not 'activating'. you can go from ready mode to critical,
just going from disabled to critical is not possible. makes sense?}
begin
  if fEnabled and Flag<>fCritical then
    fCritical:={fActive and} Flag;
end;

procedure TOk.SetState(State:TOkState);
{by setting the state, you get a shortcut way to change the properties
you want to change. REMEMBER!: CRITICAL=TRUE forces the box to stay on,
ENABLE=FALSE forces it to stay off. no matter how often you try, these
properties will block you from changing others. In critical sections the
OnOkStop procedure is never called.}
begin
  case State of
    stsActive:   if fCritical and fActive then
                   Critical:= False {transit back from critical to active}
                 else
                   Active:=   True;
    stsCritical: if fActive then
                   Critical:= True;
    stsReady:    begin
                 if fEnabled=false then
                   fEnabled:=True;
                 if fCritical then
                   fCritical:=False;
                 Canceled:=False;
                 end;
    stsCanceled: Canceled:= True;
    stsDisabled: Enabled:=  False;
    end;
end;

function TOk.GetState:TOkState;
{you definitely must play with this component in the object inspector before
using it. the CRITICAL/ENABLED flags must be understood to be useful. the 'State'
property should make the logic clearer.}
begin
  if not fEnabled then
    Result:=stsDisabled
  else
    if fCanceled then
      Result:=stsCanceled
    else
      if not fActive then
        Result:=stsReady
      else
        if fCritical then
          Result:=stsCritical
        else
          Result:=stsActive;
  if fProcessMessages then
    Application.ProcessMessages;
end;

function TOk.GetStringState:String;
begin
  result:=StringState(State);
end;

function TOk.StringState(State:TOkState):String;
begin
  case State of
  stsActive:      Result:='Active';
  stsCritical:    Result:='Critical';
  stsReady:       Result:='Ready';
  stsCanceled:    Result:='Canceled';
  stsDisabled:    Result:='Disabled';
  end;
end;

procedure TOk.SetFrozen(Flag:Boolean;ButNot:HWND);
var
  i:longint;
begin
  if Flag<>fFrozen then begin
    fFrozen:=Flag;
    for i:=0 to Screen.FormCount-1 do
      if ButNot <> Screen.Forms[i].Handle then
        with Screen.Forms[i] do
          Enabled := not Enabled;
    end;
end;

function TOk.FreezeFormHandle:HWND;
{the purpose of this function is to be replaced by a descendant in case the
usual choice of forms to be unfrozen is not right, and frankly, to allow us
to focus either on the derived OkBox or on the currently active form}
begin
  result:=Screen.ActiveForm.Handle;
end;

{}

procedure TOk.DoOkStart(Var CanStart:Boolean);
begin
  if assigned(fOnOkStart) then fOnOkStart(Self,CanStart);
  if CanStart then begin
    fActive:=CanStart;
    if fOkStart<>nil then
      if fOkStart is TOkDo then
        tOkDo(fOkStart).run(Self,CanStart);
    fActive:=CanStart;
    end;
end;

procedure TOk.DoOkChange(NewState:TOkState;Var CanChange:Boolean);
begin
  if assigned(fOnOkChange) then fOnOkChange(Self,NewState,CanChange);
end;

procedure TOk.DoOkStop(Var CanStop:Boolean);
begin
  if assigned(fOnOkStop) then
    fOnOkStop(Self,CanStop);
  if CanStop then
    if fOkStop<>nil then
       fOkStop.run(Self,CanStop);
end;

procedure TOk.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csUpdating in ComponentState) then
    exit;
  if (Operation = opRemove) then begin
    if (fOkStart<> Nil) and (AComponent = fOkStart) then fOkStart:= nil;
    if (fOkStop<> Nil) and (AComponent = fOkStop) then fOkStop:= nil;
    end;
end;

{------------------------------------------------------------------------------}
{TOkDo implements a start/do/end handler}

constructor TOkDo.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
end;

procedure TOkDo.Run(Ok: TOk;Var Success: Boolean);
var
  CanContinue:Boolean;
begin
  fOk:=Ok;
  CanContinue:=true;
  if not Ok.active then
    Exit;
  fDoResult:=0;
  try
    try
      DoInit(CanContinue,Success);
      if CanContinue then
        DoAction(CanContinue,Success);
    except
      DoExcept(Success);
    end;
    DoFinally(Success);
  finally
    DoDone(Success);
    end;
end;

procedure TOkDo.DoInit(Var CanContinue,Success: Boolean);
begin
  If assigned(fOnDoInit) then fOnDoInit(Self,CanContinue,Success);
end;

procedure TOkDo.DoAction(Var CanContinue,Success:Boolean);
begin
  If assigned(fOnDoAction) then fOnDoAction(Self,CanContinue,Success);
end;

procedure TOkDo.DoExcept(Var Success:Boolean);
begin
  If assigned(fOnDoExcept) then fOnDoExcept(Self,Success);
end;

procedure TOkDo.DoFinally(Var Success:Boolean);
begin
  If assigned(fOnDoFinally) then fOnDoFinally(Self,Success);
end;

procedure TOkDo.DoDone(Var Success:Boolean);
begin
  If assigned(fOnDoDone) then fOnDoDone(Self,Success);
end;

{------------------------------------------------------------------------------}
{TOkBranch implements a branch handling capability} {NOTE, tpersistant}

procedure TOkBranch.Run(Ok: TOk;Var Success: Boolean);
begin
  DoDone(Success);
  if Success then
    DoSuccess(Ok,Success)
  else
    DoFailure(Ok,Success);
end;

procedure TOkBranch.DoDone(Var Success: Boolean);
begin
  If assigned(fOnBranch) then fOnBranch(self, Success);
end;

procedure TOkBranch.DoSuccess(Ok: TOk;Var Success: Boolean);
begin
  If fBranchSuccess<>nil then fBranchSuccess.Run(ok, Success);
end;

procedure TOkBranch.DoFailure(Ok: TOk;Var Success: Boolean);
begin
  If fBranchFailure<>nil then fBranchFailure.Run(ok, Success);
end;

{------------------------------------------------------------------------------}
{TOkDoBranch adds branching to after finally/except based on success of action}

constructor TOkDoBranch.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  fBranch:=TOkBranch.Create;
end;

destructor TOkDoBranch.Destroy;
begin
  fBranch.Destroy;
  inherited Destroy;
end;

procedure TOkDoBranch.DoDone(Var Success: Boolean);
begin
  inherited DoDone(Success);
  If fBranch<>nil then fBranch.Run(fOk, Success);
end;

{------------------------------------------------------------------------------}
{TOkDoDelay implements a wait in between init and finally}

constructor TOkDoDelay.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  fInterval:=TInterval.Create;
  fEnabled:=True;
end;

destructor TOkDoDelay.Destroy;
begin
  fInterval.Destroy;
  inherited Destroy;
end;

procedure TOkDoDelay.DoAction(Var CanContinue,Success:Boolean);
begin
  if fInterval<>nil then
    fInterval.Execute;
  inherited DoAction(CanContinue,Success);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{TOkTry adds a retry element}

constructor TOkTry.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  fTryRetryInterval:=TInterval.Create;
  fTryProcessMessages:=True;
  fTryMaxRetryCount:=1;
end;

destructor TOkTry.Destroy;
begin
  fTryRetryInterval.Destroy;
  inherited Destroy;
end;

procedure TOkTry.Run(Ok: TOk;Var Success: Boolean);
var
  CanContinue:Boolean;
begin
  if not Ok.active then
    Exit;
  fOk:=Ok;
  fDoResult:=0;
  fTryResult:=0;
  fTryRetryCount:=0;
  CanContinue:=True;
  try try
    DoInit(CanContinue,Success);
    while Ok.active and (fTryRetryCount<=fTryMaxRetryCount) do begin {try at least once}
      if fTryProcessMessages then
        Application.ProcessMessages;
      if Ok.Active and CanContinue then
        DoAction(CanContinue,Success);
      if Ok.Active
      and (fTryRetryCount<fTryMaxRetryCount)
      and CanContinue and (not Success)          {< CANCONTINUE AND NOT SUCCESS=RETRY}
      then begin
        fTryRetryInterval.Execute;
        inc(fTryRetryCount);
        end
      else
        Break;
      end;
  except
    DoExcept(Success);
    end;
    DoFinally(Success);
  finally
    DoDone(Success);
    end;
end;

{------------------------------------------------------------------------------}

constructor TOkLoop.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  fLoopInterval:=TInterval.Create;
  fLoopProcessMessages:=True;
  fLoopMaxCount:=1;
  fLoopBranch:=TOkBranch.Create;
end;

destructor TOkLoop.Destroy;
begin
  fLoopBranch.Destroy;
  inherited Destroy;
end;

procedure TOkLoop.SetLoopMaxCount(Value:Longint);
begin
  if Value<0 then {allow 0}
    Value:=1
  else
    if Value=0 then
      Value:=MaxLongInt;
  fLoopMaxCount:=Value;
end;


procedure TOkLoop.Run(Ok: TOk;Var Success: Boolean);
var
  CanArrive,CanContinue:Boolean;
begin
  if not Ok.active then
    exit;
  fOk:=Ok;
  CanContinue:=True;
  fLoopResult:=0;
  fLoopCount:=0;
  try try
    DoLoopInit(CanContinue,Success);
    while CanContinue and Ok.active and (fLoopCount<fLoopMaxCount) do begin {more to do!}
      if fLoopProcessMessages then
        Application.ProcessMessages;
      if Ok.active then begin
        CanArrive:=True;
        DoLoopArrive(CanArrive,CanContinue,Success);
        if CanArrive then
          inherited run(ok,Success);  {TADA! the point of the loop is to repeatedly try the action}
        end;
      if Ok.Active and (fLoopCount<fLoopMaxCount-1) then
        DoLoopNext(CanContinue,Success);
      if Ok.Active and CanContinue then begin
        fLoopInterval.Execute;
        inc(fLoopCount);
        end;
      end;
  except
    DoLoopExcept(Success);
    end;
    DoLoopFinally(Success);
  finally
    DoLoopDone(Success);
    end;
end;

procedure TOkLoop.DoLoopInit(Var CanContinue,Success:Boolean);
begin
  If assigned(fOnLoopInit) then fOnLoopInit(Self,CanContinue,Success);
end;

procedure TOkLoop.DoLoopArrive(Var CanArrive,CanContinue,Success:Boolean);
begin
  If assigned(fOnLoopArrive) then fOnLoopArrive(Self,CanArrive,CanContinue,Success);
end;

procedure TOkLoop.DoLoopNext(Var CanContinue,Success:Boolean);
begin
  if assigned(fOnLoopNext) then fOnLoopNext(Self,CanContinue,Success);
end;

procedure TOkLoop.DoLoopFinally(Var Success:Boolean);
begin
  If assigned(fOnLoopFinally) then fOnLoopFinally(Self,Success);
end;

procedure TOkLoop.DoLoopExcept(Var Success:Boolean);
begin
  If assigned(fOnLoopExcept) then fOnLoopExcept(Self,Success);
end;

procedure TOkLoop.DoLoopDone(Var Success: Boolean);
begin
  If fLoopBranch<>nil then
   fLoopBranch.Run(fOk,Success);
end;


{----------------------------------------------------------------------------------------}

{$IFDEF ALL_TPACK}
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TOkAware,TOkDo,TOk,TOkDoDelay,TOkDoBranch,TOkTry,TOkLoop]);
//end;
{$ENDIF}

{----------------------------------------------------------------------------------------}
(* You may need this:

type

  TOkDoBranchProperty = class(TClassProperty)
  public
    procedure Edit; Override;
    function GetAttributes: TPropertyAttriutes; Override;
  end;

{----------------------------------------------------------------------------------------}


function TOkDoBranchProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubproperties];
end;

procedure TOkDoBranchProperty.Edit;
begin
  messagebeep($FFFF);
end;


*)

end.


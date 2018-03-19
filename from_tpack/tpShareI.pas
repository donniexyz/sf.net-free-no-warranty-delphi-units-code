unit tpShareI;

(*
Permission is hereby granted, on 18-Jul-2017, free of charge, to any person
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

{$I hrefdefines.inc}
{$IFNDEF MSWINDOWS} !! requires Windows API !! {$ENDIF}

uses
  Windows,
  utIPCObj, //TtpEvent, TSharedMem
  // tpSyncObjs also has a TSharedMem but that one does not have a Buffer
  tpIntegerTypes, Classes;

type
  TtpAltSleeperThreadEvent = procedure(Sender: TObject)
    of object;

  TtpAltSleeperThread = class(TThread)
  private
    fEvent: TtpEvent;
    fOnSignal: TtpAltSleeperThreadEvent;
    fSynchronize, fIgnore, fIgnoreOwnThreadChanges, fActive: Boolean;
  public
    constructor Create(const aName: String; SignalEvent: TtpAltSleeperThreadEvent;
      psa: PSecurityAttributes);
    destructor Destroy; override;
    //
    procedure Execute; override; // run the loop
    procedure Triggered; // the event is triggered
    procedure Syncronized; // synchronize the event
    //
    procedure Signal; // trigger all other listeners
    //
    property Active: Boolean read fActive write fActive;
    property Ignore: Boolean read fIgnore write fIgnore;
    property IgnoreOwnChanges: Boolean read fIgnoreOwnThreadChanges
      write fIgnoreOwnThreadChanges;
    property Syncronize: Boolean read fSynchronize write fSynchronize;
    property Event: TtpEvent read fEvent;
  end;

type
{$IFDEF Delphi16UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}

  TtpSharedInt32 = class(TComponent)
  private
    //
    fIgnoreOwnChanges: Boolean;
    fGlobalName: String;
    fLastValue: Integer;
    fLocalValue: Integer;

    fSharedMem: utIPCObj.TSharedMem;  // does not compile with tpSyncObjs
    fMutex: TMutex;

    fSleeper: TtpAltSleeperThread;
    fOnChange: TtpAltSleeperThreadEvent;
    procedure Signaled(Sender: TObject);
    procedure SetValue(aValue: Int32);
    procedure SetGlobalName(const Value: String);
    procedure SetIgnoreOwnChanges(Value: Boolean);
    function GetValue: Int32;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseSharedMemory;
    //
    property Mutex: TMutex read fMutex;
    property SharedMem: TSharedMem read fSharedMem;
    property Sleeper: TtpAltSleeperThread read fSleeper;
    property LastValue: Integer read fLastValue;
  published
    property GlobalName: String read fGlobalName write SetGlobalName;
    property GlobalInteger: Int32 read GetValue write SetValue;
    // Any value except MaxLongInt is okay to use.
    property IgnoreOwnChanges: Boolean read fIgnoreOwnChanges
      write SetIgnoreOwnChanges;
    property OnChange: TtpAltSleeperThreadEvent read fOnChange write fOnChange;
  end;

  TtpSharedLongint = class(TtpSharedInt32);

implementation

uses
{$IFNDEF RUNNOLOG}Forms,
{$ENDIF} {we do not really want the Forms unit linked into the runner}
{$IFDEF MSWINDOWS}ucWinSecurityObj,{$ENDIF} // gsa
  SysUtils;

// ------------------------------------------------------------------------------

constructor TtpAltSleeperThread.Create(const aName: String;
  SignalEvent: TtpAltSleeperThreadEvent; psa: PSecurityAttributes);
begin
  FreeOnTerminate := True;
  fEvent := TtpEvent.Create;
  with fEvent do
  begin
    Security := psa;
    Name := aName;
    ManualReset := True;
    Handle;
  end;
  fActive := True;
  fOnSignal := SignalEvent;
  inherited Create(True);
end;


destructor TtpAltSleeperThread.Destroy;
begin
  FreeAndNil(fEvent);
  inherited Destroy;
end;

procedure TtpAltSleeperThread.Signal;
begin
  fEvent.Signal;
end;

procedure TtpAltSleeperThread.Execute;
begin
  with fEvent do
    repeat
      Reset; // NB: Reset is only relevant when the TtpEvent is opened with ManualReset
      Wait(Infinite); // wait for the event to go off
      if (NOT TThread.CheckTerminated) then
      begin
        Triggered;
      end;
    until (TThread.CheckTerminated) or (not fActive);
end;

procedure TtpAltSleeperThread.Triggered;
begin
  if fSynchronize then
    Synchronize(Syncronized)
  else
    Syncronized;
  Sleep(0); // let the next thread run and react to the event
end;

procedure TtpAltSleeperThread.Syncronized;
begin
  if assigned(fOnSignal) then
    fOnSignal(self);
end;

// ------------------------------------------------------------------------------

constructor TtpSharedInt32.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  IgnoreOwnChanges := True;
  fLocalValue := MaxLongInt;
end;

destructor TtpSharedInt32.Destroy;
begin
  CloseSharedMemory;
  inherited Destroy;
end;

procedure TtpSharedInt32.Signaled(Sender: TObject);
var
  i, j: Integer;
  b: Boolean;
begin
{$IFNDEF RUNNOLOG}
  if Application.Terminated then
    Exit;
{$ENDIF}
  b := True;
  if assigned(fOnChange) and assigned(fMutex) and assigned(fSharedMem) then
    while b do
    begin
      with fMutex do
        try
          Get(Infinite);
          i := Int32(fSharedMem.Buffer^);
          fLastValue := i;
        finally
          Release;
        end;
      if (fLocalValue <> MaxLongInt) then
      begin
        if NOT fIgnoreOwnChanges then
          fOnChange(self);
        fLocalValue := MaxLongInt;
      end
      else
      begin
        fOnChange(self);
      end;
      //if not Continue then
      //  Exit;
      with fMutex do
        try
          Get(Infinite);
          j := Int32(fSharedMem.Buffer^);
        finally
          Release;
        end;
      b := i <> j;
    end
end;

procedure TtpSharedInt32.CloseSharedMemory;
begin
  if assigned(fSleeper) then
  begin
    if (NOT fSleeper.Terminated) then
    begin
      if assigned(fSleeper.Event) then
      begin
        fSleeper.Event.Signal;
        Sleep(0);
      end;
      fSleeper.Terminate;
      // Do not fSleeper.WaitFor here. In D6,D7 that leads to EOS error 6.
    end;
    // Do not need to fSleeper.Free here because FreeOnTerminate = True
    fSleeper := nil;
  end;
  // Do NOT Release fMutex here... in runisa.dll, that crashes Sambar web server on first use.
  FreeAndNil(fMutex);
  FreeAndNil(fSharedMem);
end;

procedure TtpSharedInt32.SetIgnoreOwnChanges(Value: Boolean);
begin
  fIgnoreOwnChanges := Value;
  if assigned(Sleeper) then
    Sleeper.IgnoreOwnChanges := Value;
end;

procedure TtpSharedInt32.SetGlobalName(const Value: String);
begin
  if assigned(fSharedMem) and (CompareText(fSharedMem.Name, Value) = 0) then
    Exit;
  CloseSharedMemory;
  fGlobalName := Value;
  try
    fMutex := TMutex.Create; // Name will be (Value+'M');
    with fMutex do
    begin
      Security := @gsa;
      Name := Value + 'M';
      Handle;
    end;
  except
    on e: exception do
      raise exception.Create(e.message + sLineBreak + ParamStr(0) + sLineBreak +
        'TtpSharedInt32.SetGlobalName(' + Value + ')');
  end;
  fSleeper := TtpAltSleeperThread.Create(Value + 'T', Signaled, @gsa); // @fsa
  with fSleeper do
  begin
    Syncronize := self.Owner <> nil;
    IgnoreOwnChanges := self.IgnoreOwnChanges;
{$IFDEF Delphi15UP}
    Start;
{$ELSE}
    Resume; // before D15
{$ENDIF}
    Sleep(0); // start executing the thread
  end;
  fSharedMem := utIPCObj.TSharedMem.Create;
  with fSharedMem do
  begin
    Security := @gsa;
    Name := Value;
    Size := SizeOf(Int32);
    Handle;
    if Creator then
      GlobalInteger := 0;
  end;
end;

procedure TtpSharedInt32.SetValue(aValue: Int32);
begin
  if assigned(fMutex) then
    with fMutex do
      try
        Get(Infinite);
        // Set value, even if it is the same as last time, so that one can,
        // for example, refresh pwebapp multiple times in a row using the
        // same triggering integer value.
        Int32(fSharedMem.Buffer^) := aValue;
        fLastValue := aValue;
        fLocalValue := aValue;
        fSleeper.Signal;
      finally
        Release;
      end;
end;

function TtpSharedInt32.GetValue: Int32;
begin
  Result := 0;
  if (self <> nil) then
  begin
    if assigned(fMutex) then
    begin
      with fMutex do
        try
          Get(Infinite);
          if fSharedMem.Buffer <> nil then  // can be nil during DUnitX cross-thread
            Result := Int32(fSharedMem.Buffer^)
          else
        finally
          Release;
        end;
    end;
    fLastValue := Result;
  end;
end;

end.

unit tpShell;
(*
Permission is hereby granted, on 3-Aug-2005, free of charge, to any person
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

{
// TtpProcess:                 Base class used by the dos/windows/dll shell components
// TWindowsShell:              Component-based WinExec call.
// TDLLShell:                  Provides assistance with calling procs in a DLL.
}

{$I hrefdefines.inc}
{
  Master copy of hrefdefines.inc is versioned on Source Forge in the ZaphodsMap project:
  https://sourceforge.net/p/zaphodsmap/code/HEAD/tree/trunk/ZaphodsMap/Source/hrefdefines.inc
}

uses
{$IFDEF MSWINDOWS}
  Windows, Forms, Controls, Messages,
{$ENDIF}
{$IFDEF LINUX}
// Kylix  QForms, QControls,
{$ENDIF}
  Classes, SysUtils, 
  updateOK, tpAction, ucWinSecurityObj, ucString;

Const
  DefaultProcessor = 'COMMAND.COM';
Const
  ShowCommands:array[TWindowState] of word = (SW_SHOWNORMAL,SW_SHOWMINIMIZED,SW_SHOWMAXIMIZED);

Type
  TShellOptions = (shlWaitTillIdle,shlWaitTillDone,shlUseShell,shlMsgTillReady,shlMsgTillDone);
  TShellFlags = set of TShellOptions;

  TTpProcess = class(TtpAction)
  private
    fFlags                : TShellFlags;
    fShellResult          : DWORD;
    fOnShelled            : TNotifyEvent;
    fOnWait               : TNotifyEvent;
    // fWorking              : TWorkingMsg; {See Note 1 at end of file}
  protected
    fCommand              : String;
    fCommandLine          : String;
    function DoShell: DWORD; Virtual;
    procedure DoOnShell; Virtual;
    procedure SetCommand(const Value:String); virtual;
    procedure SetCommandLine(const Value:String);
  public
    constructor Create(AOwner: TComponent); override;
    // procedure Notification(AComponent: TComponent; Operation: TOperation); Override; {Note 1}
    procedure DoExecute; override;
    procedure Run(const aCmd,aParam:String);
    property Command : String read fCommand write SetCommand;
    property Parameters : String read fCommandLine write SetCommandLine;
  published
    // property Working     : TWorkingMsg read fWorking write fWorking; {Note 1}
    property Flags       : TShellFlags read fFlags write fFlags;
    property ShellResult : DWORD read fShellResult write fShellResult stored false;
    property OnShelled   : TNotifyEvent read fOnShelled write fOnShelled;
    property OnWait      : TnotifyEvent read fOnWait write fOnWait;
    end;

  TDLLShell = class(TTpProcess)
  protected
    function DoShell: DWORD; Override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Module : String read fCommand write SetCommand;
    property Proc : String read fCommandLine write SetCommandLine;
    end;


  TWindowsShell = class(TTpProcess)
  {Note that from here on ShellResult equals the Handle returned by the shell.
   You can use ShellResult to see if the window is still there like the code
   associated with shlWaitTillDone even if that flag is not actually set.}
  private
    fWindowStyle: TWindowState;
    fShowWindow: word;
//    fOnPreShell           : TNotifyEvent;
//    fOnPostShell          : TNotifyEvent;
//    fOnWait               : TNotifyEvent;
    procedure SetWindowStyle(Value:TWindowState);
  protected
    function DoShell: DWORD; Override;
    procedure SetCommand(const Value:String); override;
    function GetTest:Boolean; Override;
    function GetComSpec: String; {returns default if blank}
    function GetExecStr: String;
    procedure SetExecStr(const Value:String);
    procedure SetNoString(const Value:String);
  public
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    constructor Create(AOwner: TComponent); override;
    constructor CreateWait(AOwner: TComponent;const aCommand,aParameter:String);
  published
    property Command;
    property Parameters;
    property ShowWindow: word read fShowWindow write fShowWindow stored false;
    property ComSpec: String read GetComSpec write SetNoString stored false;
    property ExecString  : String read GetExecStr write SetExecStr stored false;
    //
    property WindowStyle : TWindowState read fWindowStyle write SetWindowStyle;
    end;


function GetEnvVar(const Value:String):String;

//procedure Register;

implementation

uses 
  ucShell;

{------------------------------------------------------------------------------}

function GetEnvVar(const Value:String):String;
const
  cLength=80;
var
  i:integer;
begin
  Result:='';
  SetLength(Result,cLength);
  i:=GetEnvironmentVariable(pchar(Value),pchar(Result),cLength);
  Result:=copy(Result,1,i);
end;

{-------------------------------------------------------------------------}

constructor TTpProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

(*
See Note 1
procedure TTpProcess.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csUpdating in ComponentState) then
    exit;
  if Operation = opRemove then begin
    cx.NilIfSet(fWorking,AComponent);
    end;
end;
*)

procedure TTpProcess.Run(const aCmd,aParam:String);
begin
  Command:=aCmd;
  Parameters:=aParam;
  Execute;
end;

Procedure TTpProcess.DoExecute;
begin
{ See Note 1
  if (fFlags*[shlMsgTillReady,shlMsgTillDone])<>[] then begin
    cx.MakeIfNil(fWorking,TWorkingMsg);
    fWorking.BusyOn;
    end;
}

  inherited DoExecute;
  fShellResult:=DoShell;

{ See Note 1
  if fWorking<>nil then begin
    fWorking.BusyOff;
    fWorking:=nil;
    end;
}

end;

function TTpProcess.DoShell:DWORD;
{someone overrides this method shells than calls inherited to manage the message}
begin
  Result:=0;
  DoOnShell;
{ See Note 1
  if (shlMsgTillReady in fFlags) and (fWorking<>nil) then begin
    fWorking.BusyOff;
    fWorking:=nil;
    end;
}
end;

procedure TTpProcess.DoOnShell;
begin
  if Assigned(fOnShelled) then
    fOnShelled(Self);
end;
{}

procedure TTpProcess.SetCommand(const Value:String);
begin
  fCommand:=Value;
end;

{}

procedure TTpProcess.SetCommandLine(const Value:String);
begin
  fCommandLine:=Value;
end;

{-----------------------------------------------------------------------------------------}
{ TDLLShell                                                                               }
{-----------------------------------------------------------------------------------------}

constructor TDLLShell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TDLLShell.DoShell:DWORD;
var
  DllName,
  ProcName: PChar;
  LinkedProc: Procedure;
  Handle: THandle;
begin
  Result:=0;
{  if not FileExists(Module) then
    raise Exception.Create(classname+': Module '+Module+' does not exist!');}
  if ExtractFileExt(Module)='' then
    DllName:=PChar(ChangeFileExt(Module,'.DLL'))
  else
    DllName:=PChar(Module);
  try
    Handle:=LoadLibrary(DllName);
    if Handle<ErrorThreshold then
      raise Exception.Create(classname+': Handle for Module '+Module+' is '+inttostr(longint(Handle)));
    ProcName:=PChar(Proc);
    try
      TFarProc(@LinkedProc):= GetProcAddress(Handle, ProcName);
      if TFarProc(@LinkedProc)=nil then
        raise Exception.Create(classname+': Module '+Module+' has no procedure '+Proc);
      inherited DoShell; {can turn off message}
      LinkedProc;
    finally
      FreeLibrary(Handle);
      end;
  finally
    end;
end;

{-----------------------------------------------------------------------------------------}
{ TWindowsShell                                                                           }
{-----------------------------------------------------------------------------------------}


constructor TWindowsShell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fFlags:=[shlWaitTillIdle];
  WindowStyle:=wsNormal;
end;

constructor TWindowsShell.CreateWait(AOwner: TComponent;const aCommand,aParameter:String);
begin
  inherited Create(AOwner);
  if aCommand='' then begin
    Flags:= Flags+[shlUseShell];
    ExecString:= aParameter;
    end
  else begin
    Command:=aCommand;
    Parameters:=aParameter;
    end;
  Flags:=Flags+[shlWaitTillIdle,shlWaitTillDone,shlMsgTillReady,shlMsgTillDone];
end;

{}

procedure TWindowsShell.SetWindowStyle(Value:TWindowState);
begin
  fWindowStyle:=Value;
  fShowWindow:=ShowCommands[fWindowStyle];
end;

function TWindowsShell.DoShell:DWORD;
var
  a1,a2: string;
begin
  Result:=0;
  a1:= Command;
  a2:= Parameters;
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);  //windows
  with StartupInfo do begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow:= fShowWindow;
    end;
  if CreateProcess(nil, PChar(a1+' '+a2), @gsa, @gsa, False,
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
    with ProcessInfo do begin
      if (fFlags*[shlWaitTillIdle])<>[] then begin
        WaitForInputIdle(hProcess, INFINITE);
        end;
      inherited DoShell;
      if (fFlags*[shlMsgTillDone,shlWaitTillDone])<>[] then begin
        WaitForSingleObject(hProcess, INFINITE);
        end;
      CloseHandle(hThread);
      if not GetExitCodeProcess(hProcess,Result) then
        Result:=DWORD(-1);
      CloseHandle(hProcess);  //windows
      end
  else
    Result:=DWORD(-1);
end;

function TWindowsShell.GetTest:Boolean;
begin
  Result:= (fShellResult=0) or (fShellResult>=32);
end;

procedure TWindowsShell.SetNoString(const Value:String);
begin
end;

{-----------------------------------------------------------------------------------------}

function TWindowsShell.GetExecStr:String;
begin
  Result:=Command+' '+Parameters;
end;

procedure TWindowsShell.SetExecStr(const Value:String);
var
  a1,a2:string;
begin
  if shlUseShell in fFlags then
    a1:=ComSpec+' /C '+a1
  else
    a1:=Value;
  splitString(a1,' ',a1,a2);
  Command:=a1;
  Parameters:=a2;
end;

function TWindowsShell.GetComSpec: String;
begin
  Result:=GetEnvVar('COMSPEC');
  if Result='' then
    Result:=DefaultProcessor;
end;

procedure TWindowsShell.SetCommand(const Value:String);
begin
  fCommand:=Value;
  if pos('.',fCommand)=0 then
    fCommand:=fCommand+'.exe';
end;

//----------------------------------------------------------------------
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TTpProcess,TDLLShell,TWindowsShell]);
//end;
//----------------------------------------------------------------------

{ Note 1: The source to working.pas was posted at www.href.com/pub/source
  on 24-Mar-2003. }

end.


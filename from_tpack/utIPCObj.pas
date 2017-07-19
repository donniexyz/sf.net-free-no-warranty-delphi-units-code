unit utIPCObj; { WIN32 Inter-Process Communication Thread Helper Classes }

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
{$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes, SysUtils,
  System.SyncObjs;  // PSecurityAttributes etc.

procedure tpRaise(TExceptClass: ExceptClass; const Value: String);

type
  EKernelObject = class(Exception);

type
  TKernelObject = class(TObject)
  private
    fLeaseHandle: Boolean;
  protected
    fCreator: Boolean;
    fHandle: THandle;
    fName: string;
    function GetHandle: THandle;
    function CreateHandle: THandle; virtual; abstract;
    procedure RaiseError(const Msg: string);
  public
    Security: PSecurityAttributes;
    constructor Create; virtual;
    constructor CreateNamed(const aName: string);
    destructor Destroy; override;
    procedure Lease(Value: THandle);
    function CloseHandle: Boolean;
    property Handle: THandle read GetHandle;
    property Creator: Boolean read fCreator;
    property Name: string read fName write fName;
  end;

type
  TKernelObjectOpen = class(TKernelObject)
    // lets you defer opening an allocated object
  private
    fOpenExisting: Boolean;
  public
    property OpenExisting: Boolean read fOpenExisting write fOpenExisting;
  end;

  // ----------------------------------------------------------------------

{$IFDEF LINUX}
type
  DWORD = UInt32;
{$ENDIF}

type
  TMutex = class(TKernelObjectOpen)
  protected
    InitialOwn: Boolean;
    function CreateHandle: THandle; override;
  public
    function Get(TimeOut: DWORD): Boolean;
    function Release: Boolean;
  end;

  // ----------------------------------------------------------------------

type
  TtpEvent = class(TKernelObject)
  private
  protected
    function CreateHandle: THandle; override;
  public
    ManualReset, InitialState: Boolean;
    constructor CreateNamed(const aName: string; Manual: Boolean);
    procedure Signal;
    procedure Reset;
    function Wait(TimeOut: DWORD): Boolean;
  end;

  // ----------------------------------------------------------------------

type
  TSemaphore = class(TKernelObjectOpen)
  protected
    function CreateHandle: THandle; override;
  public
    InitialValue, MaximalValue: Integer;
    function Get(TimeOut: DWORD): Boolean;
    function Release: Boolean;
    function Signaled: Boolean;
  end;

/// <summary> Creates a region of shared memory
/// with CreateFileMapping and MapViewOfFile. </summary>
/// <remarks> Used by TPack Shared Int and related classes. </remarks>

type
  TSharedMem = class(TKernelObjectOpen)
  private
    fSize: Integer;
    procedure SetSize(Value: Integer);
  protected
    function CreateHandle: THandle; override;
    function GetBuffer: Pointer;
  public
    FFileView: Pointer;
    constructor CreateNamed(const aName: string; Size: Integer);
    destructor Destroy; override;
    property Size: Integer read fSize write SetSize;
    property Buffer: Pointer read GetBuffer;
  end;

  // ----------------------------------------------------------------------

(*
type
  TLogonType = (ltAuto, ltBatch, ltInterActive, ltService);

type
  TLogonUser = class(TKernelObjectOpen)
  private
    fDomain, fPassWord: string;
    fLogonType: TLogonType;
    fImpersonating: Boolean;
    function LogonUser(aLogonType: TLogonType; var hToken: THandle): Boolean;
  protected
    function CreateHandle: THandle; override;
  public
    constructor CreateNamed(const aDomain, aName, aPassword: string;
      aLogonType: TLogonType);
    destructor Destroy; override;
    procedure Logon;
    procedure Logout;
    procedure Impersonate;
    procedure Revert;
    //
    property Domain: String read fDomain write fDomain;
    property PassWord: String read fPassWord write fPassWord;
    property LogonType: TLogonType read fLogonType write fLogonType;
    property Impersonating: Boolean read fImpersonating write fImpersonating;
  end;
*)

  // ----------------------------------------------------------------------

(*var
  // reference event
  NullEvent: TtpEvent;*)

implementation

uses
{$IFDEF CodeSite}CodeSiteLogging, {$ENDIF}
{$IFDEF POSIX}Posix.SysTypes, Posix.SysMman,{$ENDIF} //mmap()
  ucString, ucMsTime, uCode, ZM_CodeSiteInterface;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// Criticalsection

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// Central error reporter.. use raise in one spot to limit code-overhead.

procedure tpRaise(TExceptClass: ExceptClass; const Value: String);

  function ReturnAddr: Pointer;
  begin
    Result := Pointer(TExceptClass);
  end;

begin
  Raise TExceptClass.Create(Value + sLineBreak + uCode.tpLastError)
    at ReturnAddr;
end;

procedure TKernelObject.RaiseError(const Msg: string);

  function aMsg: String;
  begin
    if (Length(Msg) > 0) and (Msg[Length(Msg)] >= ' ') then
      Result := sLineBreak
    else
      Result := '';
    Result := Result + ClassName + sLineBreak;
  end;

begin
  tpRaise(EKernelObject, Msg + aMsg + SysErrorMessage(GetLastError));
end;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// Core of all kernel objects.

constructor TKernelObject.Create;
begin
  inherited Create;
end;

constructor TKernelObject.CreateNamed(const aName: string);
begin
  inherited Create;
  fName := aName;
end;

destructor TKernelObject.Destroy;
begin
{$IFDEF LogInitFinal}  CSOutputDebugString('TKernelObject.Destroy');{$ENDIF}
  CloseHandle;
  Inherited Destroy;
end;

function TKernelObject.GetHandle: THandle;
var
  i: Integer;
begin
  if (fHandle = 0) then
  begin
    for i := 1 to 3 do
    begin
      fHandle := CreateHandle;
      if fHandle <> 0 then
        break;
      sleep(random(100)); // wait, then automatic retry, up to 3 times.
    end;
    if fHandle = 0 then
      RaiseError('TKernelObject.GetHandle can not create ' + ClassName);
  end;
  Result := fHandle;
end;

function TKernelObject.CloseHandle: Boolean;
begin
  Result := True;
{$IFDEF MSWINDOWS}
  if (fHandle <> 0) and not fLeaseHandle then
  begin
    Windows.CloseHandle(fHandle);
    fHandle := 0;
  end;
  if (fHandle <> 0) then
    Result := False;
{$ENDIF}
end;

procedure TKernelObject.Lease(Value: THandle);
begin
  if CloseHandle then
  begin
    fHandle := Value;
    fLeaseHandle := fHandle <> 0;
  end;
end;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// mutex

function TMutex.CreateHandle: THandle;
begin
{$IFDEF MSWINDOWS}
  Result := OpenMutex(MUTEX_ALL_ACCESS, True, PChar(Name));
  if (not OpenExisting) and (Result = 0) then
  begin
    Result := CreateMutex(Security, InitialOwn, PChar(Name));
    if (Result <> 0) and (GetLastError = 0) then
      fCreator := True;
  end
  else
  begin
    //CSSend(Format('%s %s OpenMutex Name %s; OpenExisting %s; Result %d', [self.ClassName, self.Name, Name, BoolToStr(OpenExisting, True), Result]));
  end;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TMutex.Get(TimeOut: DWORD): Boolean;
{$IFDEF MSWINDOWS}
var
  x: Integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  x := WaitForSingleObject(Handle, TimeOut);
  Result := (x = WAIT_OBJECT_0);
{$ENDIF}
end;

function TMutex.Release: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := ReleaseMutex(Handle);
{$ENDIF}
end;

constructor TtpEvent.CreateNamed(const aName: string; Manual: Boolean);
begin
  ManualReset := Manual;
  Inherited CreateNamed(aName);
end;

function TtpEvent.CreateHandle: THandle;
begin
  // NB: ManualReset MUST be set true prior to calling this method if you
  // are using the Reset method manually, e.g. from TtpShared...
{$IFDEF MSWINDOWS}
  Result := OpenEvent(EVENT_ALL_ACCESS or Synchronize, False, PChar(Name));
  if Result = 0 then
  begin
    // Could not open; try creating...
    Result := CreateEvent(Security, ManualReset, InitialState, PChar(Name));
    fCreator := True;
  end
  else
  begin
    // Opened an existing TtpEvent successfully
    fCreator := False;
  end;
{$ENDIF}
end;

procedure TtpEvent.Reset;
begin
{$IFDEF MSWINDOWS}
  ResetEvent(Handle);
{$ENDIF}
end;

procedure TtpEvent.Signal;
begin
{$IFDEF MSWINDOWS}
  SetEvent(Handle);
{$ENDIF}
end;

function TtpEvent.Wait(TimeOut: DWORD): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := WaitForSingleObject(Handle, TimeOut) = WAIT_OBJECT_0;
{$ENDIF}
end;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// semaphore

{$IFDEF MSWINDOWS}

const
  SEMAPHORE_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or Synchronize or $3);
{$ENDIF}

function TSemaphore.CreateHandle: THandle;
begin
{$IFDEF MSWINDOWS}
  if OpenExisting and (fName <> '') then
    Result := OpenSemaphore(SEMAPHORE_ALL_ACCESS, True, PChar(fName))
  else
  begin
    Result := CreateSemaphore(Security, InitialValue, MaximalValue,
      PChar(fName));
    fCreator := (Result <> 0);
  end;
{$ENDIF}
end;

function TSemaphore.Signaled: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := WaitForSingleObject(Handle, 0) = WAIT_TIMEOUT;
{$ENDIF}
end;

function TSemaphore.Get(TimeOut: DWORD): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := WaitForSingleObject(Handle, TimeOut) = WAIT_TIMEOUT;
{$ENDIF}
end;

function TSemaphore.Release: Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := ReleaseSemaphore(Handle, 1, nil);
{$ENDIF}
end;

// ----------------------------------------------------------------------
// ----------------------------------------------------------------------
// basic shared memory region

constructor TSharedMem.CreateNamed(const aName: string; Size: Integer);
begin
  fSize := Size;
  inherited CreateNamed(aName);
end;

procedure TSharedMem.SetSize(Value: Integer);
begin
  if FFileView = nil then
    fSize := Value
  else
    RaiseError(Name + ' already exists!' + sLineBreak);
end;

function TSharedMem.GetBuffer: Pointer;
begin
  GetHandle;
  Result := FFileView;
end;

function TSharedMem.CreateHandle: THandle;
const cFn = 'CreateHandle';
begin
{$IFDEF MSWINDOWS}
  // alternate flag suggested on MSDN: FILE_MAP_ALL_ACCESS
  Result := OpenFileMapping(FILE_MAP_READ OR FILE_MAP_WRITE,
    { dwDesiredAccess: DWORD; }
    False, { bInheritHandle: BOOL; }
    PChar(fName)); { lpName: PChar }
{$IFDEF LOGIPCERRORS}
  if OpenExisting and (Result = 0) then
    CSSendWarning(cFn + Chr(183) + 'OpenFileMapping ' + fName + ' nil');
{$ENDIF}
  // $FFFFFFFF creates a region of shared memory
  if (not OpenExisting) and (Result = 0) then
  begin
    Result := CreateFileMapping
      ({$IFDEF WIN64}$FFFFFFFFFFFFFFFF{$ELSE}$FFFFFFFF{$ENDIF}, Security,
      PAGE_READWRITE, 0, Size, PChar(fName));
    fCreator := (Result <> 0);
{$IFDEF LOGIPCERRORS}
    if Result = 0 then
      CSSendError(cFn + Chr(183) + 'CreateFileMapping ' + fName
        + ' nil');
{$ENDIF}

    // map a pointer to the handle of the shared memory region
    if Result > 0 then
    begin
      FFileView := MapViewOfFile(Result, FILE_MAP_WRITE, 0, 0, Size);
  {$IFDEF LOGIPCERRORS} if FFileView = nil then
        CSSendError(cFn + ' after MapViewOfFile ' + Chr(183) + 'FFileView nil');
      {$ENDIF}
    end;
  end;
{$ENDIF}

{$IFDEF POSIX}
// http://man7.org/linux/man-pages/man2/mmap.2.html
  FFileView := mmap(nil,
  size_t(Size),
  PROT_READ or PROT_WRITE,  MAP_SHARED,
    -1,
    0);
  Result := 1;
{$ENDIF}


end;

destructor TSharedMem.Destroy;
begin
{$IFDEF LogInitFinal}  CSOutputDebugString('TSharedMem.Destroy');{$ENDIF}
{$IFDEF MSWINDOWS}
  if FFileView <> nil then
    UnmapViewOfFile(FFileView);
  inherited Destroy;
{$ENDIF}
end;

// ----------------------------------------------------------------------
// TLogonUser

// LogonType:= ltAuto; is implicit

(*
constructor TLogonUser.CreateNamed(const aDomain, aName, aPassword: string;
  aLogonType: TLogonType);
begin
  inherited CreateNamed(aName);
  fDomain := aDomain;
  fPassWord := aPassword;
  fLogonType := aLogonType;
end;

destructor TLogonUser.Destroy;
begin
  Revert;
  inherited Destroy;
end;

function TLogonUser.LogonUser(aLogonType: TLogonType;
  var hToken: THandle): Boolean;
{$IFDEF CodeSite}const
  cFn = 'LogonUser'; {$ENDIF}
var
  dwLogonType: DWORD;
  FlagContinue: Boolean;
begin
  //CSEnterMethod(self, cFn); 
  FlagContinue := True;
  Result := False;
{$IFDEF MSWINDOWS}
  // SeTcbPrivilege
  if aLogonType = ltAuto then
    Result := LogonUser(ltBatch, hToken) or LogonUser(ltInterActive, hToken) or
      LogonUser(ltService, hToken)
  else
  begin
    case aLogonType of
      ltBatch:
        dwLogonType := LOGON32_LOGON_BATCH;
      ltInterActive:
        dwLogonType := LOGON32_LOGON_INTERACTIVE;
      ltService:
        dwLogonType := LOGON32_LOGON_SERVICE;
    else
      begin
        dwLogonType := 0; // n/a but avoid compiler warning
        FlagContinue := False;
      end;
    end;
    if FlagContinue then
    begin
      if (fDomain = '') then
      begin
        { Reference Microsoft Knowledge Base Article - 245683
          LogonUserW() Fails If lpszDomain Is NULL
          The easiest way to work around this problem is to explicitly call
          LogonUserA(), expressing the user name and password as ANSI strings. }
        Result := Windows.LogonUserA
          (PAnsiChar(UnicodeToAnsiCodePage(fName, 1252)),
          // string that specifies the user name
          PAnsiChar(UnicodeToAnsiCodePage(fDomain, 1252)),
          // string that specifies the domain or server
          PAnsiChar(UnicodeToAnsiCodePage(fPassWord, 1252)),
          // string that specifies the password
          dwLogonType, LOGON32_PROVIDER_DEFAULT,
          // DWORD dwLogonProvider, // specifies the logon provider
          hToken); // pointer to variable to receive token handle
      end
      else
      begin
        Result := Windows.LogonUser(PChar(fName),
          // string that specifies the user name
          PChar(fDomain), // string that specifies the domain or server
          PChar(fPassWord), // string that specifies the password
          dwLogonType, LOGON32_PROVIDER_DEFAULT,
          // DWORD dwLogonProvider, // specifies the logon provider
          hToken); // pointer to variable to receive token handle
      end;
    end;
  end;
{$ENDIF}
  //CSExitMethod(Self, cFn);
end;

function TLogonUser.CreateHandle: THandle;
begin
  if not LogonUser(LogonType, Result) then
    RaiseError('Can not logon as [' + Domain + '\' + name + '("' +
      PassWord + '")]');
end;

//

procedure TLogonUser.Logon;
begin
  GetHandle; // getting the handle is what logs us in. thus,
  // calling Impersonate includes the call to login.
end;

procedure TLogonUser.Logout;
begin
  Revert;
  if not CloseHandle then
    RaiseError('Can not close a leased handle');
end;

//

procedure TLogonUser.Impersonate;
begin
{$IFDEF MSWINDOWS}
  if fImpersonating then
    Revert;
  if not ImpersonateLoggedOnUser(Handle) then
    RaiseError('Can not impersonate [' + Domain + '\' + name + '("' +
      PassWord + '")]')
  else
    fImpersonating := True;
{$ENDIF}
end;

procedure TLogonUser.Revert; // ToSelf
begin
{$IFDEF MSWINDOWS}
  if fImpersonating and not RevertToSelf then
    RaiseError('Can not revert to self')
  else
    fImpersonating := False;
{$ENDIF}
end;
*)

(*procedure utIPCObj_Finalize;
const cFn = 'utIPCObj_Finalize';
begin
{$IF Defined(LogInitFinal) or Defined(LogTermRunner)}
  CSEnterMethod(nil, cFn);
{$IFEND}

FreeAndNil(NullEvent);

  {$IF Defined(MSWINDOWS) and (Defined(DEBUG) or Defined(LogTermRunner))}
  Windows.OutputDebugString(cFn + ' done');
  {$IFEND}

{$IF Defined(LogInitFinal) or Defined(LogTermRunner)}
  CSExitMethod(nil, cFn);
{$IFEND}
end;    *)

// TtpCriticalSection moved to separate unit under MIT license: tpCritSect.

(*initialization
// create a reference event
NullEvent := TtpEvent.Create;*)

(*finalization utIPCObj_Finalize;*)

end.

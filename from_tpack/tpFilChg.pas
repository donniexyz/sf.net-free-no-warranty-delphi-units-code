unit tpFilChg;
(*
Permission is hereby granted, on 1-Jun-2005, free of charge, to any person
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
  Windows, Messages, SysUtils, Classes;

const
  cFileChangeDelay= 100; //grant small delay in case of multiple changes.
                         //lets not be trigger-happy on bulk-deletes!

type
  TtpFileChangeTypes = (cnFileName,cnFileSize,cnFileTime,cnDirName,cnAttribute,cnSecurity);
  TtpFileChangeFilter = set of TtpFileChangeTypes;

  TtpFileChange = class(TComponent)
  private
    fEnabled: boolean;
    fDirectory: string;
    fSubDirectories: boolean;
    fThread: TThread;
    fDelay: integer;
    fFilter: word;
    fFilters: TtpFileChangeFilter;
    fOnFileChange: TNotifyEvent;
    //
    procedure SetEnabled(Value: Boolean);
    procedure SetFilters(Value: TtpFileChangeFilter);
    procedure SetDirectory(Value: string);
//    procedure SetDelay(Value: Integer);
    procedure SetSubDirectories(Value: boolean);
    //
    procedure Update;
    procedure CreateThread;
    procedure DestroyThread;
  protected
  public
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    property Filter: word read fFilter;
  published
    property Enabled: boolean read fEnabled write SetEnabled;
    property Filters: TtpFileChangeFilter read fFilters write SetFilters;
    property Delay: integer read fDelay write fDelay default cFileChangeDelay;
    property Directory: string read fDirectory write SetDirectory;
    property SubDirectories: boolean read fSubDirectories write SetSubDirectories;
    property OnFileChange: TNotifyEvent read fOnFileChange write fOnFileChange;
    end;

  TtpFileChangeThread = class(TThread)
  private
    fFileChangeNotify: TtpFileChange;
    //fSleeperThread: TThread;
    fDelay: integer;
    //procedure FileChanged;
    //procedure SleeperDone;
    procedure CallBack;
    procedure DoCallBack;
  public
    constructor Create(FileChangeNotify: TtpFileChange; Delay:Integer);
    procedure Execute; override;
    end;

(*
  TtpFileChangeSleeperThread = class(TThread)
  private
    fFileChangeThread: TtpFileChangeThread;
    fDelayUntil: Integer;
  public
    constructor Create(FileChangeThread: TtpFileChangeThread; DelayUntil:Integer);
    procedure Execute; override;
    end;
*)

implementation

uses
  ucFile;

//------------------------------------------------------------------------------

(*
constructor TtpFileChangeSleeperThread.Create(FileChangeThread: TtpFileChangeThread; DelayUntil:Integer);
begin
  fDelayUntil:=DelayUntil;
  fFileChangeThread:= FileChangeThread;
  FreeOnTerminate:=True;
  inherited Create(False);
end;

procedure TtpFileChangeSleeperThread.Execute;
var
  i:integer;
begin
  while not Terminated do begin
    i:=fDelayUntil-GetTickCount;
    if i<=0 then
      break;
    Sleep(i);
    end;
  fFileChangeThread.SleeperDone;
end;
*)

//------------------------------------------------------------------------------

constructor TtpFileChangeThread.Create(FileChangeNotify: TtpFileChange; Delay:Integer);
begin
  fDelay:=Delay;
  fFileChangeNotify:= FileChangeNotify;
  inherited Create(False);
  FreeOnTerminate:=True;
end;

{
procedure TtpFileChangeThread.FileChanged;
var
  i:integer;
begin
  if fDelay=0 then
    CallBack
  else begin
    i:= GetTickCount+fDelay;
    if assigned(fSleeperThread) then
      InterlockedExchange(TtpFileChangeSleeperThread(fSleeperThread).fDelayUntil,i)
    else
      fSleeperThread:= TtpFileChangeSleeperThread.Create(Self,i);
    end;
end;

procedure TtpFileChangeThread.SleeperDone;
begin
  fSleeperThread:=nil;
  CallBack;
end;
}

procedure TtpFileChangeThread.CallBack;
begin
  if not Terminated then
    Synchronize(DoCallBack);
end;

procedure TtpFileChangeThread.DoCallBack;
begin
  if not Terminated then
    with fFileChangeNotify do
      if assigned(OnFileChange) then
        OnFileChange(fFileChangeNotify);
end;

procedure TtpFileChangeThread.Execute;
var
  c: Integer;
  h: THandle;
  d: DWORD;
  w: DWORD;
begin
  c := 0;
  if not assigned(fFileChangeNotify) then
    exit;
  with fFileChangeNotify do begin
    h:=FindFirstChangeNotification(pchar(Directory),SubDirectories,Filter);
    try
      d:=INFINITE;
      while not Terminated do begin
        w:= WaitForSingleObject(h,d);
        if not Assigned(fFileChangeNotify) then
          break;
        if w=0{h} then //0-first item, e.g. h triggered
          if fDelay=0 then
            CallBack
          else
            d:=fDelay
        else begin
          d:=INFINITE;
          CallBack;
          end;
        if c = 20 then begin //close and reopen the change notification to prevent a "leak" in the nonpaged pool under Win2K
          FindCloseChangeNotification(h);
          h:=FindFirstChangeNotification(pchar(Directory),SubDirectories,Filter);
          c := 0;
          end else
            FindNextChangeNotification(h);
        Inc(c);
        end;
    finally
      FindCloseChangeNotification(h);
      end;
    end;
end;

//------------------------------------------------------------------------------

constructor TtpFileChange.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  fDelay:=cFileChangeDelay;
end;

destructor TtpFileChange.Destroy;
begin
  DestroyThread;
  inherited Destroy;
end;

procedure TtpFileChange.CreateThread;
begin
  fThread:= TtpFileChangeThread.Create(Self,Delay);
end;

procedure TtpFileChange.DestroyThread;
begin
  if Assigned(fThread) then begin //classes
    //TerminateThread(fThread.Handle,0);
    fThread.Terminate;
    fThread:= nil;
    end;
end;

procedure TtpFileChange.SetEnabled(Value: boolean);
begin
  if fEnabled<>Value then begin
    fEnabled:= Value and DirectoryExists(Directory);;
    if not (csDesigning in ComponentState) then
      if fEnabled then
        CreateThread
      else
        DestroyThread;
    end;
end;

procedure TtpFileChange.Update;
begin
  if Enabled then begin
    Enabled:=false;
    Enabled:=true;
    end;
end;

procedure TtpFileChange.SetSubDirectories(Value: boolean);
begin
  if fSubDirectories<>Value then begin
    fSubDirectories:= Value;
    Update;
    end;
end;

procedure TtpFileChange.SetDirectory(Value: string);
begin
  if CompareText(fDirectory,Value)<>0 then begin
    fDirectory:= Value;
    if fDirectory='' then
      Enabled:=False
    else
      Update;
    end;
end;

{
procedure TtpFileChange.SetDelay(Value: Integer);
begin
  if Delay<>Value then begin
    fDelay:= Value;
    Update;
    end;
end;
}

procedure TtpFileChange.SetFilters(Value: TtpFileChangeFilter);
begin
  if Value=Filters then
    exit;
  fFilter:= 0;
  if cnFileName in Value  then fFilter:=fFilter or FILE_NOTIFY_CHANGE_FILE_NAME;
  if cnFileSize in Value  then fFilter:=fFilter or FILE_NOTIFY_CHANGE_SIZE;
  if cnFileTime in Value  then fFilter:=fFilter or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if cnDirName  in Value  then fFilter:=fFilter or FILE_NOTIFY_CHANGE_DIR_NAME;
  if cnAttribute in Value then fFilter:=fFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if cnSecurity in Value  then fFilter:=fFilter or FILE_NOTIFY_CHANGE_SECURITY;
  fFilters:= Value;
  Update;
end;

//------------------------------------------------------------------------------
end.


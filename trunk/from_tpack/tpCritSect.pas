unit tpCritSect;

(*
Permission is hereby granted, on 24-June-2017, free of charge, to any person
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

Author of original version of TtpCriticalSection: Michael Ax
Copyright transferred to HREF Tools Corp. on 2-May-2000.
For background, see Richter's book and the win32 help files.
*)


interface

{$I hrefdefines.inc}

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes,
  SysUtils;

type
  TtpCriticalSection = class(TObject)
  private
    fCSect: TRTLCriticalSection;  // see also TCriticalSection in System.SyncObjs in Delphi 10.2 Tokyo
    fOnExecute: TNotifyEvent;
  protected
    procedure DoExecute; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure Lock;
    procedure UnLock;
    //
    procedure Synchronize(Method: TThreadMethod);
    //
    procedure Execute;
    property OnExecute: TNotifyEvent read fOnExecute write fOnExecute;
    property CSect: TRTLCriticalSection read fCSect;
  end;


implementation

constructor TtpCriticalSection.Create;
begin
  inherited Create;
  Windows.InitializeCriticalSection(fCSect);
end;

destructor TtpCriticalSection.Destroy;
begin
{$IFDEF MSWINDOWS}
  DeleteCriticalSection(fCSect);
{$ENDIF}
  inherited Destroy;
end;

//

procedure TtpCriticalSection.Lock;
begin
{$IFNDEF LINUX}
  EnterCriticalSection(fCSect);
{$ENDIF}
end;

procedure TtpCriticalSection.UnLock;
begin
{$IFNDEF LINUX}
  LeaveCriticalSection(fCSect);
{$ENDIF}
end;

procedure TtpCriticalSection.Synchronize(Method: TThreadMethod);
begin
  Lock;
  try
    Method;
  finally
    UnLock;
  end;
end;

procedure TtpCriticalSection.Execute;
begin
  Lock;
  try
    DoExecute;
  finally
    UnLock;
  end;
end;

procedure TtpCriticalSection.DoExecute;
begin
  if assigned(fOnExecute) then
    fOnExecute(self);
end;

end.

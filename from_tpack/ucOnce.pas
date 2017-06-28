unit ucOnce; { ucOnce- run the application once only. }

(*
  Copyright (c) 2000-2017 HREF Tools Corp.

  Permission is hereby granted, on 05-Aug-2008, free of charge, to any person
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

{$DENYPACKAGEUNIT ON}

interface

{$I hrefdefines.inc}
{
  Master copy of hrefdefines.inc is versioned on Source Forge in the ZaphodsMap project:
  https://sourceforge.net/p/zaphodsmap/code/HEAD/tree/trunk/ZaphodsMap/Source/hrefdefines.inc
}

uses
{$IFDEF POSIX}
  as of June 2017, this unit does NOT compile for Delphi 10.2 Tokyo Linux64
  {Code IFDEFd here for Linux was previously used with Kylix 3 and/or FPC}
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils,
{$IFDEF MSWINDOWS}
  ucWinAPI,
{$ENDIF}
  uCode, ucString, tpSyncObjs;

var
  OneInstanceMutex: TNamedMutex = nil;

implementation

procedure ucOnce_Initialization;
  function NeedQuit: Boolean;
  begin
    Result := HaveParam('/quit');
  end;
{$IFNDEF LINUX}

var
  AWindowName: string;
{$ENDIF}
begin
{$IFNDEF LINUX}
  AWindowName := LeftOf('.', ExtractFileName(ParamStr(0)));
  OneInstanceMutex := TNamedMutex.Create(UpperCase(AWindowName));
  if not OneInstanceMutex.WaitFor(500) then
  begin
{$WARN SYMBOL_PLATFORM OFF}
    if NeedQuit then
      ucWinAPI.TerminateWindow('TApplication', AWindowName)
    else
      ucWinAPI.ActivateWindow('TApplication', AWindowName);
{$WARN SYMBOL_PLATFORM ON}
    Halt;
  end;
{$ENDIF}
  if NeedQuit then
    Halt;
end;

procedure ucOnce_Finalize;
const cFn = 'ucOnce_Finalize';
begin
  FreeAndNil(OneInstanceMutex);
  {$IF Defined(MSWINDOWS) and (Defined(DEBUG) or Defined(LogInitFinal))}
  Windows.OutputDebugString(cFn);
  {$IFEND}
end;


initialization ucOnce_Initialization;

finalization ucOnce_Finalize;

end.

unit ucPipe;
(*
  Copyright (c) 2000-2017 HREF Tools Corp.

  Permission is hereby granted, on 31-Oct-2003, free of charge, to any person
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

{ Confirmed working 18.June.2017 with Delphi 10.2 Tokyo "D25" }

interface

uses
  Classes, // defines TGetStrProc
  SysUtils, // defines Exception
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF POSIX}Does not yet support Linux64.{$ENDIF}
  ucString;

type
  TLittleBuffer = Array [0 .. 256] of Byte;
{$IFDEF MSWINDOWS}
  TGetAnsiStrProc = procedure(const S: AnsiString) of object;
{$ENDIF}
  TGetRawBytesProc = procedure(var Data: TLittleBuffer; const Count: Integer)
    of object;


function GetDosOutput(ExeName, OptionalParams, Folder: UnicodeString;
  GetRawBytesProc: TGetRawBytesProc; out ErrorCode: Integer): RawByteString;

{$IFDEF MSWINDOWS}
function GetDosOutputA(const CmdLine: AnsiString;
  GetAnsiStrProc: TGetAnsiStrProc; out ErrorCode: Integer): AnsiString;
{$ENDIF}

implementation

uses
  ucWinSecurityObj; // pre-initialized global security attribute in TPack

const
  LineBufSize = 255;

function GetDosOutput(ExeName, OptionalParams, Folder: UnicodeString;
  GetRawBytesProc: TGetRawBytesProc; out ErrorCode: Integer): RawByteString;
var
  hPipeRead, hPipeWrite: THandle;
  StartupInfo: TSTARTUPINFOW;
  ProcessInfo: TPROCESSINFORMATION;
  tmpBuffer: TLittleBuffer; // Array[0..LineBufSize+1] of Byte;
  BytesRead: Cardinal;
  BytesReadInt: Integer;
  snippet: RawByteString;
  RawResult: RawByteString;
begin
  Result := '';
  ErrorCode := 0;
  if not CreatePipe(hPipeRead, // read handle
    hPipeWrite, // write handle
    @gsa, // global security attributes
    0) // number of bytes reserved for pipe
  then
    Raise Exception.Create( 'Error building the pipe' );

  FillChar(StartupInfo, sizeof(StartupInfoW), 0);
  with StartupInfo do
  begin
    cb := sizeof(StartupInfo);
    lpReserved := nil; // Reserved; must be NULL.
    lpTitle := nil;
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
    hStdInput := 0; // hPipeRead;
    hStdOutput := hPipeWrite;
    hStdError := hPipeWrite;
  end;

  if Folder = '' then
    Folder := GetCurrentDir;

  try
    if not CreateProcessW(PWideChar(ExeName), // Application without params
      PWideChar(ExeName + ' ' + OptionalParams), // Application with params
      @gsa, // pointer to process security attributes
      @gsa, // pointer to thread security attributes
      TRUE, // handle inheritance flag
      NORMAL_PRIORITY_CLASS, // creation flags
      nil, // pointer to new environment block
      PWideChar(Folder), // pointer to current directory name
      StartupInfo, // pointer to STARTUPINFO
      ProcessInfo) // pointer to PROCESS_INFORMATION
    then
    begin
      CloseHandle(hPipeWrite);
      CloseHandle(hPipeRead);
      ErrorCode := GetLastError;
      Exit;
    end;
  except
    on E: exception do
    begin
      ErrorCode := GetLastError;
      CloseHandle(hPipeWrite);
      CloseHandle(hPipeRead);
      Exit;
    end;
  end;

  CloseHandle(hPipeWrite);

  RawResult := '';
  BytesRead := 0;

  repeat
    Sleep(10);
    FillChar(tmpBuffer, LineBufSize, 0);
    if ReadFile(hPipeRead, // handle of the read end of our pipe
      tmpBuffer, // address of buffer  that receives data
      LineBufSize, // number of bytes to read
      BytesRead, // address of number of bytes read
      nil) // non-overlapped.
    then
    begin
      snippet := '';
      SetLength(snippet, BytesRead);
      BytesReadInt := BytesRead;
      Move(tmpBuffer, snippet[1], BytesReadInt);
      if assigned(GetRawBytesProc) then
      begin
        // NB: pass an array of Bytes, not a RawByteString which is Ansi.
        GetRawBytesProc(tmpBuffer, BytesReadInt);
      end;
      RawResult := RawResult + snippet;
    end
    else
      break;
  until False;

  with ProcessInfo do
  begin
    WaitForSingleObject(hProcess, INFINITE);
    CloseHandle(hThread);
    CloseHandle(hProcess);
  end;

  CloseHandle(hPipeRead);
  Result := RawResult;
end;

{$IFDEF MSWINDOWS}
function GetDosOutputA(const CmdLine: AnsiString;
  GetAnsiStrProc: TGetAnsiStrProc; out ErrorCode: Integer): AnsiString;
var
  hPipeRead, hPipeWrite: THandle;
  StartupInfo: {$IFDEF UNICODE}TSTARTUPINFOA{$ELSE}TSTARTUPINFO{$ENDIF};
  ProcessInfo: TPROCESSINFORMATION;
  tmpBuffer: array [0 .. LineBufSize + 1] of AnsiChar;
  BytesRead: DWORD;
begin
  ErrorCode := 0;
  if not CreatePipe(hPipeRead, // read handle
    hPipeWrite, // write handle
    @gsa, // global security attributes
    0) // number of bytes reserved for pipe
  then
    tpRaise(exception, 'Error building the pipe');

  FillChar(StartupInfo, sizeof(StartupInfo), 0);
  with StartupInfo do
  begin
    cb := sizeof(StartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
    hStdInput := 0; // hPipeRead;
    hStdOutput := hPipeWrite;
    hStdError := hPipeWrite;
  end;

  if not CreateProcessA(nil, // pointer to name of executable module
    PAnsiChar(CmdLine), // pointer to command line string
    @gsa, // pointer to process security attributes
    @gsa, // pointer to thread security attributes
    TRUE, // handle inheritance flag
    NORMAL_PRIORITY_CLASS, // creation flags
    nil, // pointer to new environment block
    nil, // pointer to current directory name
    StartupInfo, // pointer to STARTUPINFO
    ProcessInfo) // pointer to PROCESS_INFORMATION
  then
  begin
    CloseHandle(hPipeWrite);
    CloseHandle(hPipeRead);
    ErrorCode := GetLastError;
  end;

  CloseHandle(hPipeWrite);

  Result := '';
  BytesRead := 0;
  repeat
    Sleep(10);
    FillChar(tmpBuffer, LineBufSize, 0);
    if ReadFile(hPipeRead, // handle of the read end of our pipe
      tmpBuffer, // address of buffer  that receives data
      LineBufSize, // number of bytes to read
      BytesRead, // address of number of bytes read
      nil) // non-overlapped.
    then
    begin
      tmpBuffer[BytesRead] := #0;
      Result := Result + tmpBuffer;
      if assigned(GetAnsiStrProc) then
        GetAnsiStrProc(tmpBuffer);
    end
    else
      break;
  until False;

  with ProcessInfo do
  begin
    WaitForSingleObject(hProcess, INFINITE);
    CloseHandle(hThread);
    CloseHandle(hProcess);
  end;

  CloseHandle(hPipeRead);
end;
{$ENDIF}

end.

unit ucPipe;
(*
  Copyright (c) 2000-2018 HREF Tools Corp.

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

{ Confirmed working 26.July.2017 with Delphi 10.2 Tokyo "D25" }

interface

uses
  Classes, // defines TGetStrProc
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF POSIX}Does not yet support Linux64.{$ENDIF}
  SysUtils // defines Exception
  ;

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
  ZM_CodeSiteInterface,  // source in ZaphodsMap project on sf.net
  ucWinSecurityObj; // pre-initialized global security attribute in TPack

const
  LineBufSize = 255;

function GetDosOutput(ExeName, OptionalParams, Folder: UnicodeString;
  GetRawBytesProc: TGetRawBytesProc; out ErrorCode: Integer): RawByteString;
const cFn = 'GetDosOutput';
var
  hPipeRead, hPipeWrite: THandle;
  StartupInfo: TSTARTUPINFOW;
  ProcessInfo: TPROCESSINFORMATION;
  tmpBuffer: TLittleBuffer; // Array[0..LineBufSize+1] of Byte;
  BytesRead: Cardinal;
  BytesReadInt: Integer;
  snippet: RawByteString;
  RawResult: RawByteString;
  InfoMsg: string;
  ApplicationName: string;
  CommandLine: string;
  CurrentDirFolder: string;
  lpApplicationName, lpCommandLine: Pointer;
begin
  {$IFDEF LOGUCPIPE}CSEnterMethod(nil, cFn);{$ENDIF}

  Result := '';
  ErrorCode := 0;
  if not CreatePipe(hPipeRead, // read handle
    hPipeWrite, // write handle
    @gsa, // global security attributes
    0) // number of bytes reserved for pipe
  then
  begin
    InfoMsg := 'Error building the pipe';
    CSSendError(cFn + ': ' + InfoMsg);
    Raise Exception.Create( InfoMsg );
  end;

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

  if Folder <> '' then
    Folder := IncludeTrailingPathDelimiter(Folder);

  {$IFDEF LOGUCPIPE}
  CSSend(csmLevel5, 'Folder', Folder);
  CSSend(csmLevel5, 'ExeName', ExeName);
  CSSend(csmLevel5, 'OptionalParams', OptionalParams);
  {$ENDIF}

  CurrentDirFolder := GetCurrentDir;

  if Pos('cmd.exe', ExeName) > 0 then
  begin
    ApplicationName := ExeName;
    CommandLine := OptionalParams;
  end
  else
  if SameText(ExtractFileExt(ExeName), '.bat') then
  begin
    { To run a batch file, you must start the command interpreter;
      set lpApplicationName to cmd.exe and set lpCommandLine to the following
      arguments: /c plus the name of the batch file.
      Reference:
      https://msdn.microsoft.com/en-us/library/windows/desktop/ms682425(v=vs.85).aspx
    }
    ApplicationName := GetEnvironmentVariable('windir') + '\system32\cmd.exe';
    CommandLine := '/c ' + Folder + ExeName + ' ' + OptionalParams;
  end
  else
  begin
    ApplicationName := '';
    CommandLine := Folder + ExeName + ' ' + OptionalParams;
  end;

  {$IFDEF LOGUCPIPE}
  CSSend(csmLevel5, 'ApplicationName', ApplicationName);
  CSSend(csmLevel5, 'CommandLine', CommandLine);
  {$ENDIF}

  if ApplicationName = '' then
    lpApplicationName := nil
  else
    lpApplicationName := @ApplicationName[1];
  if CommandLine = '' then
    lpCommandLine := nil
  else
    lpCommandLine := @CommandLine[1];


  try
    if not CreateProcess(
      lpApplicationName,
      lpCommandLine,
      @gsa, // pointer to process security attributes
      @gsa, // pointer to thread security attributes
      TRUE, // handle inheritance flag
      NORMAL_PRIORITY_CLASS, // creation flags
      nil, // pointer to new environment block
      PChar(CurrentDirFolder), // NB: always non-blank here.
      StartupInfo, // pointer to STARTUPINFO
      ProcessInfo) // pointer to PROCESS_INFORMATION
    then
    begin
      ErrorCode := GetLastError;
      if ErrorCode = 0 then
        ErrorCode := 1;
      CSSendError(cFn + ': CreateProcess ErrorCode ' + S(ErrorCode)
      {$IFDEF MSWINDOWS}
      + ' ' + SysErrorMessage(ErrorCode)
      {$ENDIF}
      );
    end;
  except
    on E: Exception do
    begin
      CSSendException(nil, cFn, E);
      ErrorCode := GetLastError;
      if ErrorCode = 0 then
        ErrorCode := 1;
      CSSendError(cFn + ': Exception ErrorCode ' + S(ErrorCode)
      {$IFDEF MSWINDOWS}
      + ' ' + SysErrorMessage(ErrorCode)
      {$ENDIF}
      );
    end;
  end;

  CloseHandle(hPipeWrite);

  if ErrorCode = 0 then
  begin

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

    Result := RawResult;
    //CSSend('RawResult', string(RawResult));
  end;

  CloseHandle(hPipeRead);

  {$IFDEF LOGUCPIPE}CSExitMethod(nil, cFn);{$ENDIF}
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
    Raise Exception.Create('Error building the pipe');

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

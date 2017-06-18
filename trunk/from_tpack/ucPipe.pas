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

Author of original version of this file: Michael Ax
Copyright transferred to HREF Tools Corp. on 2-May-2000.
*)

interface

uses
  Classes,  //defines TGetStrProc
  SysUtils, //defines Exception
  Windows;  //for good cheer

function GetDosOutput(CmdLine:string;GetStrProc:TGetStrProc): String;

{$IFNDEF LINUX}
function DirectorySize(const Path:String): integer;
//calculates the size of a directory
//n:=DirectorySize('.');  .. -1 if does not exist
{$ENDIF}

implementation

uses
  uCode, //global exception raise routine tpRaise
  utIpcObj; //pre-initialized global security attribute

//------------------------------------------------------------------------------

const
  LineBufSize = 255;

//------------------------------------------------------------------------------

function GetDosOutput(CmdLine:string;GetStrProc:TGetStrProc): String;
var
  hPipeRead,
  hPipeWrite: THandle;
  StartupInfo: TSTARTUPINFO;
  ProcessInfo: TPROCESSINFORMATION;
  tmpBuffer: array[0..LineBufSize] of char;
  BytesRead: DWORD;
//  BytesReadAddr: Cardinal;
begin
  if not CreatePipe(
    hPipeRead,    // read handle
    hPipeWrite,   // write handle
    @gsa,         // global security attributes
    0)            // number of bytes reserved for pipe
  then
    tpRaise(exception,'Error building the pipe');

  FillChar( StartupInfo, sizeof(StartupInfo), 0 );
  with StartupInfo do begin
    cb          := sizeof(StartupInfo);
    dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_HIDE;
    hStdInput   := 0; //hPipeRead;
    hStdOutput  := hPipeWrite;
    hStdError   := hPipeWrite;
    end;

  if not CreateProcess( //tpshell
    nil,                    // pointer to name of executable module
    pchar(CmdLine),         // pointer to command line string
    @gsa,                   // pointer to process security attributes
    @gsa,                   // pointer to thread security attributes
    TRUE,                   // handle inheritance flag
    NORMAL_PRIORITY_CLASS,  // creation flags
    nil,                    // pointer to new environment block
    nil,                    // pointer to current directory name
    StartupInfo,            // pointer to STARTUPINFO
    ProcessInfo)            // pointer to PROCESS_INFORMATION
  then begin
    CloseHandle(hPipeWrite);
    CloseHandle(hPipeRead);
    tpRaise(exception,'Error launching process: '+IntToStr(GetLastError));
    end;

  CloseHandle(hPipeWrite);

  Result:='';
  BytesRead := 0;
  repeat
    Sleep( 10 );
    tmpBuffer := '';
    if ReadFile(
      hPipeRead,   // handle of the read end of our pipe
      tmpBuffer,   // address of buffer  that receives data
      lineBufSize, // number of bytes to read
      BytesRead,   // address of number of bytes read
      nil)         // non-overlapped.
    then begin
      Result:= Result+tmpBuffer;
      if assigned(GetStrProc) then
        GetStrProc(tmpBuffer);
      end
    else
      break;
  until false;

  with ProcessInfo do begin
    WaitForSingleObject(hProcess, INFINITE);
    CloseHandle(hThread);
    CloseHandle(hProcess);
    end;

  //CloseHandle(hPipeWrite);
  CloseHandle(hPipeRead);
end;

//----------------------------------------------------------------------

{$IFNDEF LINUX}
function DirectorySize(const Path:String): integer;
//calculates the size of a directory
var
  SearchRec: TSearchRec;
begin
  Result:=0;
{$IFDEF DELPHI6UP}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
  if SysUtils.FindFirst(TrailingBackSlash(Path)+'*.*',faReadOnly+faArchive,SearchRec)=0 then
    try
      repeat
        Result:=Result+SearchRec.Size;
      until (SysUtils.FindNext(searchRec)<>0);
    finally
      SysUtils.FindClose(SearchRec);
      end
  else
    Result:=-1;
{$IFDEF DELPHI6UP}
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
end;
{$ENDIF}

end.

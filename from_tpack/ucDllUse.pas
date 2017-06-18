unit ucDllUse;   {Utility Code for DLL Use in Delphi 1.0}
(*
Permission is hereby granted, on 27-Jul-2002, free of charge, to any person 
obtaining a copy of this software (the "Software"), to deal in the Software 
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

interface

{$IFNDEF WIN32}
uses
  SysUtils
, WinProcs, WinTypes
, ToolHelp;

procedure InitTModuleEntry(var m: TModuleEntry);
procedure FillModuleEntryByName(var m: TModuleEntry;Const ModuleName:String);
procedure KillDll(Const ModuleName:String);
procedure AdjustDllCount(Const ModuleName:String;Target:longint);
function DllUsageCount(Const ModuleName:String): longint;
function DllFullName(Const ModuleName:String): String;
function DllPathName(Const ModuleName:String): String;
{$ENDIF}


implementation

{DLL Utility procs -----------------------------------------------}

{$IFNDEF WIN32}

procedure InitTModuleEntry(var m: TModuleEntry);
begin
  {initialize size, once only}
  m.dwSize:=sizeof(m);
  {clear data fields}
  StrpCopy(m.szModule,'');
  StrpCopy(m.szExePath,'');
  m.wUsageFlags:=0;
end;

procedure FillModuleEntryByName(var m: TModuleEntry;Const ModuleName:String);
var
  i:word;
  a:array[0..50] of char;
begin
  InitTModuleEntry(m);
  StrpCopy(a,ModuleName);
  ModuleFindName(addr(m),a);
  if strpas(m.szModule)<>ModuleName then {not running.}
    InitTModuleEntry(m);  {buffer points to this exe. clear it}
end;

procedure KillDll(Const ModuleName:String);
var
  m: TModuleEntry;
begin
  FillModuleEntryByName(m,ModuleName);
  {decrement usage count till freed}
  while FreeModule(m.hModule) do;
end;

function DllUsageCount(Const ModuleName:String): longint;
var
  m: TModuleEntry;
begin
  FillModuleEntryByName(m,ModuleName);
  Result:= m.wUsageFlags;
end;

procedure AdjustDllCount(Const ModuleName:String;Target:longint);
var
  m: TModuleEntry;
begin
  While true do begin
    FillModuleEntryByName(m,ModuleName);
    if m.wUsageFlags<=Target then
      exit;
    FreeModule(m.hModule)
    end;
end;

function DllFullName(Const ModuleName:String): String;
var
  m: TModuleEntry;
begin
  FillModuleEntryByName(m,ModuleName);
  Result:= StrPas(m.szExePath);
end;

function DllPathName(Const ModuleName:String): String;
begin
  Result:= ExtractFilePath(DllFullName(ModuleName));
end;

{$ENDIF}
{---------------------------------------------------------------}
end.

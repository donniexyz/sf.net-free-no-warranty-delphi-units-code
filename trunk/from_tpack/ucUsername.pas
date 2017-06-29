unit ucUsername;

(*
Copyright (c) 2000-2017 HREF Tools Corp.

Permission is hereby granted, on 28-Jun-2017, free of charge, to any person 
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

interface

{$I hrefdefines.inc}
{
  Master copy of hrefdefines.inc is versioned on Source Forge in the ZaphodsMap project:
  https://sourceforge.net/p/zaphodsmap/code/HEAD/tree/trunk/ZaphodsMap/Source/hrefdefines.inc
}

/// <summary> returns user name of current process </summary>
function GetProcessUserName: string;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
  // avoid using MultiTypeApp here.
{$ENDIF}

function GetProcessUserName: string;
{$IFDEF MSWINDOWS}
var
  Buffer: array [0 .. MAX_PATH] of Char;
  Size: DWORD;
{$ENDIF}
{$IF Defined(FPC) and Defined(LINUX)}
var
  PasswordRecord: PPasswordRecord;
{$IFEND}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  Size := Length(Buffer);
  if GetUserName(Buffer, Size) then
    Result := Buffer;
{$ENDIF}
{$IF Defined(FPC) and Defined(LINUX)}
  PasswordRecord := getpwuid(geteuid);
  if PasswordRecord <> nil then
    Result := PasswordRecord.pw_name;
{$IFEND}
end;

end.

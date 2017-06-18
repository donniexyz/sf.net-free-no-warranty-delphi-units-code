unit ucPChar;   {Utility Procs for dealing with PChars}
(*
Permission is hereby granted, on 24-Mar-2003, free of charge, to any person
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

uses SysUtils;

function NewPChar(Var VarPointer:PChar;const Value:String):PChar;
function MakePChar(const Value:String):PChar;
procedure MovePChar2PString(Dest:PString;Source:PChar;aFree:Boolean);
procedure FreePChar(Value:PChar);
function ReceivePChar(Value:PChar):String;

implementation

{------------------------------------------------------------------------------}
{  PCHAR AND PSTRING UTILITIES                                                 }
{------------------------------------------------------------------------------}

function NewPChar(Var VarPointer:PChar;const Value:String):PChar;
begin
  Result:=MakePChar(Value);
  VarPointer:=Result;
end;

function MakePChar(const Value:String):PChar;
begin
  GetMem(Result,256);           {make room for a pascal maxlen pchar}
  StrPCopy(Result,Value);       {copy string passed into buffer}
end;

procedure FreePChar(Value:PChar);
begin
  if Value<>nil then
    FreeMem(Value,256);
end;

function ReceivePChar(Value:PChar):String;
begin
  Result:=StrPas(Value);
  FreePChar(Value);
end;

procedure MovePChar2PString(Dest:PString;Source:PChar;aFree:Boolean);
begin
  AssignStr(Dest,StrPas(Source));
  if aFree then
    FreePChar(Source);
end;

end.

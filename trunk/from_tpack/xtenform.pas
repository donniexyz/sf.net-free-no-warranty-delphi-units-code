unit xtenform;

(*
Permission is hereby granted, on 1-Nov-2003, free of charge, to any person
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
  Classes, SysUtils, Forms, Windows, 
  xtension;

Type

  TtpFormExtensions = class(TComponentExtensions)
  protected
  public
    function SetIfFoundInApp(aComponent:TComponent;aClass:TClass):TComponent;
    function FindInApp(aClass:TClass):integer;
    end;

implementation

{-----------------------------------------------------------------------------------------}

function TtpFormExtensions.SetIfFoundInApp(aComponent:TComponent;aClass:TClass):TComponent;
var
  Index:integer;
begin
  Result:=aComponent;
  if Result=nil then begin
    Index:=FindInApp(aClass);
    if Index>-1 then
      Result:= Application.Components[Index];
    end;
end;

function TtpFormExtensions.FindInApp(aClass:TClass):integer;
begin
  with Application do
    for result:=0 to pred(ComponentCount) do
      if Components[result] is aClass then
        exit;
  result:=-1;
end;

{-----------------------------------------------------------------------------------------}
end.

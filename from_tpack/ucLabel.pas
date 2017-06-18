unit ucLabel;   {Utility Code for working with TLabels}
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

uses StdCtrls, SysUtils;

{---------------------------------------------------------------}
procedure LabelInt(aLabel:TLabel;Value:LongInt);
procedure LabelInc(aLabel:TLabel);
procedure LabelDec(aLabel:TLabel);

{
procedure TForm1.UpdateBits(Value:Word);
var
  i:integer;
begin
  tplabel3.AsInteger:=Value;
  with tpLabel4 do begin
    Caption:='';
    for i:=0 to 15 do begin
      if Value and (1 shl i) = (1 shl i) then
        Caption:='1'+Caption
      else
        Caption:='0'+Caption;
      if (i>0) and (i<15) and (succ(i) mod 4 = 0) then
        Caption:='.'+Caption;
      end;
    end;
end;
}

implementation
{---------------------------------------------------------------}

procedure LabelInt(aLabel:TLabel;Value:LongInt);
begin
  if aLabel <> nil then
    with aLabel do
      Caption:=Inttostr(Value);
end;

procedure LabelInc(aLabel:TLabel);
begin
  if aLabel <> nil then
    with aLabel do
      Caption:=Inttostr(Succ(StrToIntDef(Caption,0)));
end;

procedure LabelDec(aLabel:TLabel);
begin
  if aLabel <> nil then
    with aLabel do
      Caption:=Inttostr(Pred(StrToIntDef(Caption,0)));
end;


end.

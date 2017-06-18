unit utRegis;
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
  Classes, Registry;

type
  TtpRegistry=class(TRegistry)
  public
    function GetSectionValues(List: TStrings): TStrings;
    procedure SetSectionValues(List: TStrings);
      //returns name=value pairs for the current section
    procedure GetSectionValuesNoName(List: TStrings);
    procedure SetSectionValuesNoName(List: TStrings);
      //returns values from the current section
    end;

implementation

uses
  ucstring;
  
//------------------------------------------------------------------------------

function TtpRegistry.GetSectionValues(List: TStrings): TStrings;
var
  t: TStringList;
  I: Integer;
begin
  Result:=List;
  t:= TStringList.Create;
  with t do try
    GetValueNames(t);
    with List do try
      BeginUpdate;
      for i:=0 to pred(t.Count) do
        Values[t[i]]:= ReadString(t[i]);
    finally
      EndUpdate;
      end;
  finally
    Free;
    end;
end;


procedure TtpRegistry.SetSectionValues(List: TStrings);
var
  t: TStringList;
  I: Integer;
  a1,a2: String;
begin
  t:= TStringList.Create;
  with t do try
    GetValueNames(t);
    for i:=0 to pred(t.Count) do
      DeleteValue(t[i]);
    for i:=0 to pred(List.Count) do
      if splitstring(List[i],'=',a1,a2) then
        Writestring(a1,a2);
  finally
    Free;
    end;
end;

//------------------------------------------------------------------------------

procedure TtpRegistry.GetSectionValuesNoName(List: TStrings);
var
  t: TStringList;
  I: Integer;
begin
  t:= TStringList.Create;
  with t do try
    GetValueNames(t);
    with List do try
      BeginUpdate;
      for i:=0 to pred(t.Count) do
        Add(ReadString(t[i]));
    finally
      EndUpdate;
      end;
  finally
    Free;
    end;
end;

procedure TtpRegistry.SetSectionValuesNoName(List: TStrings);
var
  t: TStringList;
  I: Integer;
begin
  t:= TStringList.Create;
  with t do try
    GetValueNames(t);
    for i:=0 to pred(t.Count) do
      DeleteValue(t[i]);
    for i:=0 to pred(List.Count) do
      Writestring(Str2(i),List[i]);
  finally
    Free;
    end;

end;

//------------------------------------------------------------------------------
end.

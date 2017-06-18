unit ucDB;
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

Author of original version of this file: Ann Lynnworth
*)

interface

uses 
  dbtables, classes;

procedure RecordToList(tbl:TTable; slist:TStringList);
procedure ListToRecord(tbl:TTable; slist:TStringList);

implementation

uses 
  ucString;

procedure RecordToList(tbl:TTable; slist:TStringList);
var
  i:integer;
begin
  slist.clear;
  with tbl do begin
    for i:=0 to pred(FieldCount) do begin
      slist.add(fields[i].fieldname+'='+fields[i].asString);
      end;
    end;
end;

//----------------------------------------------------------------------------//

procedure ListToRecord(tbl:TTable; slist:TStringList);
var
  i:integer;
begin
  with tbl do begin
    for i:=0 to pred(FieldCount) do begin
      fields[i].asString:=RightOfEqual(slist[i]);
      end;
    end;
end;

end.
 
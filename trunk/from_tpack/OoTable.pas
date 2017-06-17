unit OoTable; {Object Oriented Table. At least I think so..}
(*
Permission is hereby granted, on 1-Aug-2005, free of charge, to any person
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

{we're going to create a property that represents the current record's unique id }

interface
uses db, dbtables;

type
  TTableUniqueIDField=class(TTable)
  private
    fUniqueID: TField;
    {making this a pointer to the tField rather than an index keeps things simple and fast}
  protected
    function GetUniqueID:LongInt;
    procedure SetUniqueID(aValue:LongInt);
  public
    procedure DoAfterOpen; Override;
  published
    property UniqueID: LongInt read GetUniqueID write SetUniqueID stored false;
    end;  {modified but not tested as published in this sample}

  {note: while you could make this a component you probably'd want to define individual
  components for every major table in the system vis inheritance.. but that get tricky if
  you want tables to know about each other. otoh you can create new instance of tables
  easy if they have a type. you can set table names in the Create procs so that you can
  instantiate and open them without knowing a tablename or alias!..}

  TCustomerTable= class(TTableUniqueIDField); {that's enough to get an alias type for this}

const
  cUniqueID='UniqueID'; {reference to the field name. use a stringtable resource!}

implementation

procedure TTableUniqueIDField.DoAfterOpen;
begin
  inherited DoAfterOpen;
  fUniqueID:=FieldByName(cUniqueId); {raises exception if not there}
  fUniqueID.ReadOnly:=True;          {user can not change the field}
end;

procedure TTableUniqueIDField.SetUniqueID(aValue:LongInt);
begin
  if State<>dsInactive then
    with fUniqueID do begin
      ReadOnly:=False;               {gain access}
      AsInteger:=aValue;             {set}
      ReadOnly:=True;                {restrict access} {without passwording things}
      end
end;

function TTableUniqueIDField.GetUniqueID:LongInt;
begin
  if State<>dsInactive then
    Result:=fUniqueID.AsInteger
  else
    Result:=-1;
end;

end.

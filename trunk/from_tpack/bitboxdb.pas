unit bitboxdb;

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

{THE POINT:  To create checkbox group components that will take a byte or word
and provide dynamically sized boxes containing selected items from a universe
of 8 or 16 choices. Allowing the user to check/set bits via a form.}

{this unit takes advantage of delphi's small set implementation, which works
in bytes and words for sets with less than 9/17 members respectively.}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls
, Windows
, Forms, Dialogs, DB, DBCtrls, DBTables, StdCtrls
, BitBox;

type
  TdbBitBox = class(TBitBox)
  private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
  protected
    procedure ChangeSelected(Sender:TObject); override;
    procedure Notification(AComponent:TComponent; Operation:TOperation); override;
  public
    constructor Create(aOwner:TComponent); Override;
    destructor Destroy; Override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    end;


implementation

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

constructor TdbBitBox.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  inherited ReadOnly := True;
  FDataLink:= TFieldDataLink.Create;
  FDataLink.OnDataChange:= DataChange;
  FDataLink.Control := Self;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TdbBitBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TdbBitBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

{------------------------------------------------------------------------------}
{ PLUMBING AND READ-ONLY                                                       }
{------------------------------------------------------------------------------}

function TdbBitBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TdbBitBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TdbBitBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TdbBitBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TdbBitBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TdbBitBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TdbBitBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

procedure TdbBitBox.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    Numeric := FDataLink.Field.AsInteger
  else
    if csDesigning in ComponentState then Numeric := 0;
end;

procedure TdbBitBox.ChangeSelected(Sender:TObject);
begin
  inherited ChangeSelected(Sender);
  if FDataLink.Field <> nil then
    if not (csDesigning in ComponentState) then
      UpdateData(Sender);
end;

procedure TdbBitBox.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TdbBitBox.UpdateData(Sender: TObject);
begin
  if Numeric<>FDataLink.Field.AsInteger then
    if FDataLink.Edit then
      FDataLink.Field.AsInteger:= Numeric;
end;


{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

end.

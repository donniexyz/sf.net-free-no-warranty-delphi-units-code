unit tdmemof;
(*
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
*)

interface

uses
  SysUtils, Messages, Classes, Graphics, 
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls, 
  DBCtrls, DB, DBTables, Windows, 
  utForm, Toolbar, Restorer, UpdateOk, IniLink, tpAction;

type
  TDbMemoForm = class(TtpFitForm)
    FormRestorer: TFormRestorer;
    IniFileLink: TIniFileLink;
    Panel1: TPanel;
    DBMemo: TDBMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TtpDbMemoButton = class(TtpToolButton)
  private
    fDbMemoForm: TDbMemoForm;
    fDataLink: TFieldDataLink;
    function GetField: TField;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure RippleUp;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(aOwner:TComponent); Override;
    destructor Destroy; Override;
    procedure Click; Override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    end;

{----------------------------------------------------------------------------------------}

//procedure Register;

implementation

{$R *.DFM}

{----------------------------------------------------------------------------------------}

constructor TtpDbMemoButton.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  FDataLink:= TFieldDataLink.Create;
end;

destructor TtpDbMemoButton.Destroy;
begin
  if fDbMemoForm<>nil then
    fDbMemoForm.Close; {saves position}
  fDbMemoForm.Free;
  fDataLink.Free;
  inherited Destroy;
end;

procedure TtpDbMemoButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csUpdating in ComponentState) then
    exit;
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

{------------------------------------------------------------------------------}

procedure TtpDbMemoButton.Click;
begin
  inherited Click;
  if fDbMemoForm=nil then begin
    fdbMemoForm:= TDbMemoForm.Create(Self);
    RippleUp;
    end;
  with fdbMemoForm do
    if Visible then
      Hide
    else
      Show;
end;

procedure TtpDbMemoButton.RippleUp;
begin
  if (fDbMemoForm<>nil) then
    with fDbMemoForm do begin
      with DBMemo do begin
        DataSource:=Self.DataSource;
        DataField:=Self.DataField;
        end;
      if (Caption<>Self.Field.DisplayLabel) then begin
        FormRestorer.Save;
        Caption:=Self.Field.DisplayLabel;
        FormRestorer.Load;
        end;
      end;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

function TtpDbMemoButton.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TtpDbMemoButton.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  RippleUp;
end;

function TtpDbMemoButton.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TtpDbMemoButton.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
  RippleUp;
end;

function TtpDbMemoButton.GetField: TField;
begin
  Result := FDataLink.Field;
end;

//----------------------------------------------------------------------
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtpDbMemoButton]);
//end;
//----------------------------------------------------------------------
end.

unit dbstrgrd;

(*
Permission is hereby granted, on 16-Aug-2005, free of charge, to any person
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
  SysUtils, Messages, Classes, Graphics, Controls
{$IFDEF WIN32}
, Windows
{$ELSE}
, WinProcs, WinTypes
{$ENDIF}
, Forms
, Dialogs
, DB
, StdCtrls
, ExtCtrls
, Grids
, TxtGrid;

type
  TInstantForm = class(TTextGrid)
  private
    fPanel: TPanel;
    fDataSource: TDataSource;
    fSuspend: Boolean;
  protected
    procedure ShowDragState(a:string;x,y:integer);
    function DragPos(x,y:integer):string;
    function DragRowCol(r,c:integer):string;
    procedure ManageChange;
  public
    constructor Create(aOwner:TComponent); override;
    procedure Loaded; override;
    procedure Init;
    {procs for default events}
    procedure goDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure goDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure goEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure goRowMoved(Sender: TObject; FromIndex, ToIndex: Longint);
    procedure goSelectCell(Sender: TObject; Col, Row: Longint; var CanSelect: Boolean);
    procedure goClick(Sender: TObject);
    {procs to patch into linked components}
    procedure goDataSourceDataChange(Sender: TObject; Field: TField);
    procedure goDataSourceStateChange(Sender: TObject);
  published
    property Panel: tPanel read fPanel write fPanel;
    property DataSource: TDataSource read fDataSource write fDataSource;
    end;

{------------------------------------------------------------------------------}

//procedure Register;

implementation

{------------------------------------------------------------------------------}

procedure TInstantForm.ShowDragState(a:string;x,y:integer);
begin
  if fPanel<>nil then
    fPanel.Caption:=a+' @ '+dragPos(x,y);
end;

function TInstantForm.DragPos(x,y:integer):string;
begin
  result:=IntToStr(x)+'x, '+IntToStr(y)+'y'
end;

function TInstantForm.DragRowCol(r,c:integer):string;
begin
  result:='row:'+IntToStr(r)+', col:'+IntToStr(c);
end;

procedure TInstantForm.goDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  ShowDragState('DragDrop',x,y);
end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

procedure TInstantForm.goDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  a:string;
  r,c:longint;
begin
  case State of
    dsDragEnter: a:='Enter';
    dsDragMove:  a:='Move';
    dsDragLeave: a:='Leave';
    end;
  //r:=(y-1) div DefaultRowHeight;
  //c:=x div DefaultColWidth;
  //Accept:=(c=0) and (r>0);
  MouseToCell(x,y,c,r);
  Accept:=(c>fixedcols) and (r>fixedrows);
  ShowDragState('Drag'+a+' '+DragRowCol(r,c),x,y);   //MouseToCell
end;

procedure TInstantForm.goEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var
  r:integer;
begin
  if (x>0) and (y>0) then begin
    r:=(y-1) div DefaultRowHeight;
    ShowDragState('EndDrag '+DragRowCol(r,0),x,y);
    end;
end;

procedure TInstantForm.goRowMoved(Sender: TObject; FromIndex,
  ToIndex: Longint);
begin
  fDataSource.DataSet.Fields[FromIndex-1].Index := ToIndex-1;
end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

constructor TInstantForm.Create(aOwner:TComponent);
begin
  inherited create(aOwner);
  ColCount:= 2;
  Options:=[goFixedHorzLine, goFixedVertLine, goHorzLine, goVertLine
           ,goRangeSelect, goDrawFocusSelected, goColSizing, goRowMoving
           ,goColMoving, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking];
end;

procedure TInstantForm.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then begin
    OnClick := goClick;
    OnDragDrop := goDragDrop;
    OnDragOver := goDragOver;
    OnEndDrag := goEndDrag;
    OnRowMoved := goRowMoved;
    OnSelectCell := goSelectCell;
    end;
  Init;
end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

procedure TInstantForm.Init;
begin
  if fdatasource<>nil then
    with fDataSource do begin
      OnDataChange:=goDataSourceDataChange;
      OnStateChange:=goDataSourceStateChange;
      if (Dataset<>nil) and Dataset.Active then
        goDataSourceDataChange(Self,nil);
      end;
end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}
procedure TInstantForm.goDataSourceStateChange(Sender: TObject);
begin
end;

procedure TInstantForm.goDataSourceDataChange(Sender: TObject; Field: TField);
var
  i,n,r:integer;
  a:string;
begin
  if fSuspend then
    exit;
  {SuspendFit}
  with datasource.dataset do begin
    FitCells[0,0]:=Name;
    FitCells[1,0]:='Current Value';
    r:=FixedRows;
    n:=fieldcount;
    for i:= 1 to n do
      with fields[i-1] do
        if visible then begin
          a:=DisplayLabel;
          if a='' then
            a:=FieldName;
          FitCells[0,r]:=a;
          FitCells[1,r]:=AsString;
          r:=r+1;
          end;
    RowCount:=r;
    end;
 end;

{------------------------------------------------------------------------------}
{                                                                              }
{------------------------------------------------------------------------------}

procedure TInstantForm.goClick(Sender: TObject);
begin
  ManageChange;
end;

procedure TInstantForm.goSelectCell(Sender: TObject; Col, Row: Longint;
  var CanSelect: Boolean);
begin
  CanSelect:=True;
  ManageChange;
end;

procedure TInstantForm.ManageChange;         //hidelinkingkeys  //tptable
var
  i,j,n,k:integer;
begin
  i:=row-FixedRows;
  n:=0;
  with fDataSource.DataSet do begin
    k:=pred(FieldCount);
    for j:=0 to k do
      with fields[j] do
        if visible then
          if n=i then
            //j now points to the i'th visible field
            break
          else
            inc(n);
    if j>k then
      exit; //otherwise refers to an invalid field
    end;
  with fDataSource.DataSet,fDataSource.DataSet.Fields[j] do
    if AsString <> cells[col,row] then
      try
        if State=dsEdit then
          AsString:=cells[col,row]
        else begin
          fSuspend:=True;
          Edit; {refreshes dataset and updates ctrls}
          fSuspend:=False;
          AsString:=cells[col,row]; {again updates dataset}
          {cells[col,row]:=AsString;}
          end;
      except
        cells[col,row]:=AsString;
        end;
end;

{------------------------------------------------------------------------------}
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TInstantForm]);
//end;
{----------------------------------------------------------------------------------------}

end.

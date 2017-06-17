unit Dbload;

{ Author: Michael Ax, HREF Tools Corp.
  February 7, 1996.
  Copyright (c) 1995 HREF Tools Corp.  All Rights Reserved.

  For updates & other news, please visit our web site at http://www.href.com/.
  Send eMail to code@href.com for technical support.

  Our License Agreement for Free and Trial-Basis items is posted on the web.
  Please <A HREF="http://www.href.com/pub/freelicn.html">Read It Now</A>.
  If you agree to be bound to all its terms you may use our software,
  otherwise you may not.  Expiration dates do apply to the usage of some
  files.

  TPack can be downloaded from ftp.sonic.net/pub/mall/moreDelphi
  Documentation for TPack classes are at www.href.com/compdocs/

}

{----------------------------------------------------------}

{ This project demonstrates how to build a string list from
  items in a table.  Two techniques are shown:

  1. Using a regular Delphi TTable object
  2. Using a TTableWithRetry from TPack, with the onScan event

  The string list is memo1.lines.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, DB, TblRetry, DBTables, DBCtrls,
  Grids, DBGrids, Buttons;

type
  TForm1 = class(TForm)
    Table1: TTable;
    TableWithRetry1: TTableWithRetry;
    DataSource1: TDataSource;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Notebook1: TNotebook;
    Memo1: TMemo;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeciesNo: TFloatField;
    Category: TStringField;
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TableWithRetry1Scan(Sender: TTableWithRetry);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{----------------------------------------------------------}
{goofy timing code -- in our experience, the timing stats
 are not consistent from one run to the next, and do not
 indicate a clear speed winner.  Both approaches take about
 the same number of milliseconds (320 approx.) }

var
  Timing: Longint;

procedure BeginOp;
begin
  Timing:= GetTickCount;
end;

procedure EndOp;
begin
  with Form1.Memo1.Lines do
    Add('ThisTook='+IntToStr(GetTickCount-Timing)+'ms');
end;

{----------------------------------------------------------}
{demoing ttable scan using 'found' fields}

procedure TForm1.Button1Click(Sender: TObject);
var
  SpeciesNo:tField;  {note tField datatype here}
  Category:tField;
begin
  BeginOp;
  with Memo1 do begin
    Clear;
    with Table1 do begin
      First;
      SpeciesNo:=FindField('Species No');  {connect pointer to correct field}
      Category:=FindField('Category');
      repeat
        Lines.Add(SpeciesNo.AsString+'='+Category.AsString);
        Next;
      until eof;
      end;
    end;
  EndOp;
end;


{----------------------------------------------------------}
{demoing tTableWithRetry's scan function}
{Setup for this involves double-clicking the TableWithRetry1
 component and setting up field objects for the relevant
 fields.  Note that the field names are the same as the
 vars used in the previous example, but that is not
 necessary.}

procedure TForm1.Button2Click(Sender: TObject);
begin
  BeginOp;
  with Memo1 do
    Clear;
  with TableWithRetry1 do
    Scan;   {this initiates a while-not-eof loop, with an onScan
             event for each record.}
  EndOp;
end;

procedure TForm1.TableWithRetry1Scan(Sender: TTableWithRetry);
begin
  {the TTableWithRetry component calls this event once for
   each record.
   Remember that here, SpeciesNo is a TField object that already
   points at the correct field.}
  with memo1 do
    Lines.Add(SpeciesNo.AsString+'='+Category.AsString);
end;

{----------------------------------------------------------}
{cosmetics}

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Notebook1.PageIndex:=0;
  SpeedButton1.Down:=True;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  Notebook1.PageIndex:=1;
  SpeedButton2.Down:=True;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  close;
end;

end.

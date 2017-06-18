unit Smileyf;

{ Authors: Michael Ax and Ann Lynnworth.
  Copyright (c) 1996 HREF Tools Corp.  All Rights Reserved.
  Smiley used with permission from Nick Hodges.
  Distributed with permission in Delphi 2.0 In Depth, Cary Jensen, editor.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, TSmile, Buttons;

type
  TForm1 = class(TForm)
    Smiley: TSmiley;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

var             {this part just for fun..}
  x:tBitmap;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  t:tDateTime;
begin
  x:=TBitmap.Create;
  x.Handle := LoadBitmap(hInstance,'SMDEAD');
  Smiley.Picture.Graphic := TGraphic(x);
  Smiley.Update;
  t:=now; while t=now do;
  t:=now; while t=now do;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  smiley.mood:=smhappy;
  x.free;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  close;
end;

end.

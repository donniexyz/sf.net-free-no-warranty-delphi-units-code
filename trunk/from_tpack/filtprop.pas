unit filtprop;

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

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TypInfo, Buttons, Toolbar, ExtCtrls,
  Windows,
  ucTypes, utForm, Bitbox;

type
  TFiltPropForm = class(TtpFitForm)
    Toolbar1: TtpToolbar;
    VarsButton: TtpToolButton;
    ClassesButton: TtpToolButton;
    MethodsButton: TtpToolButton;
    AllButton: TtpToolButton;
    pnlBG: TPanel;
    pnlFooter: TPanel;
    pnlButtons: TPanel;
    btOK: TButton;
    btCancel: TButton;
    BitBox1: TBitBox;
    procedure FormCreate(Sender: TObject);
    procedure AllButtonClick(Sender: TObject);
    procedure VarsButtonClick(Sender: TObject);
    procedure ClassesButtonClick(Sender: TObject);
    procedure MethodsButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    k:TTypeKinds;
  end;

var
  FiltPropForm: TFiltPropForm;

implementation

uses DocProps;

{$R *.DFM}

procedure TFiltPropForm.FormCreate(Sender: TObject);
var
  aKind:TTypeKind;
begin
  with BitBox1 do begin
    Captions.Clear;
    K:=[];
    for aKind:=low(TTypeKind) to high(TTypeKind) do begin
      K:=K + [aKind];
      Captions.Add(
        Copy(
          GetEnumName(TypeInfo(TTypeKind),ord(aKind))
          ,3,255));
      end;
//    Possible:=TWordSet(k);
    end;
  {$IFDEF VER100}
  Height:=Height+28; //one more row
  {$ENDIF}
end;

procedure TFiltPropForm.AllButtonClick(Sender: TObject);
begin
  BitBox1.Possible:=TWordSet(k); //TWordSet(AllFilter);
end;

procedure TFiltPropForm.VarsButtonClick(Sender: TObject);
begin
  BitBox1.Possible:=TWordSet(VarFilter);
end;

procedure TFiltPropForm.ClassesButtonClick(Sender: TObject);
begin
  BitBox1.Possible:=TWordSet(ClassFilter);
end;

procedure TFiltPropForm.MethodsButtonClick(Sender: TObject);
begin
  BitBox1.Possible:=TWordSet(MethodFilter);
end;

end.

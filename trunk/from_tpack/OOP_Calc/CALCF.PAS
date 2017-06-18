unit Calcf;

{ Author: Michael Ax
  Copyright (c) 1996 HREF Tools Corp. 
  Written for distribution with _Delphi 2.0 In-Depth_, Cary Jensen, Editor.
}

(*
Permission is hereby granted, on 03-Oct-1996, free of charge, to any person
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
*)

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, UpdateOk, StdCtrls,
  TTwoNum, Buttons {component};

type
  TForm1 = class(TForm)
    TwoNumbers1: TTwoNumbers;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    procedure FormClick(Sender: TObject);
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

procedure TForm1.FormClick(Sender: TObject);
begin
  TwoNumbers1.Edit;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  close;
end;

end.

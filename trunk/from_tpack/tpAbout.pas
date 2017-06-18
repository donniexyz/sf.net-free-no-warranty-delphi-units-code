unit tpAbout;
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

(*
using the about box in tpmenu:

	you need to manually 'poke' the address of your aboutbox method into tpmenu.aboutbox.
	see the unit ToolProc.pas which does that, and include it in your project to get the 
	default tpaboutbox when using a tpmenu.

*)

interface

uses SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Buttons, 
  ExtCtrls, Windows, 
  UpdateOk, tpAction, utForm, IniLink, UserWin;

type
  TtPackAboutBox = class(TTpAction)
  private
  protected
  public
    constructor Create(aOwner:TComponent); override;
    procedure Execute; Override;
  published
    end;

  TtPackAboutBoxForm = class(TtpFitForm)
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    Panel3: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
    end;

{------------------------------------------------------------------------------}

procedure AboutBox;

//procedure Register;

implementation

{------------------------------------------------------------------------------}

uses
  tpmenu;

{$R *.DFM}

const cShipTime='7/01/96 3:25:00 AM';  // mm/dd/yy

 {------------------------------------------------------------------------------}

procedure TtPackAboutBoxForm.FormCreate(Sender: TObject);
begin
  Version.Caption:='Version '+cShipTime;
end;

constructor TtPackAboutBox.Create(aOwner:TComponent);
begin
  inherited create(aOwner);
end;

procedure AboutBox;
begin
  try
    with TtPackAboutBoxForm.Create(Application) do try
      ShowHint:=True;
      ShowModal;
    finally
      Free;
      end;
  except
    end;
end;

procedure TtPackAboutBox.Execute;
begin
  { Create dialog in memory }
  AboutBox;
end;

//----------------------------------------------------------------------
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtPackAboutBox]);
//end;
//----------------------------------------------------------------------
initialization
  tpmenu.AboutBoxProc:=AboutBox;
finalization
//  if tpmenu.AboutBoxProc=AboutBox then
//    tpmenu.AboutBoxProc:=nil;
end.


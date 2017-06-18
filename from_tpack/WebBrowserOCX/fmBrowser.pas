unit fmBrowser;

(*
Permission is hereby granted, on 01-May-1997, free of charge, to any person
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OleCtrls, shdocvw, Buttons, Toolbar, ExtCtrls, StdCtrls;

type
  TfmWebBrowser = class(TForm)
    tpToolBar2: TtpToolBar;
    tpToolButton1: TtpToolButton;
    url: TEdit;
    WebBrowserOCX1: TWebBrowserOCX;
    procedure tpToolButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure WebBrowserOCX1BeforeNavigate(Sender: TObject;
      const URL: string; Flags: Integer; const TargetFrameName: string;
      var PostData: Variant; const Headers: string; var Cancel: Wordbool);
    procedure WebBrowserOCX1TitleChange(Sender: TObject;
      const Text: string);
  private
    { Private declarations }
    fActivatedOnce:Boolean;
  public
    { Public declarations }
  end;

var
  fmWebBrowser: TfmWebBrowser;

implementation

{$R *.DFM}

procedure TfmWebBrowser.FormCreate(Sender: TObject);
begin
  fActivatedOnce:=false;
  with WebBrowserOCX1 do begin
    align:=alclient;
//    OnBeforeNavigate:=WebBrowserOCX1BeforeNavigate;
//    OnTitleChange:=WebBrowserOCX1TitleChange;
    end;
end;

procedure TfmWebBrowser.FormActivate(Sender: TObject);
begin
  if fActivatedOnce then
    exit;
  fActivatedOnce:=True;
  //
  with WebBrowserOCX1 do begin
    tpToolButton1Click(Sender);
    end;
end;

procedure TfmWebBrowser.tpToolButton1Click(Sender: TObject);
//Navigate(const URL: string; var Flags, TargetFrameName, PostData, Headers: Variant);
var
  Flags,
  TargetFrameName,
  PostData,
  Headers: Variant;
begin
  Flags:=null;
  TargetFrameName:=null;
  PostData:=null;
  Headers:=null;

  with WebBrowserOCX1 do begin
    Navigate(url.text, Flags, TargetFrameName, PostData, Headers);
    end;
end;

(*

not sure how to get the events to come in at this time..

procedure TForm1.WebBrowserOCX1BeforeNavigate(Sender: TObject;
  const URL: string; Flags: Integer; const TargetFrameName: string;
  var PostData: Variant; const Headers: string; var Cancel: Wordbool);
begin
  cancel:=pos('http://www.href.com/',lowercase(url))=0;
end;

procedure TForm1.WebBrowserOCX1TitleChange(Sender: TObject;
  const Text: string);
begin
  with WebBrowserOCX1 do begin
    if pos('http://www.href.com/',lowercase(LocationURL))=0 then begin
      GoBack;
      beep;
      end;
    end;
end;

*)

end.

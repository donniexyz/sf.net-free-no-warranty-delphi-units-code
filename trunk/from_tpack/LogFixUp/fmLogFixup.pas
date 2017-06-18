unit fmLogFixup;

(*
Permission is hereby granted, on 01-Jan-2003, free of charge, to any person
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
  Buttons, Toolbar, ExtCtrls, StdCtrls, UpdateOk, tpAction, IniLink;

type
  TPruneLogs = class(TForm)
    ListBox1: TListBox;
    tpToolButton1: TtpToolButton;
    tpToolButton3: TtpToolButton;
    tpToolBar2: TtpToolBar;
    LoadList: TtpToolButton;
    FileMask: TComboBox;
    SpeedButton1: TSpeedButton;
    IniFile: TIniFileLink;
    OpenDialog: TOpenDialog;
    cbNoCmd: TCheckBox;
    cbNoImg: TCheckBox;
    procedure tpToolButton1Click(Sender: TObject);
    procedure LoadListClick(Sender: TObject);
    procedure tpToolButton3Click(Sender: TObject);
    procedure ProcessFile(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FileMaskExit(Sender: TObject);
    procedure FileMaskKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    bRunning: boolean;
    fPath: String;
  public
    { Public declarations }
  end;

var
  PruneLogs: TPruneLogs;

implementation

{$R *.DFM}

uses
  ucScnDir, ucFile, ucString;

procedure TPruneLogs.tpToolButton1Click(Sender: TObject);
begin
  if bRunning then begin
    bRunning:=False;
    tpToolButton1.Caption:='Convert one file';
    exit; //don't actually enter the loop again!
    end
  else begin
    bRunning:=True;
    tpToolButton1.Caption:='STOP!';
    end;
  ProcessFile(Sender);
  tpToolButton1.Caption:='Convert one file';
end;

procedure TPruneLogs.ProcessFile(Sender: TObject);
var
  fIn,fOut1,fOut2:textfile;
  Buf: array[0..128*1024] of Char;
  i,iNextTick: integer;
  a1,a2:string;

  procedure OpenFile;
  var
    a1,a2,a3:string;
  begin
    screen.cursor:=crHourGlass;
    //
    with listbox1 do
      a1:=fPath+'\'+items[itemindex];
    //
    assignfile(fIn,a1);
    system.SetTextBuf(fIn, Buf);
    reset(fIn); //open
    //
    if cbNoImg.checked then begin
      splitright(a1,'\',a1,a2);
      a1:=a1+'\noimg';
      ForceDirectories(a1);
      assignfile(fOut1,a1+'\'+a2);
      rewrite(fOut1);
      end;
    //
    if cbNoCmd.checked then begin
      splitright(a1,'\',a1,a3);
      a1:=a1+'\nocmd';
      ForceDirectories(a1);
      assignfile(fOut2,a1+'\'+a2);
      rewrite(fOut2);
      end;
  end;

  procedure ReadNextLine;
  var
    ao1,ao2,a1,a2,a3,a4,a5:string;
    i:integer;
  begin
    Readln(fIn,a1);          
    if splitstring(a1,', GET, ',a2,a3) then begin
      ao1:='';
      ao2:='';
      splitright(a2,', ',a4,a5);
      splitright(a4,', ',a4,a5);
      if a5<>'200' then
        exit; //ignore 404 etc, only use 200's here.
      splitstring(a3,', ',a3,a4);
      if (pos('.gif',a3)=0)
      and (pos('.jpg',a3)=0)
      and (pos('.jpeg',a3)=0)
      and (pos('.png',a3)=0) then begin
        ao1:=a1; //:=a2+', GET, '+a3+', '+a4;
        if (pos('.dll',a3)>0) then begin
          a4:=StringReplace(a4,'%3a',':');
          if splitstring(a4,':',a3,a4) then
            ao2:=a2+', GET, '+a3+':'+leftof(',',leftof(':',a4))+', -, '
          else
            ao2:=a1;
          end
        else
          ao2:=a1;
        end;
      end
    else
    if splitstring(a1,', POST, ',a2,a3) then begin
      ao1:=a1;
      splitstring(a3,', ',a3,a4);
      if (pos('.dll',a3)>0) then begin
        a4:=StringReplace(a4,'%3a',':');
        splitthree(a4,':',a3,a4,a5);
        ao2:=a2+', POST, '+a3+':'+a4+', ';
        end;
      end
    else begin
      ao1:=a1;
      ao2:=a1;
      end;
    if cbNoImg.checked and (ao1<>'') then writeln(fOut1,ao1);
    if cbNoCmd.checked and (ao2<>'') then writeln(fOut2,ao2);
    //
  end;

  procedure CloseIt;
  begin
    tpToolButton3.Caption:='Convert Log files';
    tpToolButton1.Caption:='Convert one file';
    CloseFile(fIn);
    if cbNoImg.checked then CloseFile(fOut1);
    if cbNoCmd.checked then CloseFile(fOut2);
    //
    screen.cursor:=crDefault;
  end;

begin
  //
  OpenFile;
  iNextTick:=0;
  try
    while bRunning
     and (not application.terminated)
     and (not eof(fIn))
     do begin
      ReadNextLine;
      i:=GetTickCount;
      if i>iNextTick then begin
        Application.ProcessMessages;
        iNextTick:=GetTickCount+100;
        end;
      end;
  finally
    CloseIt;
    end;
end;

function Addfile(const Filename:String):boolean;
//function Addfile(const Filename:String;aObject:TObject):boolean;
var
  a1,a2:string;
begin
  result:=true;
  splitright(filename,'\',a1,a2);
  PruneLogs.listbox1.items.add(a2);
//  tForm1(aObject).listbox1.items.add(a2);
end;

procedure TPruneLogs.LoadListClick(Sender: TObject);
var
  a1:string;
begin
  fPath:=FileMask.text;
  splitright(fPath,'\',fPath,a1);
  listbox1.items.clear;
  ScanDirectory(fPath,a1,Addfile,False);
  listbox1.itemIndex:=0;
//  ScanDirectory(edit1.text,'*.log',Addfile,Form1);
end;

procedure TPruneLogs.tpToolButton3Click(Sender: TObject);
var
  i:integer;
begin
  if bRunning then begin
    bRunning:=False;
    tpToolButton3.Caption:='Convert Log files';
    exit;
    end
  else begin
    bRunning:=True;
    tpToolButton3.Caption:='STOP!';
    end;
  //
  with listbox1 do
    for i:=0 to pred(items.count) do begin
      ItemIndex:=i;
      ProcessFile(Sender);
      if not bRunning then
        break;
      end;
  tpToolButton3.Caption:='Convert Log files';
end;

procedure TPruneLogs.SpeedButton1Click(Sender: TObject);
var
  a1,a2,a3:string;
begin
  with OpenDialog do begin
    splitright(FileMask.text,'\',a1,a2);
    InitialDir:=a1;
    FileName:=a2;
    if Execute then begin
      splitright(FileName,'\',a1,a2);
      splitright(a2,'.',a2,a3);
      FileMask.Text:=a1+'\*.'+a3;
      FileMaskExit(Sender);
      end;
    end;
end;

//

procedure TPruneLogs.FileMaskExit(Sender: TObject);
var
  i:integer;
  b:boolean;
  a1:string;
begin
  with FileMask do begin
    a1:=text;
    with Items do begin
      b:=false;
      for i:=0 to pred(count) do begin
        b:=comparetext(strings[i],a1)=0;
        if b then
          break;
        end;
      if not b then begin
        insert(0,lowercase(a1));
        if Count>DropDownCount then
          delete(pred(count));
        end;
      end;
    end;
end;

procedure TPruneLogs.FileMaskKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift=[]) and (Key=Vk_Return) then begin
    LoadList.Click;
    Key:=0;
    end;
end;

procedure TPruneLogs.FormCreate(Sender: TObject);
var
  i:integer;
begin
  with IniFile do begin
    Section:='Defaults';
    with FileMask do begin
      Text:=StringEntry['FileMask'];
      DropDownCount:=LongIntEntry['DropDownCount'];
      if DropDownCount=0 then
        DropDownCount:=10;
      ReadSection('FileMasks',Items);
      end;
    end;
  with FileMask.Items do
    for i:=0 to pred(count) do
      strings[i]:=rightOf('=',strings[i]);
end;

procedure TPruneLogs.FormDestroy(Sender: TObject);
var
  i:integer;
begin
  with IniFile do begin
    Section:='Defaults';
    StringEntry['FileMask']:=FileMask.Text;
    LongIntEntry['DropDownCount']:=FileMask.DropDownCount;
    with FileMask.Items do
      for i:=0 to pred(count) do
        strings[i]:=inttostr(i)+'='+strings[i];
    WriteSection('FileMasks',FileMask.Items);
    end;
end;

end.

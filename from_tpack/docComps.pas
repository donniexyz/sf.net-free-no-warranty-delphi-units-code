unit docComps; {Run-time type information, mostly readonly}
(*
Permission is hereby granted, on 1-Aug-2005, free of charge, to any person
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

{$I hrefdefines.inc}
{
  Master copy of hrefdefines.inc is versioned on Source Forge in the ZaphodsMap project:
  https://sourceforge.net/p/zaphodsmap/code/HEAD/tree/trunk/ZaphodsMap/Source/hrefdefines.inc
}

uses
{$IFDEF MSWINDOWS}
  Windows, Forms, Controls, Graphics, ExtCtrls, Dialogs, Messages, Grids,
  Buttons,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QControls, QGraphics, QExtCtrls, QDialogs, QGrids, QButtons,
{$ENDIF}
  SysUtils, Classes, toolbar, TypInfo, updateOK, tpAction, ucInteg, utForm,
  tpList, txtGrid, docProps;

{----------------------------------------------------------------------------------------}

type
  TtpPropertyViewer = class(TtpAction)
  private
    fPersistent: TPersistent;
  protected
    procedure DoExecute; Override;
    function  GetPersistent:TComponent;
    procedure SetPersistent(Value:TComponent);
  public
    constructor Create(aOwner:Tcomponent); Override;
    procedure  Notification(AComponent: TComponent; Operation: TOperation); Override;
    property Persistent: TPersistent read fPersistent write fPersistent;
  published
    property Component: TComponent read GetPersistent write SetPersistent stored false;
  end;

{----------------------------------------------------------------------------------------}

type
  TDocComponentsForm = class(TtpDocForm)
    Toolbar1: TtpToolbar;
    ToolButton1: TtpToolButton;
    ToolButton2: TtpToolButton;
    pnlBG: TPanel;
    HeaderGrid: TTextGrid;
    StringGrid1: TTextGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure DetailButtonClick(Sender: TObject);
    procedure ScreenButtonClick(Sender: TObject);
    procedure ApplicationButtonClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    fPropertyViewer: TTpPropertyViewer;
    function  GetComponent: TComponent;
    procedure SetComponent(Value:TComponent);
  published
    property Component: TComponent read GetComponent write SetComponent;
    end;

  TtpComponentViewer = class(TtpAction)
  public
    fComponent: TComponent;
    fPropertyViewer: TtpPropertyViewer;
  protected
    procedure DoExecute; Override;
  public
    constructor Create(aOwner:Tcomponent); Override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); Override;
  published
    property Component: TComponent read fComponent write fComponent;
    property PropertyViewer: TtpPropertyViewer read fPropertyViewer write fPropertyViewer;
  end;

{----------------------------------------------------------------------------------------}

//procedure Register;

implementation

uses
  Math;
  
{----------------------------------------------------------------------------------------}

{$R *.dfm}

{-----------------------------------------------------------------------------------------}

constructor TtpPropertyViewer.Create(aOwner:Tcomponent);
begin
  inherited Create(aOwner);
end;

procedure TtpPropertyViewer.DoExecute;
begin
  if fPersistent=nil then
    fPersistent:=Application;
  inherited DoExecute;
  with TComponentPropForm.Create(application) do try
    Persistent:=Self.fPersistent;
    Execute
  finally
    Free;
    end;
end;

procedure TtpPropertyViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csUpdating in ComponentState) then
    exit;
  if (Operation=opRemove)
  and (fPersistent<>nil)
  and (fPersistent=tPersistent(AComponent)) then
    fPersistent:=nil;
end;

{-----------------------------------------------------------------------------------------}

function TtpPropertyViewer.GetPersistent:TComponent;
begin
  Result:=nil;
  if (fPersistent<>nil) and (fPersistent is tComponent) then
    Result:=tComponent(fPersistent);
end;

procedure TtpPropertyViewer.SetPersistent(Value:TComponent);
begin
  fPersistent:=value;
end;

{-----------------------------------------------------------------------------------------}
{-----------------------------------------------------------------------------------------}

constructor TtpComponentViewer.Create(aOwner:Tcomponent);
begin
  inherited Create(aOwner);
end;

procedure TtpComponentViewer.DoExecute;
begin
  if fComponent=nil then
    fComponent:=self;
  cx.MakeIfNil(fPropertyViewer,TtpPropertyViewer);
  inherited DoExecute;
  with TDocComponentsForm.Create(self) do try
    fPropertyViewer:=Self.PropertyViewer;
    ShowModal;
  finally
    Free;
    end;
end;

procedure TtpComponentViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (csUpdating in ComponentState) then
    exit;
  if (Operation=opRemove) then begin
    cx.NilIfSet(fComponent,AComponent);
    cx.NilIfSet(fPropertyViewer,AComponent);
    end;
end;

{-----------------------------------------------------------------------------------------}
{                                                                                         }
{-----------------------------------------------------------------------------------------}
procedure TDocComponentsForm.FormCreate(Sender: TObject);
begin
  if fPersistent=nil then
    fPersistent:=tComponent(Sender);
end;

procedure TDocComponentsForm.ToolButton1Click(Sender: TObject);
begin
  close;
end;

{-----------------------------------------------------------------------------------------}

function TDocComponentsform.GetComponent: TComponent;
begin
  Result:=TComponent(fPersistent);
end;

procedure TDocComponentsForm.SetComponent(Value:TComponent);
begin
  if TPersistent(value)<>fPersistent then
    fPersistent:=Value;
end;

{-----------------------------------------------------------------------------------------}

procedure TDocComponentsForm.FormActivate(Sender: TObject);
var
  i,n:integer;
  TypeData: PTypeData;
begin
  if fPersistent=nil then
    fPersistent:=Application;
  TypeData:=GetTypeData(fPersistent.ClassInfo);
  with HeaderGrid do begin
    ColCount:=3;
    ResetColumnWidths;
    FitCells[0,0]:='ClassName';
    FitCells[1,0]:='Properties';
    FitCells[2,0]:='Unit';
    i:=TypeData^.PropCount;
    FitCells[0,1]:=TypeData^.ClassType.ClassName;
    FitCells[1,1]:=IntToStr(i);
    FitCells[2,1]:=TypeData^.UnitName;
    end;
  with StringGrid1 do begin
    ColCount:=6;
    ResetColumnWidths;
    FitCells[0,0]:='#';
    FitCells[1,0]:='Class';
    FitCells[2,0]:='Name';
    FitCells[3,0]:='Value';
    FitCells[4,0]:='Components';
    FitCells[5,0]:='Controls';
    with TComponent(fPersistent) do begin
      n:=ComponentCount;
      RowCount:=Math.Max(2,n+1);
      for i:=1 to n do
        with components[i-1] do begin
          FitCells[0,i]:=inttostr(i);
          FitCells[1,i]:=ClassName;
          FitCells[2,i]:=Name;
          FitCells[3,i]:=inttostr(InstanceSize);
          FitCells[4,i]:=inttostr(ComponentCount);
          FitCells[5,i]:=inttostr(ControlCount);
          end;
      end;
    end;
end;

procedure TDocComponentsForm.DetailButtonClick(Sender: TObject);
begin
  with fPropertyViewer do begin
    if ActiveControl<>HeaderGrid then
      Persistent:=fPersistent
    else
      Persistent:=tComponent(fPersistent).Components[StringGrid1.Row-1];
    Execute;
    end;
end;

procedure TDocComponentsForm.ApplicationButtonClick(Sender: TObject);
begin
  with fPropertyViewer do begin
    Persistent:=Application;
    Execute;
    end;
end;

procedure TDocComponentsForm.ScreenButtonClick(Sender: TObject);
begin
  with fPropertyViewer do begin
    Persistent:=Screen;
    Execute;
    end;
end;

//----------------------------------------------------------------------
//procedure Register;
//begin
//  RegisterComponents('TPACK', [TtpComponentViewer,TtpPropertyViewer]);
//end;
//----------------------------------------------------------------------
end.

unit tpDbg;
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

(* Set compiler directive TPDEBUGLOG to activate use of the DebugLog within
   TtpAction.DoExecute in tpaction.pas, and elsewhere. *)

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, Toolbar, ExtCtrls, Windows,
  utForm;

type
  TDebugDlg = class(TtpFitForm)
    FontDialog: TFontDialog;
    Panel1: TPanel;
    Memo: TMemo;
    pnlFooter: TPanel;
    pnlButtons: TPanel;
    btOK: TButton;
    btCancel: TButton;
    Panel2: TPanel;
    ToolButton2: TtpToolButton;
    tpToolButton1: TtpToolButton;
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tpToolButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  TDebugExtendedComponentOptions = (decEnabled, decDesign, decYield, decLogFile
                                  , decCreate, decDestroy, decLoaded, decUpdate
                                  , decRefresh, decExecute, decInsert, decRemove );

  TDebugExtendedComponentStates =  (decActive,decFormError,decDestroying
                                   ,decPrintSet,decPrinting,decPrintError
                                   ,decFiling,decFileError );

  TDebugExtendedComponentFlags = set of TDebugExtendedComponentOptions;
  TDebugExtendedComponentState = set of TDebugExtendedComponentStates;


{using the flags and log procedure other parts of the app can use debugging services.}

procedure DebugOn; export;
procedure DebugOff; export;
procedure DebugLog(Instance:TObject;const Text:String); export;

procedure AdjustDebugFlags(Value:TDebugExtendedComponentFlags); export;

var
  DebugDlg: TDebugDlg;
  DebugFlags:TDebugExtendedComponentFlags;
  DebugState:TDebugExtendedComponentState;


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

implementation

uses
  Restorer, UpdateOk, IniLink, uclogfil, ucwinapi, ucString;

var
  FormRestorer:TFormRestorer;

const
{  DebugLogName= 'c:\debuglog.ini';}
  MaxDebugLines= 300;
  MinDebugLines= MaxDebugLines div 2;

{$R *.DFM}

var
  Count:Longint;
  indent:Byte;

procedure DebugLog(Instance:TObject;const Text:String);
const
  BufSize=144;
var
  offset:byte;
  txt:string;
  b:boolean;

  procedure tOut(const aText:String);
  begin
    with DebugDlg.Memo do
      with Lines do begin
        if Count>MaxDebugLines then begin
          Visible:=False;
          while Count>MinDebugLines do
            Delete(0);
          Visible:=True;
          end;
        try
          add(aText);
        except {ignore?}
          end;
        end;

    if decLogFile in DebugFlags then  //uclogfil ucwinapi
      AppendToLog(WinTempPath+'DebugLog.txt',Text);

    if decYield in DebugFlags then
      Application.ProcessMessages;
  end;

begin

  if not (decEnabled in DebugFlags) or (decDestroying in DebugState) then
    exit;

  if Text='CLOSE' then begin
    if DebugDlg<>nil then begin
      DebugDlg.Close;
      DebugDlg:=nil;
      end;
    exit;
    end;

  if not ((decFormError in DebugState) or (decActive in DebugState)) then
    if not (decFormError in DebugState) then begin
      if DebugDlg=nil then begin
        b:=(decCreate in DebugFlags);
        DebugFlags:=DebugFlags-[decCreate];
        DebugDlg:= TDebugDlg.Create(nil);
        if b then
          DebugFlags:=DebugFlags+[decCreate];
        end
      else {take our chances on the form really really being there already!}
        ;
      try
        with DebugDlg do begin
          with Memo.Lines do begin
            Clear;
            Add('Opened '+datetimetostr(now));
            end;
          OnClose:=FormClose;
          Show;
          end;
      except
        DebugState:=DebugState+[decFormError];
        raise;
        end;
      DebugState:=DebugState+[decActive]
      end;

  if (Instance<>nil) and (Instance is TComponent) then
    if csDesigning in TComponent(Instance).ComponentState then
      if not (decDesign in DebugFlags) then
        exit;

{
    GetModuleFileName(hInstance,p,80);
    Result:=(Pos('.DCL',UpperCase(StrPas(p)))>0)
         or ((Pos('.DPL',UpperCase(StrPas(p)))>0)
             and (uppercase(copy(paramstr(0),1,5))='DELPHI') );
}
{ if (pos('.DCL',paramstr(0))>0) then {do nothing inside library!}
{   if (pos('Create',Text)>0) then
      exit;}

  if Text='CLOSE' then begin
    DebugDlg.Close;
    exit;
    end;

  case Text[1] of
  '[',
  '+',
  '-': offset:=2;
  '^': begin
    DebugDlg.Caption:=copy(text,2,255);
    FormRestorer.Load;
    exit;
    end;
  else
    offset:=1;
  end;

  case Text[1] of
  '[': indent:=indent+2;
  '-': indent:=indent-2;
    end;

  if Instance<>nil then
    if Instance is tComponent then
      with tComponent(Instance) do begin
        if Name<>'' then
          Txt:=Name+'.'+ClassName
        else
          Txt:=ClassName;
        if csAncestor in ComponentState then Txt:=Txt+'(csAncestor)'; //classes
        if csFixups in ComponentState then   Txt:=Txt+'(csFixups)'; //classes
        if csUpdating in ComponentState then Txt:=Txt+'(csUpdating)'; //classes
        if csReading in ComponentState then  Txt:=Txt+'(csReading)'; //classes
        if csWriting in ComponentState then  Txt:=Txt+'(csWriting)'; //classes

        if Owner<>nil then
          with Owner do begin
            if Name<>'' then
              Txt:=Name+'.'+ClassName
            else
              Txt:=ClassName;
            if csAncestor in ComponentState then Txt:=Txt+'(csAncestor)'; //classes
            if csFixups in ComponentState then   Txt:=Txt+'(csFixups)'; //classes
            if csUpdating in ComponentState then Txt:=Txt+'(csUpdating)'; //classes
            if csReading in ComponentState then  Txt:=Txt+'(csReading)'; //classes
            if csWriting in ComponentState then  Txt:=Txt+'(csWriting)'; //classes
            end;
        end
    else
      Txt:=Instance.Classname
  else
    Txt:='';
  if Txt<>'' then
    Txt:=#9' ['+Txt;
  Txt:=copy(Text,offset,255)+Txt;

  Count:=Count+1;
  tOut(inttostr(Count)+'. '+Spaces(Indent)+txt);

//SendToLog(inttostr(Count)+'. '+Spaces(Indent)+txt);

  case Text[1] of
  '[': indent:=indent-2;
  '+': indent:=indent+2;
    end;

end;

{}

procedure DebugOn;
begin
  AdjustDebugFlags([decEnabled]);
end;

procedure DebugOff;
begin
  AdjustDebugFlags([]);
end;

procedure AdjustDebugFlags(Value:TDebugExtendedComponentFlags);
begin
  if not (decEnabled in Value) and (decEnabled in DebugFlags) then begin{turn all off}
    Value:=Value-[decCreate,decDesign,decDestroy,decLoaded,decUpdate,decInsert,decRemove];
    end;
  if (decEnabled in Value) and not (decEnabled in DebugFlags) then begin{turn all on}
    Value:=Value+[decCreate,decDesign,decDestroy,decLoaded,decUpdate,decInsert,decRemove];
    end;

  DebugFlags:=Value;
end;


{-----------------------------------------------------------------------------------------}
{                                                                                         }
{-----------------------------------------------------------------------------------------}

procedure TDebugDlg.FormCreate(Sender: TObject);
begin
  Tag:= (Top shl 16) + Left;
  Top:= -1000;
end;

procedure TDebugDlg.FormActivate(Sender: TObject);
begin
  if FormRestorer=nil then begin
    OnClose:=nil;
    FormRestorer:= TFormRestorer.Create(Self);
    with FormRestorer do begin
      Flags:= [resDefaultIni, resUseCaption];
      Load;
      end;
    OnClose:=FormClose;
    if Top=-1000 then begin
      Top:=tag shr 16;
      Left:=tag and $FFFF;
      end;
    end;
end;

procedure TDebugDlg.ToolButton2Click(Sender: TObject);
begin
  with TtpToolButton(Sender) do
    if down then begin
      Caption:='On';
      AdjustDebugFlags([decEnabled]);
      end
    else begin
      Caption:='Off';
      AdjustDebugFlags([]);
      end;
end;

procedure TDebugDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var
  TempFlags:TDebugExtendedComponentFlags;
begin
  Action:=caFree;
  DebugState:=DebugState-[decActive];
  DebugDlg:=nil;
  if FormRestorer<>nil then begin
    TempFlags:=DebugFlags;
    DebugFlags:=[];
    FormRestorer.Save;
    FormRestorer.IniFileLink.Free;
    FormRestorer.Free;
    FormRestorer:=nil;
    DebugFlags:=TempFlags;
    end;
end;

procedure TDebugDlg.ToolButton1Click(Sender: TObject);
begin
  Close;
end;


{-----------------------------------------------------------------------------------------}
{ INITIALIZATION AND EXIT PROCEDURES                                                      }
{-----------------------------------------------------------------------------------------}

procedure InitializeThisUnit;
var
  i:integer;
  a:string;
begin
  DebugDlg:=nil;
  FormRestorer:=nil;


  DebugFlags:=[ decEnabled, decDesign, decYield {, decLogFile}
              , decCreate, decDestroy, decLoaded, decUpdate
              , decRefresh, decExecute, decInsert, decRemove ];
  DebugFlags:= [];
  DebugState:= [];

//  DebugFlags:= [];
//  DebugState:= [];
 { if csDesigning in ComponentState then exit;}
  {process the commandline to set the unit's globals to the desired DEBUG state.}
  for i:=1 to ParamCount do begin
    a:=uppercase(ParamStr(i));
    if copy(a,1,4)='/Dbg' then begin
      DebugFlags:=DebugFlags+[decEnabled];
      if Length(a)=4 then
        DebugFlags:=DebugFlags+[decCreate,decDesign,decDestroy,decLoaded,decUpdate,decInsert,decRemove]
      else begin
        delete(a,1,4);
        if pos('C',a)>0 then DebugFlags:=DebugFlags+[decCreate];
        if pos('D',a)>0 then DebugFlags:=DebugFlags+[decDesign];
        if pos('L',a)>0 then DebugFlags:=DebugFlags+[decLoaded];
        if pos('U',a)>0 then DebugFlags:=DebugFlags+[decUpdate];
        if pos('I',a)>0 then DebugFlags:=DebugFlags+[decInsert];
        if pos('R',a)>0 then DebugFlags:=DebugFlags+[decRemove];
        if pos('E',a)>0 then DebugFlags:=DebugFlags+[decExecute];
        if pos('F',a)>0 then DebugFlags:=DebugFlags+[decRefresh];
        end;
      exit;
      end;
    end;
end;

{-----------------------------------------------------------------------------------------}

procedure FinalizeUnit;
begin
{  if (decPrint in DebugFlags) or (decFile in DebugFlags) then {turn off}
    AdjustDebugFlags([]); {stores back into global}
end;

{-----------------------------------------------------------------------------------------}
{-----------------------------------------------------------------------------------------}

var
  Initialized: boolean;
{  SaveExit: Pointer =nil;                    { Saves the old ExitProc }

procedure Finalize; far;
begin
{  ExitProc := SaveExit;}
  FinalizeUnit;
end;

procedure Initialize;
begin
  if not Initialized then begin
    AddExitProc(Finalize);
    Initialized:=True;
    DebugFlags:= [];
    DebugState:= [];
    Count:=0;
    Indent:=0;
    InitializeThisUnit;
    end;
end;

procedure TDebugDlg.tpToolButton1Click(Sender: TObject);
begin
  with FontDialog do begin
    font.assign(memo.font);
    if execute then
      memo.font.assign(font);
    end;
end;

initialization
  Initialized := False;
end.

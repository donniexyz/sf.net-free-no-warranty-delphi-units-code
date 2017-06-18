unit tpIdle;
(*
Permission is hereby granted, on 11-May-2003, free of charge, to any person 
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

// Author this file: Michael Ax


{'tpack's complete waste of time'. silly little gadgets to help
out with the horrenduous task of linking actions to idle et all.}
{actually, I kind of like all of these guys for their utility.}

interface

uses Forms, SysUtils, Graphics, Windows
, Classes, Messages, StdCtrls, Controls, ExtCtrls, Consts 
, Toolbar, UpdateOk, ucInteg;

type

{I reworked the vcl timer to support 'IdleOnTime'. When you
use one of these on your forms, IDLE will not be forgotten and
you can use the TtpApplication component to link your idle code
without once refering to the application-object. IOW, this is no
breakthrough component, just a bit of helpful vcl tweaking that
would not have to be done with a little more care in bi's base
class. anyway, here's the silly hack:}

  TtpIdleTimer = class(TComponentExtended)
  private
    fEnabled: Boolean;
    fIdle: Boolean;
    fInterval: Word;
    fWindowHandle: HWND;
    fOnTimer: TNotifyEvent;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Word);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure WndProc(var Msg: TMessage);
  protected
    procedure Timer; dynamic;
    procedure Idle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property IdleOnTimer: Boolean read fIdle write fIdle default True;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Word read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

type

{another lazy man's favorite! plug in one of these and you get an
idea of how much time your app's releasing by the speed of the
color-change. on a more serious note; study this to see how to link
into and out of event procedure chains.}

  TtpIdleDot = class(TtpExtendedPanel)
  private
    fOnIdleHook: TIdleEvent;
    fNextTime: integer;
    fIdleTime: integer;
  protected
    procedure DoIdle(Sender:TObject; var Done:Boolean);
  public
    constructor Create(aOwner:TComponent); Override;
    destructor Destroy; Override;
    procedure Idle; virtual;
    property UpdateDelay:integer read fIdleTime write fIdleTime;
    end;

const
  cIdleDotIdleTime   =   72;//ms
  cIdleTimerInterval = 1000;//ms
  cDotBufferSize     =   64;//square words.

type
  TDotModes = (dotSimple,dotFancy,dotWild);
  TDotBuffer = Array[0..pred(cDotBufferSize*cDotBufferSize)] of word;
  pDotBuffer = ^TDotBuffer;

  TtpIdlerDot = class(TtpIdleDot)
  private
    fDotMode: TDotModes;
    fDotImage: TImage;
    fDotBuffer: pDotBuffer;
    fDotBase: Longint;
  protected
    procedure SetDotMode(Value:TDotModes);
    procedure DotImageClick(Sender: TObject);
    procedure DoFancyDot(Mode:byte);
  public
    constructor Create(aOwner:TComponent); Override;
    destructor Destroy; Override;
    procedure Click; override;
    procedure Idle; Override;
  published
    property DotMode: TDotModes read fDotMode write SetDotMode default low(TDotModes);
    end;


type
{this goes credits to a forgotten name from CIS. I captured the
CreateParams code, and while quite inadequate, it tides you over
when you just have to have things aligned 'normally' for a bit.}

  TEdRight = class(TEdit)
  private
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function  GetInteger: Longint;
    procedure SetInteger(Value:Longint);
  public
    procedure CreateParams(var Params: TCreateParams); Override;
    procedure Inc;
    procedure Dec;
  published
    property AsInteger : Longint read GetInteger write SetInteger stored false;
    end;

{------------------------------------------------------------------------------}

//procedure Register;

implementation

uses
  ucString;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

constructor TtpIdleTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fEnabled:= True;
  fIdle:= True;
  fInterval:= cIdleTimerInterval;
  fWindowHandle := AllocateHWnd(WndProc);
//  if (csLoading in aOwner.ComponentState) then
//    UpdateTimer;
end;

destructor TtpIdleTimer.Destroy;
begin
  fEnabled:= False;
  UpdateTimer;
  DeallocateHWnd(fWindowHandle);
  inherited Destroy;
end;

procedure TtpIdleTimer.Loaded;
begin
  inherited Loaded;
  UpdateTimer;
end;

procedure TtpIdleTimer.UpdateTimer;
begin
  if (csDesigning in ComponentState) then
    exit; {}
  KillTimer(FWindowHandle, 1);
  if (fInterval <> 0)
  and fEnabled
  and (Assigned(fOnTimer) or fIdle)
  then
    if SetTimer(fWindowHandle, 1, fInterval, nil) = 0 then
{$IFNDEF VER90}
      raise EOutOfResources.Create(SNoTimers);
{$ELSE}
      raise EOutOfResources.Create(LoadStr(SNoTimers));
{$ENDIF}
end;

procedure TtpIdleTimer.SetEnabled(Value: Boolean);
begin
  if Value<>fEnabled then begin
    fEnabled:= Value;
    UpdateTimer;
    end;
end;

procedure TtpIdleTimer.SetInterval(Value: Word);
begin
  if Value<>fInterval then begin
    fInterval:= Value;
    UpdateTimer;
    end;
end;

procedure TtpIdleTimer.SetOnTimer(Value: TNotifyEvent);
begin
  fOnTimer:= Value;
  UpdateTimer;
end;

procedure TtpIdleTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg=WM_TIMER then begin
      Result:=0;
      if not Application.Terminated then try
        if fIdle then
          Idle;
        if Assigned(FOnTimer) then
          Timer;
      except
        Application.HandleException(Self);
         end;
      end
    else
      Result:= DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

procedure TtpIdleTimer.Timer;
begin
  if Assigned(fOnTimer) then
    fOnTimer(Self);
end;

procedure TtpIdleTimer.Idle;
var
  Done:Boolean;
  OnIdle: TIdleEvent;
begin
  Done:=True;
  OnIdle:=Application.OnIdle;
  if assigned(OnIdle) then
    OnIdle(Self,Done);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

procedure TEdRight.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or ES_RIGHT;
end;

{------------------------------------------------------------------------------}

function TEdRight.GetInteger: Longint;
var
  a1,a2:string;
begin
  SplitString(Text,#13#10,a1,a2);
  Result:=StrToIntDef(a1+a2,0);
end;

procedure TEdRight.SetInteger(Value:Longint);
begin
  Text:=IntToStr(Value);
end;

procedure TEdRight.Inc;
var
  a1,a2:string;
begin
  SplitString(Text,#13#10,a1,a2);
  SetInteger(Succ(StrToIntDef(a1+a2,0)));
end;

procedure TEdRight.Dec;
var
  a1,a2:string;
begin
  SplitString(Text,#13#10,a1,a2);
  SetInteger(Pred(StrToIntDef(a1+a2,0)));
end;

procedure TEdRight.WMKeyDown(var Message: TWMKeyDown);
var
  a1,a2:string;
begin
  if Message.CharCode<>vk_RETURN then
    inherited
  else begin
    Message.CharCode:=vk_home;
    Dispatch(Message);
    SplitString(Text,#13#10,a1,a2);
    Text:=a1+a2;
    end;
end;

procedure TEdRight.KeyDown(var Key: Word; Shift: TShiftState);
var
  a1,a2:string;
//    Msg: Cardinal;
//    CharCode: Word;
//    Unused: Word;
//    KeyData: Longint;
//    Result: Longint;
begin
  case Key of
   VK_RETURN: Key:=VK_LEFT;
       VK_UP: begin
         inc;
         Key:=VK_END;
         {Key:=VK_LEFT;}
         end;
     VK_DOWN: begin
         dec;
         Key:=VK_END;
         {Key:=VK_RIGHT}
         end;
    end;
  if Key=VK_RETURN then begin
    end;
  inherited KeyDown(Key,Shift);
  SplitString(Text,#13#10,a1,a2);
  if a2<>'' then
    Text:=a1+a2;
//  if pos(#13,Text)>0 then
//    Text:=LeftOf(#13,text);
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

constructor TtpIdleDot.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  if not (csLoading in aOwner.ComponentState) then begin
    Height:=15;
    Width:=21;
    UniqueName:=Copy(ClassName,2,255);
    Caption:='';
    end
  else begin
    fIdleTime:=cIdleDotIdleTime;
    fOnIdleHook:=Application.OnIdle;
    Application.OnIdle:=DoIdle;
    end;
end;

destructor TtpIdleDot.Destroy;
begin
  if not (csDesigning in ComponentState) then
    Application.OnIdle:=fOnIdleHook;
  inherited Destroy;
end;

procedure TtpIdleDot.DoIdle(Sender:TObject; var Done:Boolean);
var
  i:integer;
begin
  i:=GetTickCount;
  if i<fNextTime then
    exit;
  fNextTime:=i+fIdleTime;
  if Visible then
    Idle;
  if assigned(fOnIdleHook) then
    fOnIdleHook(Sender,Done);
end;

procedure TtpIdleDot.Idle;
{this little change to the canvase will make the idle kick in
 again soon.}
begin
//  if Color>=clWhite then
//    Color:=clBlack
//  else
//    Color:=succ(Color);
  if Color=clBlack then Color:=clMaroon else
  if Color=clMaroon then Color:=clGreen else
  if Color=clGreen then Color:=clOlive else
  if Color=clOlive then Color:=clNavy else
  if Color=clNavy then Color:=clPurple else
  if Color=clPurple then Color:=clTeal else
  if Color=clTeal then Color:=clGray else
  if Color=clGray then Color:=clSilver else
  if Color=clSilver then Color:=clRed else
  if Color=clRed then Color:=clLime else
  if Color=clLime then Color:=clYellow else
  if Color=clYellow then Color:=clBlue else
  if Color=clBlue then Color:=clFuchsia else
  if Color=clFuchsia then Color:=clAqua else
  if Color=clAqua then Color:=clLtGray else
  if Color=clLtGray then Color:=clDkGray else
  if Color=clDkGray then Color:=clWhite else
    Color:=clBlack;
  invalidate;
end;

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

constructor TtpIdlerDot.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  DotMode:=low(TDotModes);
end;

destructor TtpIdlerDot.Destroy;
begin
  fDotImage.Free;
  fDotImage:=nil;
  if fDotBuffer<>nil then
    FreeMem(fDotBuffer,sizeof(TDotBuffer));
  fDotBuffer:=nil;
  inherited Destroy;
end;

procedure TtpIdlerDot.Click;
begin
  inherited Click;
  DotImageClick(Self);
end;

procedure TtpIdlerDot.DotImageClick(Sender: TObject);
begin
  if fDotMode<high(TDotModes) then
    DotMode:=succ(fDotMode)
  else
    DotMode:=low(TDotModes);
end;

procedure TtpIdlerDot.SetDotMode(Value:TDotModes);
begin
  if Value<>fDotMode then begin
    if fDotMode in [DotFancy,DotWild] then
      if assigned(fDotImage) then
        fDotImage.Visible:=False;
    fDotMode:=Value;
    if fDotMode in [DotFancy,DotWild] then begin
      if fDotBuffer=nil then begin
        GetMem(fDotBuffer,sizeof(TDotBuffer));
        fDotImage:=TImage.Create(Self);
        with fDotImage do begin
          Parent:=Self;
          Align:=alClient;
          Align:=alNone;
          Width:=min(cDotBufferSize,Width);
          Height:=min(cDotBufferSize,Height);
          Picture.Bitmap.Width:=Width;
          Picture.Bitmap.Height:=Height;
          OnClick:=DotImageClick;
          end;
        end;
      fDotBase:=random($7fff);
      with fDotImage do begin
        Visible:=True;
        DoFancyDot(0);
        end;
      end;
    end;
end;

procedure TtpIdlerDot.Idle;
begin
  if Visible then
   case fDotMode of
    DotSimple: inherited Idle;
    DotFancy:  DoFancyDot(1);
    DotWild:   DoFancyDot(2);
    end;
end;

procedure TtpIdlerDot.DoFancyDot(Mode:byte);
var
  i,j,x,y:byte;
  n:longint;

  procedure SetDot;
  begin
    n:=i+j;
    fDotBuffer^[pred(i)+pred(j)*x]:=fDotBase +n +n shl 8;
  end;

  function GetDot:Word;
  begin
    Result:=fDotBuffer^[pred(i)+pred(j)*x];
  end;

  procedure IncDot(Value:Word);
  begin
    {$Q-} {local override; this is meant to overflow!}
    inc(fDotBuffer^[pred(i)+pred(j)*x],Value);
  end;

begin
  if fDotImage=nil then
    exit;
  with fDotImage do begin
    x:=min(cDotBufferSize,Width);
    y:=min(cDotBufferSize,Height);
    end;
  for i:=1 to x do
    for j:=1 to y do
      case Mode of
      0: SetDot;
      1: IncDot(4);
      2: IncDot(random(4));
      end;
  with fDotImage do begin
    with fDotImage.Picture.Bitmap do
      SetBitmapBits(Handle,x*y*sizeof(word),fDotBuffer);
    invalidate;
    end;
end;


{------------------------------------------------------------------------------}

//procedure Register;
//begin
//  RegisterComponents('TPACK',[TEdRight,TtpIdleTimer,TtpIdleDot,TtpIdlerDot]);
//end;

{------------------------------------------------------------------------------}
end.

unit shdocvw;

{ Conversion log:
  Property TWebBrowser.Type renamed to Type_: Identifier is a reserved word }

interface

uses Ole2, OleCtl, Classes, Graphics, OleCtrls;

const

{ BrowserNavConstants }

  navOpenInNewWindow = 1;
  navNoHistory = 2;
  navNoReadFromCache = 4;
  navNoWriteToCache = 8;

{ RefreshConstants }

  REFRESH_NORMAL = 0;
  REFRESH_IFEXPIRED = 1;
  REFRESH_COMPLETELY = 3;

{ CommandStateChangeConstants }

  CSC_UPDATECOMMANDS = -1;
  CSC_NAVIGATEFORWARD = 1;
  CSC_NAVIGATEBACK = 2;

type

  TWebBrowserBeforeNavigate = procedure(Sender: TObject; const URL: string; Flags: Integer; const TargetFrameName: string; var PostData: Variant; const Headers: string; var Cancel: TOleBool) of object;
  TWebBrowserNavigateComplete = procedure(Sender: TObject; const URL: string) of object;
  TWebBrowserStatusTextChange = procedure(Sender: TObject; const Text: string) of object;
  TWebBrowserProgressChange = procedure(Sender: TObject; Progress, ProgressMax: Integer) of object;
  TWebBrowserCommandStateChange = procedure(Sender: TObject; Command: Integer; Enable: TOleBool) of object;
  TWebBrowserNewWindow = procedure(Sender: TObject; const URL: string; Flags: Integer; const TargetFrameName: string; var PostData: Variant; const Headers: string; var Processed: TOleBool) of object;
  TWebBrowserTitleChange = procedure(Sender: TObject; const Text: string) of object;
  TWebBrowserFrameBeforeNavigate = procedure(Sender: TObject; const URL: string; Flags: Integer; const TargetFrameName: string; var PostData: Variant; const Headers: string; var Cancel: TOleBool) of object;
  TWebBrowserFrameNavigateComplete = procedure(Sender: TObject; const URL: string) of object;
  TWebBrowserFrameNewWindow = procedure(Sender: TObject; const URL: string; Flags: Integer; const TargetFrameName: string; var PostData: Variant; const Headers: string; var Processed: TOleBool) of object;
  TWebBrowserQuit = procedure(Sender: TObject; var Cancel: TOleBool) of object;
  TWebBrowserPropertyChange = procedure(Sender: TObject; const szProperty: string) of object;

  TWebBrowserOCX = class(TOleControl)
  private
    FOnBeforeNavigate: TWebBrowserBeforeNavigate;
    FOnNavigateComplete: TWebBrowserNavigateComplete;
    FOnStatusTextChange: TWebBrowserStatusTextChange;
    FOnProgressChange: TWebBrowserProgressChange;
    FOnDownloadComplete: TNotifyEvent;
    FOnCommandStateChange: TWebBrowserCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnNewWindow: TWebBrowserNewWindow;
    FOnTitleChange: TWebBrowserTitleChange;
    FOnFrameBeforeNavigate: TWebBrowserFrameBeforeNavigate;
    FOnFrameNavigateComplete: TWebBrowserFrameNavigateComplete;
    FOnFrameNewWindow: TWebBrowserFrameNewWindow;
    FOnQuit: TWebBrowserQuit;
    FOnWindowMove: TNotifyEvent;
    FOnWindowResize: TNotifyEvent;
    FOnWindowActivate: TNotifyEvent;
    FOnPropertyChange: TWebBrowserPropertyChange;
  protected
    procedure InitControlData; override;
  public
    procedure GoBack; stdcall;
    procedure GoForward; stdcall;
    procedure GoHome; stdcall;
    procedure GoSearch; stdcall;
    procedure Navigate(const URL: string; var Flags, TargetFrameName, PostData, Headers: Variant); stdcall;
    procedure Refresh; stdcall;
    procedure Refresh2(var Level: Variant); stdcall;
    procedure Stop; stdcall;
    property Application: Variant index 200 read GetVariantProp;
    property Parent: Variant index 201 read GetVariantProp;
    property Container: Variant index 202 read GetVariantProp;
    property Document: Variant index 203 read GetVariantProp;
    property TopLevelContainer: TOleBool index 204 read GetOleBoolProp;
    property Type_: string index 205 read GetStringProp;
    property LocationName: string index 210 read GetStringProp;
    property LocationURL: string index 211 read GetStringProp;
    property Busy: TOleBool index 212 read GetOleBoolProp;
  published
    property TabStop;
    property DragCursor;
    property DragMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property Left: Integer index 206 read GetIntegerProp write SetIntegerProp stored False;
    property Top: Integer index 207 read GetIntegerProp write SetIntegerProp stored False;
    property Width: Integer index 208 read GetIntegerProp write SetIntegerProp stored False;
    property Height: Integer index 209 read GetIntegerProp write SetIntegerProp stored False;
    property OnBeforeNavigate: TWebBrowserBeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TWebBrowserNavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnStatusTextChange: TWebBrowserStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowserProgressChange read FOnProgressChange write FOnProgressChange;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnCommandStateChange: TWebBrowserCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnNewWindow: TWebBrowserNewWindow read FOnNewWindow write FOnNewWindow;
    property OnTitleChange: TWebBrowserTitleChange read FOnTitleChange write FOnTitleChange;
    property OnFrameBeforeNavigate: TWebBrowserFrameBeforeNavigate read FOnFrameBeforeNavigate write FOnFrameBeforeNavigate;
    property OnFrameNavigateComplete: TWebBrowserFrameNavigateComplete read FOnFrameNavigateComplete write FOnFrameNavigateComplete;
    property OnFrameNewWindow: TWebBrowserFrameNewWindow read FOnFrameNewWindow write FOnFrameNewWindow;
    property OnQuit: TWebBrowserQuit read FOnQuit write FOnQuit;
    property OnWindowMove: TNotifyEvent read FOnWindowMove write FOnWindowMove;
    property OnWindowResize: TNotifyEvent read FOnWindowResize write FOnWindowResize;
    property OnWindowActivate: TNotifyEvent read FOnWindowActivate write FOnWindowActivate;
    property OnPropertyChange: TWebBrowserPropertyChange read FOnPropertyChange write FOnPropertyChange;
  end;

procedure Register;

implementation

{$J+}

procedure TWebBrowserOCX.InitControlData;
const
  CEventDispIDs: array[0..16] of Integer = (
    $00000064, $00000065, $00000066, $0000006C, $00000068, $00000069,
    $0000006A, $0000006B, $00000071, $000000C8, $000000C9, $000000CC,
    $00000067, $0000006D, $0000006E, $0000006F, $00000070);
  CControlData: TControlData = (
    ClassID: (
      D1:$EAB22AC3;D2:$30C1;D3:$11CF;D4:($A7,$EB,$00,$00,$C0,$5B,$AE,$0B));
    EventIID: (
      D1:$EAB22AC2;D2:$30C1;D3:$11CF;D4:($A7,$EB,$00,$00,$C0,$5B,$AE,$0B));
    EventCount: 17;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000);
begin
  ControlData := @CControlData;
end;

procedure TWebBrowserOCX.GoBack;
const
  DispInfo: array[0..7] of Byte = ($64,$00,$00,$00,$00,$01,$00,$00);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure TWebBrowserOCX.GoForward;
const
  DispInfo: array[0..7] of Byte = ($65,$00,$00,$00,$00,$01,$00,$00);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure TWebBrowserOCX.GoHome;
const
  DispInfo: array[0..7] of Byte = ($66,$00,$00,$00,$00,$01,$00,$00);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure TWebBrowserOCX.GoSearch;
const
  DispInfo: array[0..7] of Byte = ($67,$00,$00,$00,$00,$01,$00,$00);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure TWebBrowserOCX.Navigate;
const
  DispInfo: array[0..12] of Byte = ($68,$00,$00,$00,$00,$01,$05,$00,$08,$8C,$8C,$8C,$8C);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure TWebBrowserOCX.Refresh;
const
  DispInfo: array[0..7] of Byte = ($DA,$FD,$FF,$FF,$00,$01,$00,$00);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure TWebBrowserOCX.Refresh2;
const
  DispInfo: array[0..8] of Byte = ($69,$00,$00,$00,$00,$01,$01,$00,$8C);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure TWebBrowserOCX.Stop;
const
  DispInfo: array[0..7] of Byte = ($6A,$00,$00,$00,$00,$01,$00,$00);
begin
  InvokeMethod(DispInfo, nil);
end;

procedure Register;
begin
  RegisterComponents('OCX', [TWebBrowserOCX]);
end;

end.
